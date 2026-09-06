import * as Layer from 'effect/Layer'
import * as Schema from 'effect/Schema'
import { NodeRuntime, NodeServices } from '@effect/platform-node'
import * as Effect from 'effect/Effect'
import * as Config from 'effect/Config'
import * as Data from 'effect/Data'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as Stream from 'effect/Stream'
import * as Console from 'effect/Console'
import * as Result from 'effect/Result'
import { ChildProcess, ChildProcessSpawner } from 'effect/unstable/process'
import * as CompilationProfile from '../../dist/CompilationProfile.js'
import * as NodeHeapObservation from '../../dist/NodeHeapObservation.js'
import * as Driver from '../../dist/Driver.js'
import * as NativeToolchain from '../../dist/NativeToolchain.js'
import * as NativeLinkInput from '../../dist/NativeLinkInput.js'
import * as SourceFile from '../../dist/SourceFile.js'
import * as SourceResolver from '../../dist/SourceResolver.js'

class ConformanceError extends Data.TaggedError('ConformanceError') {}
const program = Effect.gen(function* () {
  const fs = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const spawner = yield* ChildProcessSpawner.ChildProcessSpawner
  const target = yield* Config.string('SILK_SUPPLY_TARGET')
  const clang = yield* Config.string('SILK_SUPPLY_CLANG')
  const llvmAr = yield* Config.string('SILK_SUPPLY_AR')
  const linker = yield* Config.string('SILK_SUPPLY_LINKER')
  const inspect = yield* Config.string('SILK_SUPPLY_READOBJ')
  const root = yield* Config.string('SILK_SUPPLY_ROOT')
  const gcc = yield* Config.string('SILK_SUPPLY_GCC').pipe(Config.withDefault(''))
  const image = yield* Config.string('SILK_SUPPLY_IMAGE').pipe(Config.withDefault(''))
  const output = path.resolve(
    yield* Config.string('SILK_SUPPLY_OUTPUT').pipe(
      Config.withDefault('.scratch/platform-supplies'),
    ),
  )
  yield* fs.makeDirectory(output, { recursive: true })
  const run = Effect.fnUntraced(
    /**
     * @param {string} command
     * @param {ReadonlyArray<string>} args
     * @param {number} expected
     */ function* (command, args, expected = 0) {
      return yield* Effect.scoped(
        Effect.gen(function* () {
          const child = yield* spawner.spawn(
            ChildProcess.make(command, args, { stdin: 'ignore', stdout: 'pipe', stderr: 'pipe' }),
          )
          const [status, stdout, stderr] = yield* Effect.all(
            [
              child.exitCode,
              Stream.mkString(Stream.decodeText(child.stdout)),
              Stream.mkString(Stream.decodeText(child.stderr)),
            ],
            { concurrency: 'unbounded' },
          )
          if (status !== expected)
            return yield* new ConformanceError({
              message: `${command}: expected ${expected}, received ${status}\n${stdout}${stderr}`,
            })
          return { command, arguments: args, status, stdout, stderr }
        }),
      )
    },
  )
  const versions = []
  for (const tool of [clang, llvmAr, linker, inspect]) {
    const result = yield* run(tool, ['--version'])
    if (!(result.stdout + result.stderr).includes('22.1.8'))
      return yield* new ConformanceError({ message: `Required LLVM 22.1.8 tool missing: ${tool}` })
    versions.push(result)
  }
  const invalidLto = yield* Effect.result(CompilationProfile.decode({ target, lto: true }))
  if (!Result.isFailure(invalidLto))
    return yield* new ConformanceError({ message: 'Unverified LTO was accepted' })
  const report = { schema: 1, target, tools: versions, lto: 'rejected', lanes: [] }
  for (const optimization of ['none', 'speed']) {
    const input = {
      target,
      optimization,
      debug: optimization === 'none',
      ...(target.includes('apple') ? { deployment: '11.0.0' } : {}),
    }
    const profile = yield* CompilationProfile.normalize(input)
    const tools = yield* NativeToolchain.resolveToolchain(
      {
        _tag: 'Toolchain',
        clang,
        llvmAr,
        platform: {
          kind: 'explicit',
          target: profile.target.id,
          root,
          linker,
          origin: 'required platform conformance',
          support:
            gcc === ''
              ? []
              : [{ root: gcc, target: profile.target.id, origin: 'pinned GCC12 compiler support' }],
        },
      },
      profile,
    )
    const lane = yield* NativeToolchain.withBuildScope(
      'platform-conformance',
      Effect.fnUntraced(function* (scope) {
        const live = yield* NativeToolchain.compileCObject(
          tools,
          scope,
          profile.target,
          'live',
          'int supply_archive_root(void) { return 0; } int supply_add(int a, int b) { return a + b; }',
        )
        const dead = yield* NativeToolchain.compileCObject(
          tools,
          scope,
          profile.target,
          'dead',
          'extern int undefined_dead_member(void); int supply_unused(void) { return undefined_dead_member(); }',
        )
        const weak = yield* NativeToolchain.compileCObject(
          tools,
          scope,
          profile.target,
          'weak',
          '__attribute__((weak)) int supply_add(int a, int b) { (void)a; (void)b; return 99; }',
        )
        const entry = {
          request: { kind: 'default' },
          composition: { kind: 'default' },
          resolved: { kind: 'default' },
        }
        const archivePath = path.join(scope.root, 'libsupply_fixture.a')
        const archivePlan = yield* NativeToolchain.planNativeLink(
          tools,
          scope,
          'NativeStaticLibrary',
          profile,
          [live.artifact, dead.artifact],
          [],
          archivePath,
          entry,
        )
        yield* NativeToolchain.NativeFinalizer.finalize(
          archivePlan,
          'NativeStaticLibrary',
          archivePath,
        )
        const source = `unsafe extern "C" fn supply_archive_root() -> i32
unsafe extern "C" fn supply_add(a: i32, b: i32) -> i32
pub fn main() -> i32 { let rooted = unsafe supply_archive_root()
 return unsafe supply_add(40, 2) + rooted }`
        const destination = path.join(output, `${target}-${optimization}`)
        const outcome = yield* Driver.compile({
          compilation: {
            root: SourceFile.make('fixture/main', new TextEncoder().encode(source)),
            configuration: { profile: input },
          },
          toolchain: tools,
          artifactKind: 'NativeExecutable',
          packageName: 'supply-conformance',
          destination,
          cache: false,
          nativeLinkInputs: [
            NativeLinkInput.object(weak.artifact.path),
            NativeLinkInput.searchPath(scope.root),
            NativeLinkInput.library(
              'supply_fixture',
              target.includes('apple') ? 'Dynamic' : 'Static',
            ),
          ],
        }).pipe(Effect.provide(SourceResolver.empty))
        if (outcome._tag !== 'Compiled' || outcome.linkPlan === undefined)
          return yield* new ConformanceError({
            message: `Required compilation failed: ${yield* Schema.encodeEffect(Schema.fromJsonString(Schema.Unknown))(outcome).pipe(Effect.orDie)}`,
          })
        const plan = outcome.linkPlan
        const expectedVersion = target.includes('apple') ? '15.5' : '2.36'
        if (plan.supply.version !== expectedVersion)
          return yield* new ConformanceError({
            message: `Expected platform ${expectedVersion}, got ${plan.supply.version}`,
          })
        if (plan.inputs.some((item) => item.digest.length !== 64))
          return yield* new ConformanceError({ message: 'Incomplete physical input digest' })
        const inspection = yield* run(inspect, [
          '--file-header',
          '--program-headers',
          '--needed-libs',
          '--symbols',
          ...(target.includes('linux') ? ['--string-dump=.interp'] : []),
          destination,
        ])
        if (
          target.includes('linux') &&
          (plan.interpreter === undefined || !inspection.stdout.includes(plan.interpreter))
        )
          return yield* new ConformanceError({
            message: 'Interpreter inspection disagrees with the physical plan',
          })
        let execution
        if (target.includes('apple')) execution = yield* run(destination, [], 42)
        else {
          if (image === '')
            return yield* new ConformanceError({
              message: 'Required GNU execution image missing; execution cannot be skipped',
            })
          execution = yield* run(
            'docker',
            [
              'run',
              '--rm',
              '--platform',
              target.startsWith('aarch64') ? 'linux/arm64' : 'linux/amd64',
              '-v',
              `${output}:/fixture:ro`,
              image,
              `/fixture/${path.basename(destination)}`,
            ],
            42,
          )
        }
        // A separately compiled C consumer verifies ordinary library import resolution as well.
        const consumer = yield* NativeToolchain.compileCObject(
          tools,
          scope,
          profile.target,
          'consumer',
          'extern int supply_add(int,int); int main(void) { return supply_add(20,22); }',
        )
        const cDestination = path.join(output, `${target}-${optimization}-c`)
        const cPlan = yield* NativeToolchain.planNativeLink(
          tools,
          scope,
          'NativeExecutable',
          profile,
          [consumer.artifact],
          [NativeLinkInput.staticArchive(archivePath)],
          cDestination,
          entry,
        )
        yield* NativeToolchain.NativeFinalizer.finalize(cPlan, 'NativeExecutable', cDestination)
        const cExecution = target.includes('apple')
          ? yield* run(cDestination, [], 42)
          : yield* run(
              'docker',
              [
                'run',
                '--rm',
                '--platform',
                target.startsWith('aarch64') ? 'linux/arm64' : 'linux/amd64',
                '-v',
                `${output}:/fixture:ro`,
                image,
                `/fixture/${path.basename(cDestination)}`,
              ],
              42,
            )
        const failures = []
        if (optimization === 'none') {
          if (target.includes('linux')) {
            const missingRoot = path.join(scope.root, 'missing-crt-sysroot')
            yield* fs.makeDirectory(missingRoot)
            const incomplete = yield* NativeToolchain.resolveToolchain(
              {
                _tag: 'Toolchain',
                clang,
                llvmAr,
                platform: {
                  kind: 'explicit',
                  target: profile.target.id,
                  root: missingRoot,
                  linker,
                  origin: 'missing CRT fixture',
                },
              },
              profile,
            )
            const rejected = yield* Effect.result(
              NativeToolchain.planNativeLink(
                incomplete,
                scope,
                'NativeExecutable',
                profile,
                [consumer.artifact],
                [],
                path.join(scope.root, 'missing-crt'),
                entry,
              ),
            )
            if (
              !Result.isFailure(rejected) ||
              rejected.failure.reason._tag !== 'SupplyFailed' ||
              !rejected.failure.reason.failure.subject.includes('crt')
            )
              return yield* new ConformanceError({
                message: 'An empty explicit GNU sysroot did not reject missing CRT inputs',
              })
            failures.push({ name: 'missing-crt', reason: rejected.failure.reason })
          }
          const missing = yield* Effect.result(
            NativeToolchain.planNativeLink(
              tools,
              scope,
              'NativeExecutable',
              profile,
              [consumer.artifact],
              [
                target.includes('apple')
                  ? NativeLinkInput.framework('SilkMissingFramework')
                  : NativeLinkInput.library('silk_missing_library', 'Dynamic'),
              ],
              path.join(scope.root, 'missing-library'),
              entry,
            ),
          )
          if (!Result.isFailure(missing) || missing.failure.reason._tag !== 'SupplyFailed')
            return yield* new ConformanceError({ message: 'Missing platform library was accepted' })
          failures.push({
            name: target.includes('apple') ? 'missing-framework' : 'missing-library',
            reason: missing.failure.reason,
          })
          // Ask the actual selected linker to resolve symbols, including its archive rules.
          for (const { name, objects } of [
            { name: 'missing', objects: [consumer.artifact] },
            { name: 'duplicate', objects: [consumer.artifact, live.artifact, live.artifact] },
          ]) {
            const rejectedPlan = yield* NativeToolchain.planNativeLink(
              tools,
              scope,
              'NativeExecutable',
              profile,
              objects,
              [],
              path.join(output, `${target}-${name}`),
              entry,
            )
            const rejected = yield* Effect.result(
              NativeToolchain.NativeFinalizer.finalize(
                rejectedPlan,
                'NativeExecutable',
                path.join(output, `${target}-${name}`),
              ),
            )
            if (
              !Result.isFailure(rejected) ||
              rejected.failure.reason._tag !== 'LinkFailed' ||
              rejected.failure.reason.inputs.length === 0
            )
              return yield* new ConformanceError({
                message: `${name} symbol fixture did not fail with input origins`,
              })
            failures.push({ name, reason: rejected.failure.reason })
          }
        }
        return {
          optimization,
          profile: profile.identity,
          plan,
          archivePlan,
          inspection,
          execution,
          cExecution,
          failures,
        }
      }),
    )
    report.lanes.push(lane)
    yield* Console.log(`${target} ${optimization}: compiled, linked, inspected, Silk/C executed`)
  }
  const reportPath = path.join(output, `${target}.json`)
  yield* fs.writeFileString(
    reportPath,
    (yield* Schema.encodeEffect(Schema.fromJsonString(Schema.Unknown))(report).pipe(Effect.orDie)) +
      '\n',
  )
  yield* Console.log(`Supply evidence: ${reportPath}`)
})
NodeRuntime.runMain(
  program.pipe(Effect.provide(Layer.mergeAll(NodeServices.layer, NodeHeapObservation.layer))),
)
