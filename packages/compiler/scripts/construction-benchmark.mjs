// Opt-in built-code benchmark. The loader instruments only this child process; production
// sources and packages have no timers or benchmark-only public API.
import { createHash } from 'node:crypto'
import { readFileSync, readdirSync, mkdirSync, writeFileSync, existsSync } from 'node:fs'
import { Session } from 'node:inspector/promises'
import { registerHooks } from 'node:module'
import { cpus, release } from 'node:os'
import { resolve } from 'node:path'
import { spawnSync, execFileSync } from 'node:child_process'
import { fileURLToPath } from 'node:url'

const packageRoot = fileURLToPath(new URL('../', import.meta.url))
const repository = resolve(packageRoot, '../..')
const fixtures = resolve(packageRoot, 'benchmarks/construction')
const sha256 = (bytes) => createHash('sha256').update(Uint8Array.from(bytes)).digest('hex')
const arg = (name, fallback) =>
  process.argv.find((v) => v.startsWith(`--${name}=`))?.slice(name.length + 3) ?? fallback
const median = (values) => [...values].sort((a, b) => a - b)[Math.floor(values.length / 2)]
const workload = arg('workload', 'lexer')
const revision = execFileSync('git', ['rev-parse', 'HEAD'], {
  cwd: repository,
  encoding: 'utf8',
}).trim()
const output = resolve(arg('output', '/tmp/silk-construction-benchmark'))
mkdirSync(output, { recursive: true })

if (!process.argv.includes('--sample')) {
  const results = []
  const run = (name, ordinal, timing) => {
    const child = spawnSync(
      process.execPath,
      [
        ...new Set([...process.execArgv, '--expose-gc']),
        import.meta.filename,
        '--sample',
        `--workload=${name}`,
        `--output=${output}`,
        `--ordinal=${ordinal}`,
        ...(timing ? ['--timing'] : []),
        ...(arg('llvm-dist') ? [`--llvm-dist=${arg('llvm-dist')}`] : []),
        ...(arg('llvm-revision') ? [`--llvm-revision=${arg('llvm-revision')}`] : []),
      ],
      { encoding: 'utf8', maxBuffer: 32 * 1024 * 1024 },
    )
    if (child.status !== 0)
      throw new Error(child.stderr || child.stdout || `child exited ${child.status}`)
    return JSON.parse(child.stdout)
  }
  for (const name of ['minimal', 'arithmetic', 'lexer']) {
    const timing = run(name, 'timing', true)
    const memory = []
    for (let ordinal = 0; ordinal < 5; ordinal += 1) {
      const sample = run(name, ordinal, false)
      if (
        sample.bitcodeSha256 !== timing.bitcodeSha256 ||
        sample.inventorySha256 !== timing.inventorySha256
      )
        throw new Error(`${name}: fresh-process output changed`)
      memory.push({
        peakRssKiB: sample.peakRssKiB,
        bitcodeSha256: sample.bitcodeSha256,
        inventorySha256: sample.inventorySha256,
      })
    }
    results.push({
      name,
      medianMs: median(timing.timings.slice(2)),
      medianPeakRssKiB: median(memory.map((s) => s.peakRssKiB)),
      timing,
      memory,
    })
    process.stderr.write(`${name}: ${results.at(-1).medianMs.toFixed(2)} ms\n`)
    writeFileSync(
      resolve(output, 'samples.json'),
      `${JSON.stringify({ schemaVersion: 1, revision, warmups: 2, measuredSamples: 5, processPolicy: 'Timing: one fresh process per workload, two discarded warmups then five emissions of the same MIR/request. Memory: five separate fresh processes, one emission each; RSS includes frontend, construction, verification and encoding, excludes children. No construction-memory attribution.', results }, null, 2)}\n`,
    )
  }
  process.stdout.write(
    `${JSON.stringify(results.map(({ name, medianMs, medianPeakRssKiB }) => ({ name, medianMs, medianPeakRssKiB })))}\n`,
  )
} else {
  let instrumented = false
  const profiling = process.argv.includes('--profile')
  if (profiling) {
    const session = new Session()
    session.connect()
    await session.post('Profiler.enable')
    globalThis[Symbol.for('silk.construction.profiler')] = {
      start: () => session.post('Profiler.start'),
      stop: async () => {
        const { profile } = await session.post('Profiler.stop')
        writeFileSync(resolve(output, `${workload}.cpuprofile`), JSON.stringify(profile))
        session.disconnect()
      },
    }
  }
  registerHooks({
    load(url, context, next) {
      const loaded = next(url, context)
      const llvmDist = arg('llvm-dist')
      const llvmPrefix = new URL('../../llvm/dist/', import.meta.url).href
      if (llvmDist !== undefined && url.startsWith(llvmPrefix))
        return {
          ...loaded,
          source: readFileSync(resolve(llvmDist, url.slice(llvmPrefix.length)), 'utf8'),
        }
      if (url === new URL('../dist/NativeProgram.js', import.meta.url).href) {
        let source = String(loaded.source)
        const start = 'const builder = yield* Builder.make('
        const end = 'const violations = yield* Verify.verify(builder);'
        for (const anchor of [start, end])
          if (source.split(anchor).length !== 2)
            throw new Error(`Construction instrumentation anchor changed: ${anchor}`)
        source = source
          .replace(
            start,
            (profiling
              ? 'yield* Effect.promise(() => globalThis[Symbol.for("silk.construction.profiler")].start()); '
              : '') +
              'const constructionStart = performance.now(); ' +
              start,
          )
          .replace(
            end,
            'performance.measure("silk-construction", { start: constructionStart }); ' +
              (profiling
                ? 'yield* Effect.promise(() => globalThis[Symbol.for("silk.construction.profiler")].stop()); '
                : '') +
              end,
          )
        instrumented = true
        return { ...loaded, source }
      }
      return loaded
    },
  })
  const Effect = await import('effect/Effect')
  const Analysis = await import('../dist/Analysis.js')
  const SourceFile = await import('../dist/SourceFile.js')
  const SourceResolver = await import('../dist/SourceResolver.js')
  const NativeProgram = await import('../dist/NativeProgram.js')
  const MirVerification = await import('../dist/MirVerification.js')
  const sourceRoot = resolve(fixtures, workload, 'src')
  const sources = new Map(
    readdirSync(sourceRoot, { recursive: true })
      .filter((name) => name.endsWith('.silk'))
      .sort()
      .map((name) => [name.slice(0, -5), readFileSync(resolve(sourceRoot, name))]),
  )
  const main = sources.get('main')
  if (main === undefined) throw new Error('Missing frozen main.silk')
  const target = 'aarch64-apple-darwin'
  const snapshot = await Effect.runPromise(
    Analysis.makeRealized({ root: SourceFile.make('main', main), target }).pipe(
      Effect.provide(SourceResolver.memory(sources)),
    ),
  )
  const diagnostics = Analysis.diagnostics(snapshot)
  if (diagnostics.length > 0 || snapshot.mir._tag !== 'Available')
    throw new Error(JSON.stringify(diagnostics))
  const program = snapshot.mir.value
  const mirViolations = MirVerification.verify(program)
  if (mirViolations.length > 0) throw new Error(JSON.stringify(mirViolations))
  const request = {
    mode: 'debug',
    support: false,
    sources: new Map(
      [...snapshot.closure.sources].map(([id, source]) => [id, Uint8Array.from(source.bytes)]),
    ),
  }
  const timings = []
  let artifact
  let bitcodeSha256
  const iterations = process.argv.includes('--timing') ? 7 : 1
  for (let index = 0; index < iterations; index += 1) {
    artifact = undefined
    if (typeof globalThis.gc !== 'function') throw new Error('Run samples with node --expose-gc')
    globalThis.gc()
    artifact = await Effect.runPromise(NativeProgram.emit(program, request))
    const digest = sha256(artifact.bitcode)
    if (bitcodeSha256 !== undefined && bitcodeSha256 !== digest)
      throw new Error('Bitcode changed across warmups/samples')
    bitcodeSha256 = digest
    const timing = performance.getEntriesByName('silk-construction')
    if (!instrumented || timing.length !== index + 1)
      throw new Error('Construction boundary was not measured exactly once per emission')
    timings.push(timing[index].duration)
  }
  if (artifact === undefined) throw new Error('No emitted artifact')
  const symbols = artifact.symbols
  const operations = program.functions.map((fn) => ({
    instance: fn.instance,
    operations: MirVerification.operations(fn).map((op) => op._tag),
  }))
  const inventory = { symbols, operations }
  const inventoryBytes = JSON.stringify(inventory)
  const inventorySha256 = createHash('sha256').update(inventoryBytes).digest('hex')
  writeFileSync(
    resolve(output, `${workload}-inventory.json`),
    `${JSON.stringify(inventory, null, 2)}\n`,
  )
  const llvmRoot = arg('llvm-dist', resolve(packageRoot, '../llvm/dist'))
  const builtIdentity = ['compiler', 'llvm'].map((name) => {
    const root = name === 'compiler' ? resolve(packageRoot, 'dist') : llvmRoot
    const hash = createHash('sha256')
    for (const file of readdirSync(root, { recursive: true })
      .filter((file) => file.endsWith('.js'))
      .sort())
      hash.update(file).update(readFileSync(resolve(root, file)))
    return { name, sha256: hash.digest('hex') }
  })
  const ordinal = arg('ordinal', '0')
  const bitcodePath = resolve(output, `${workload}.bc`)
  if (existsSync(bitcodePath) && !readFileSync(bitcodePath).equals(Buffer.from(artifact.bitcode)))
    throw new Error('Bitcode differs from another sample in this output directory')
  writeFileSync(bitcodePath, artifact.bitcode)
  const record = {
    revision,
    llvmRevision: arg('llvm-revision', revision),
    workload,
    ordinal,
    builtIdentity,
    harnessSha256: sha256(readFileSync(import.meta.filename)),
    builtMode: 'tsc dist ESM',
    node: process.version,
    nodeFlags: process.execArgv,
    nodeOptions: process.env.NODE_OPTIONS ?? '',
    host: {
      architecture: process.arch,
      platform: process.platform,
      release: release(),
      cpu: cpus()[0]?.model,
    },
    target,
    profile: snapshot.profile,
    request: { ...request, sources: undefined },
    cache: 'disabled: direct NativeProgram.emit of freshly realized MIR; no Driver cache',
    sources: [...snapshot.closure.sources]
      .map(([id, source]) => ({ id, sha256: sha256(source.bytes) }))
      .sort((a, b) => a.id.localeCompare(b.id)),
    diagnostics,
    mirViolations,
    verifier: 'Verify.verify returned no violations; NativeProgram.emit fails otherwise',
    realizedSymbolCount: symbols.length,
    mirOperationCount: operations.reduce((n, fn) => n + fn.operations.length, 0),
    inventorySha256,
    timings,
    constructionMs: median(timings.slice(iterations === 7 ? 2 : 0)),
    peakRssKiB: process.resourceUsage().maxRSS,
    bitcodeSha256: sha256(artifact.bitcode),
  }
  process.stdout.write(`${JSON.stringify(record)}\n`)
}
