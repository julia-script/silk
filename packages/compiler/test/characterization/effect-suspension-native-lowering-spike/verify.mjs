import { execFileSync } from 'node:child_process'
import { mkdtempSync, readFileSync, rmSync } from 'node:fs'
import { tmpdir } from 'node:os'
import { dirname, join } from 'node:path'
import { fileURLToPath } from 'node:url'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'

const configured = (name, fallback) =>
  Effect.runSync(Config.string(name).pipe(Config.withDefault(fallback)))

const root = dirname(fileURLToPath(import.meta.url))
const temporary = mkdtempSync(join(tmpdir(), 'silk-effect-native-lowering-spike-'))
const llvmBin = configured('SILK_SPIKE_LLVM_BIN', '/opt/homebrew/opt/llvm/bin')
const clang = configured('SILK_SPIKE_CLANG', join(llvmBin, 'clang'))
const llvmAs = configured('SILK_SPIKE_LLVM_AS', join(llvmBin, 'llvm-as'))
const opt = configured('SILK_SPIKE_OPT', join(llvmBin, 'opt'))

const run = (command, arguments_, options = {}) =>
  execFileSync(command, arguments_, {
    cwd: root,
    encoding: 'utf8',
    stdio: options.capture ? ['ignore', 'pipe', 'pipe'] : 'inherit',
  })

const requireInvariant = (condition, message, diagnostic = '') => {
  if (!condition) {
    throw new Error(`${message}${diagnostic.length === 0 ? '' : `\n${diagnostic}`}`)
  }
}

const extract = (text, scenario) => {
  const match = new RegExp(`BEGIN ${scenario}\\n([\\s\\S]*?)END ${scenario}\\n`).exec(text)
  requireInvariant(match !== null, `missing ${scenario} trace`, text)
  return match[1]
}

const assertTraceShape = (output) => {
  const untaken = extract(output, 'untaken')
  requireInvariant(!untaken.includes('allocate-'), 'untaken branch allocated', untaken)

  for (const scenario of ['success', 'typed-failure']) {
    const trace = extract(output, scenario)
    requireInvariant(
      (trace.match(/allocate-begin:/g) ?? []).length === 2,
      `${scenario} does not have exactly two visible requests`,
      trace,
    )
    for (const boundary of [1, 2]) {
      const loanClose = trace.indexOf(`allocator-loan-close:b${boundary}`)
      const publish = trace.indexOf(`publish:b${boundary}`)
      const reborrow = trace.indexOf(`child-reborrow-begin:b${boundary}`)
      const child = trace.indexOf(`child-start:b${boundary}`)
      requireInvariant(
        loanClose >= 0 && loanClose < publish && publish < reborrow && reborrow < child,
        `${scenario} boundary ${boundary} violated loan-close/publish/reborrow/child order`,
        trace,
      )
    }
    requireInvariant(
      trace.indexOf('source-cleanup:owner=202') < trace.indexOf('reclaim:b2') &&
        trace.indexOf('reclaim:b2') < trace.indexOf('source-cleanup:owner=101') &&
        trace.indexOf('source-cleanup:owner=101') < trace.indexOf('reclaim:b1'),
      `${scenario} violated inner-to-outer cleanup then reclaim order`,
      trace,
    )
  }

  const failure = extract(output, 'typed-failure')
  requireInvariant(failure.includes('OUTCOME 1 731'), 'typed failure payload changed', failure)

  for (const ordinal of [1, 2]) {
    const trace = extract(output, `refuse-${ordinal}`)
    requireInvariant(
      trace.includes(`allocate-refused:b${ordinal}:o${ordinal}`),
      'missing refusal',
      trace,
    )
    requireInvariant(!trace.includes(`publish:b${ordinal}`), 'rejected owner published', trace)
    requireInvariant(!trace.includes(`child-start:b${ordinal}`), 'rejected child started', trace)
    requireInvariant(trace.includes('OUTCOME 2 0'), 'OutOfMemoryError outcome changed', trace)
  }

  for (const checkpoint of [1, 2]) {
    const trace = extract(output, `teardown-${checkpoint}`)
    requireInvariant(!trace.includes('source-cleanup:'), 'teardown reported source cleanup', trace)
    requireInvariant(
      (trace.match(/reclaim:/g) ?? []).length === checkpoint,
      'teardown did not reclaim every private frame exactly once',
      trace,
    )
  }
}

try {
  const schema = JSON.parse(readFileSync(join(root, 'schema.json'), 'utf8'))
  requireInvariant(schema.version === 1, 'frozen schema version drifted')
  requireInvariant(schema.status === 'spike-local-only', 'schema escaped spike-local status')
  requireInvariant(schema.continuations.length === 3, 'fixture must contain three boundary shapes')
  requireInvariant(
    schema.continuations.filter((continuation) => continuation.reached).length === 2,
    'fixture must contain exactly two reached suspension points',
  )
  requireInvariant(
    schema.continuations.filter((continuation) => !continuation.reached).length === 1,
    'fixture must contain exactly one untaken suspension branch',
  )

  const version = run(clang, ['--version'], { capture: true })
  requireInvariant(version.includes('22.1.8'), 'spike requires LLVM clang 22.1.8', version)
  requireInvariant(
    run(llvmAs, ['--version'], { capture: true }).includes('22.1.8'),
    'llvm-as is not 22.1.8',
  )
  requireInvariant(
    run(opt, ['--version'], { capture: true }).includes('22.1.8'),
    'opt is not 22.1.8',
  )

  const direct = join(temporary, 'direct')
  run(clang, [
    '-std=c17',
    '-O2',
    '-Wall',
    '-Wextra',
    '-Werror',
    'harness.c',
    'direct.c',
    '-o',
    direct,
  ])
  const directOutput = run(direct, [], { capture: true })
  assertTraceShape(directOutput)

  const bitcode = join(temporary, 'switched.bc')
  const lowered = join(temporary, 'switched.lowered.ll')
  const switched = join(temporary, 'switched')
  run(llvmAs, ['switched.ll', '-o', bitcode])
  run(opt, ['-passes=coro-early,cgscc(coro-split),coro-cleanup', '-S', bitcode, '-o', lowered])
  run(opt, ['-passes=verify', '-disable-output', lowered])
  const loweredIr = readFileSync(lowered, 'utf8')
  requireInvariant(
    loweredIr.includes('define internal fastcc void @open_one.resume') &&
      loweredIr.includes('define internal fastcc void @open_one.destroy') &&
      loweredIr.includes('define internal fastcc void @open_two.resume') &&
      loweredIr.includes('define internal fastcc void @open_two.destroy'),
    'CoroSplit did not produce switched resume/destroy entries',
    loweredIr,
  )
  for (const boundary of [1, 2]) {
    const request = new RegExp(
      `call ptr @spike_allocate\\(ptr %context, i64 ([1-9][0-9]*), i64 ([1-9][0-9]*), i32 ${boundary}\\)`,
    ).exec(loweredIr)
    requireInvariant(
      request !== null,
      `boundary ${boundary} lacks one complete lowered request`,
      loweredIr,
    )
  }
  requireInvariant(
    (loweredIr.match(/call ptr @spike_allocate\(/g) ?? []).length === 2,
    'lowered switched module contains a hidden or missing allocation call',
    loweredIr,
  )

  run(clang, [
    '-std=c17',
    '-O2',
    '-Wall',
    '-Wextra',
    '-Werror',
    'harness.c',
    lowered,
    '-o',
    switched,
  ])
  const switchedOutput = run(switched, [], { capture: true })
  assertTraceShape(switchedOutput)

  const retconBitcode = join(temporary, 'retcon.bc')
  const retconLowered = join(temporary, 'retcon.lowered.ll')
  const retcon = join(temporary, 'retcon')
  run(llvmAs, ['retcon.ll', '-o', retconBitcode])
  run(opt, [
    '-passes=coro-early,cgscc(coro-split),coro-cleanup',
    '-S',
    retconBitcode,
    '-o',
    retconLowered,
  ])
  run(opt, ['-passes=verify', '-disable-output', retconLowered])
  const retconLoweredIr = readFileSync(retconLowered, 'utf8')
  requireInvariant(
    retconLoweredIr.includes('define internal ptr @open_one.resume.0') &&
      retconLoweredIr.includes('define internal ptr @open_two.resume.0'),
    'CoroSplit did not produce returned-continuation entries',
    retconLoweredIr,
  )
  requireInvariant(
    !retconLoweredIr.includes('call ptr @retcon_fallback_allocate') &&
      !retconLoweredIr.includes('call void @retcon_fallback_deallocate'),
    'returned-continuation lowering exceeded its accepted inline frame',
    retconLoweredIr,
  )
  const retconFrameAccesses = retconLoweredIr
    .split('\n')
    .filter((line) => line.includes('getelementptr') && line.includes('.Frame'))
  requireInvariant(
    retconFrameAccesses.length > 0 &&
      retconFrameAccesses.every((line) => line.includes('ptr %buffer') || line.includes('ptr %0')),
    'returned-continuation frame access escaped the selected inline buffer',
    retconFrameAccesses.join('\n'),
  )
  requireInvariant(
    (retconLoweredIr.match(/call ptr @spike_allocate\(/g) ?? []).length === 1,
    'returned-continuation adapter contains a hidden or missing allocation call',
    retconLoweredIr,
  )

  run(clang, [
    '-std=c17',
    '-O2',
    '-Wall',
    '-Wextra',
    '-Werror',
    'harness.c',
    retconLowered,
    '-o',
    retcon,
  ])
  const retconOutput = run(retcon, [], { capture: true })
  assertTraceShape(retconOutput)

  // Physical frame bytes are intentionally target-private. Normalize only the already-validated
  // complete size/alignment fields while comparing the semantic and ownership event ordering.
  const normalizeTrace = (output, variant) =>
    output
      .replace(`VARIANT ${variant}`, 'VARIANT normalized')
      .replace(/size=[0-9]+:align=[0-9]+/g, 'size=<target>:align=<target>')
  const directTrace = normalizeTrace(directOutput, 'direct-iterative')
  const switchedTrace = normalizeTrace(switchedOutput, 'llvm-switched-resume')
  const retconTrace = normalizeTrace(retconOutput, 'llvm-returned-continuation')
  requireInvariant(
    directTrace === switchedTrace,
    'switched-resume trace differs from the direct reference',
    `DIRECT\n${directTrace}\nSWITCHED\n${switchedTrace}`,
  )
  requireInvariant(
    directTrace === retconTrace,
    'returned-continuation trace differs from the direct reference',
    `DIRECT\n${directTrace}\nRETCON\n${retconTrace}`,
  )

  process.stdout.write(
    `${JSON.stringify(
      {
        status: 'pass',
        llvm: version.split('\n')[0],
        variants: ['direct-iterative', 'llvm-switched-resume', 'llvm-returned-continuation'],
        scenarios: schema.scenarios,
        coroutinePassPipeline: 'coro-early,cgscc(coro-split),coro-cleanup',
        note: 'This semantic verifier does not decide task 1.9; evidence.mjs applies the measured selection rule.',
      },
      null,
      2,
    )}\n`,
  )
} finally {
  rmSync(temporary, { recursive: true, force: true })
}
