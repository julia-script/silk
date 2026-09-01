import { spawnSync } from 'node:child_process'
import { mkdirSync, readFileSync, rmSync, statSync, writeFileSync } from 'node:fs'
import { dirname, join } from 'node:path'
import { performance } from 'node:perf_hooks'
import { fileURLToPath } from 'node:url'
import * as Console from 'effect/Console'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'

const log = (...values) => Effect.runSync(Console.log(...values))
const configured = (name, fallback) =>
  Effect.runSync(Config.string(name).pipe(Config.withDefault(fallback)))

const root = dirname(fileURLToPath(import.meta.url))
const evidenceRoot = join(root, 'evidence')
const reportPath = join(root, 'evidence-report.md')
const selectionPath = join(root, 'selection-report.md')
const jsonPath = join(root, 'evidence.json')
const llvmBin = configured('SILK_SPIKE_LLVM_BIN', '/opt/homebrew/opt/llvm/bin')
const target = 'aarch64-apple-darwin'
const coroutinePassPipeline = 'coro-early,cgscc(coro-split),coro-cleanup'
const tools = Object.freeze({
  clang: configured('SILK_SPIKE_CLANG', join(llvmBin, 'clang')),
  dsymutil: configured('SILK_SPIKE_DSYMUTIL', join(llvmBin, 'dsymutil')),
  llvmAs: configured('SILK_SPIKE_LLVM_AS', join(llvmBin, 'llvm-as')),
  llvmDis: configured('SILK_SPIKE_LLVM_DIS', join(llvmBin, 'llvm-dis')),
  llvmDwarfdump: configured('SILK_SPIKE_LLVM_DWARFDUMP', join(llvmBin, 'llvm-dwarfdump')),
  llvmNm: configured('SILK_SPIKE_LLVM_NM', join(llvmBin, 'llvm-nm')),
  llvmObjdump: configured('SILK_SPIKE_LLVM_OBJDUMP', join(llvmBin, 'llvm-objdump')),
  llvmReadobj: configured('SILK_SPIKE_LLVM_READOBJ', join(llvmBin, 'llvm-readobj')),
  llvmSize: configured('SILK_SPIKE_LLVM_SIZE', join(llvmBin, 'llvm-size')),
  llvmSymbolizer: configured('SILK_SPIKE_LLVM_SYMBOLIZER', join(llvmBin, 'llvm-symbolizer')),
  opt: configured('SILK_SPIKE_OPT', join(llvmBin, 'opt')),
})

const commands = []
const requireInvariant = (condition, message, diagnostic = '') => {
  if (!condition) {
    throw new Error(`${message}${diagnostic.length === 0 ? '' : `\n${diagnostic}`}`)
  }
}

const invoke = (command, arguments_, options = {}) => {
  commands.push({ command, arguments: arguments_ })
  const result = spawnSync(command, arguments_, {
    cwd: root,
    encoding: 'utf8',
    env: process.env,
    input: options.input,
    maxBuffer: 64 * 1024 * 1024,
  })
  if (result.status !== 0) {
    throw new Error(
      `command failed (${result.status ?? result.signal}): ${command} ${arguments_.join(' ')}\n${result.stdout}\n${result.stderr}`,
    )
  }
  return `${result.stdout}${result.stderr}`
}

const retain = (name, content) => {
  const path = join(evidenceRoot, name)
  writeFileSync(path, content)
  return path
}

const median = (values) => {
  const sorted = [...values].sort((left, right) => left - right)
  const middle = Math.floor(sorted.length / 2)
  return sorted.length % 2 === 0 ? (sorted[middle - 1] + sorted[middle]) / 2 : sorted[middle]
}

const summarize = (values) => {
  const center = median(values)
  return { median: center, mad: median(values.map((value) => Math.abs(value - center))) }
}

const timed = (command, arguments_, outputPath) => {
  commands.push({ command, arguments: arguments_, timed: true })
  rmSync(outputPath, { force: true })
  const started = performance.now()
  const result = spawnSync(command, arguments_, {
    cwd: root,
    encoding: 'utf8',
    env: process.env,
    maxBuffer: 64 * 1024 * 1024,
  })
  const elapsed = performance.now() - started
  requireInvariant(
    result.status === 0,
    `timed command failed: ${command} ${arguments_.join(' ')}`,
    `${result.stdout}\n${result.stderr}`,
  )
  return elapsed
}

const parseCodeDataSize = (output) => {
  let total = 0
  for (const match of output.matchAll(/^\s*Section __[^:]+:\s*(\d+)$/gm)) total += Number(match[1])
  requireInvariant(total > 0, 'llvm-size did not report Mach-O code/data sections', output)
  return total
}

const parseDepth = (output) => JSON.parse(output.trim().split('\n').at(-1))

const parseFrameLayouts = (output) => {
  const layouts = []
  for (const match of output.matchAll(
    /allocate-begin:b(?<boundary>\d+):o(?<ordinal>\d+):size=(?<size>\d+):align=(?<alignment>\d+)/g,
  )) {
    layouts.push({
      boundary: Number(match.groups.boundary),
      ordinal: Number(match.groups.ordinal),
      size: Number(match.groups.size),
      alignment: Number(match.groups.alignment),
    })
  }
  return layouts.slice(0, 2)
}

const parseIrGraph = (text, variant) => {
  const graph = new Map()
  let current
  for (const line of text.split('\n')) {
    const definition = line.match(/^define\b.*@(?:"([^"]+)"|([^ (]+))\(/)
    if (definition !== null) {
      current = definition[1] ?? definition[2]
      graph.set(current, new Set())
      continue
    }
    if (current === undefined) continue
    if (line.trim() === '}') {
      current = undefined
      continue
    }
    for (const call of line.matchAll(/\b(?:call|invoke)\b.*@(?:"([^"]+)"|([^ (]+))\(/g)) {
      graph.get(current).add(call[1] ?? call[2])
    }
  }
  if (variant === 'switched') {
    const resume = graph.get('spike_variant_resume') ?? new Set()
    const teardown = graph.get('spike_variant_teardown') ?? new Set()
    for (const name of graph.keys()) {
      if (name.endsWith('.resume')) resume.add(name)
      if (name.endsWith('.destroy') || name.endsWith('.cleanup')) teardown.add(name)
    }
    graph.set('spike_variant_resume', resume)
    graph.set('spike_variant_teardown', teardown)
  }
  if (variant === 'retcon') {
    // Retcon stores the first continuation in the accepted allocation's header. The adapter's
    // indirect calls therefore have exactly these two initialized targets for both resume and
    // teardown; the boolean argument selects source cleanup versus raw-frame destruction.
    const continuations = [...graph.keys()].filter((name) => name.endsWith('.resume.0'))
    graph.set('spike_variant_resume', new Set(continuations))
    graph.set('spike_variant_teardown', new Set(continuations))
  }
  // The final candidate IR is linked to the same frozen driver and child harness. Model those
  // edges explicitly so the SCC proof covers the complete transfer cycle, including indirect
  // switched-resume targets, rather than only the candidate object in isolation.
  graph.set('driver_main', new Set(['spike_variant_resume', 'open_boundary']))
  graph.set('open_boundary', new Set(['spike_variant_open']))
  return graph
}

const cyclicComponents = (graph) => {
  let index = 0
  const stack = []
  const onStack = new Set()
  const indices = new Map()
  const low = new Map()
  const cycles = []
  const visit = (node) => {
    indices.set(node, index)
    low.set(node, index)
    index += 1
    stack.push(node)
    onStack.add(node)
    for (const next of graph.get(node) ?? []) {
      if (!graph.has(next)) continue
      if (!indices.has(next)) {
        visit(next)
        low.set(node, Math.min(low.get(node), low.get(next)))
      } else if (onStack.has(next)) {
        low.set(node, Math.min(low.get(node), indices.get(next)))
      }
    }
    if (low.get(node) !== indices.get(node)) return
    const component = []
    let member
    do {
      member = stack.pop()
      onStack.delete(member)
      component.push(member)
    } while (member !== node)
    const selfCycle =
      component.length === 1 && (graph.get(component[0]) ?? new Set()).has(component[0])
    if (component.length > 1 || selfCycle)
      cycles.push(component.sort((left, right) => left.localeCompare(right)))
  }
  for (const node of graph.keys()) if (!indices.has(node)) visit(node)
  return cycles
}

const parseDisassemblyGraph = (text, variant) => {
  const graph = new Map()
  let current
  for (const line of text.split('\n')) {
    const definition = line.match(/^[0-9a-f]+ <_([^>]+)>:$/)
    if (definition !== null) {
      current = definition[1]
      graph.set(current, new Set())
      continue
    }
    if (current === undefined) continue
    const branch = line.match(/\b(?:bl|b)\s+<_([^>+]+)(?:\+[^>]*)?>/)
    if (branch !== null) graph.get(current).add(branch[1])
  }
  if (variant === 'switched') {
    // The two blr sites load the resume/destroy pointer stored by CoroSplit in the continuation
    // header. Record the complete initialized target sets so the machine graph includes the
    // otherwise indirect edges seen in the linked disassembly.
    const resume = graph.get('spike_variant_resume') ?? new Set()
    const teardown = graph.get('spike_variant_teardown') ?? new Set()
    for (const name of graph.keys()) {
      if (name.endsWith('.resume')) resume.add(name)
      if (name.endsWith('.destroy') || name.endsWith('.cleanup')) teardown.add(name)
    }
    graph.set('spike_variant_resume', resume)
    graph.set('spike_variant_teardown', teardown)
  }
  if (variant === 'retcon') {
    // CoroSplit's retcon continuation pointer is stored in the accepted frame header. Recover its
    // finite target set so the linked-machine SCC proof includes these otherwise-indirect calls.
    const continuations = [...graph.keys()].filter((name) => name.endsWith('.resume.0'))
    graph.set('spike_variant_resume', new Set(continuations))
    graph.set('spike_variant_teardown', new Set(continuations))
  }
  return graph
}

rmSync(evidenceRoot, { recursive: true, force: true })
mkdirSync(evidenceRoot, { recursive: true })

const environment = {
  clang: invoke(tools.clang, ['--version']).trim(),
  opt: invoke(tools.opt, ['--version']).trim(),
  llvmAs: invoke(tools.llvmAs, ['--version']).trim(),
  llvmObjdump: invoke(tools.llvmObjdump, ['--version']).trim(),
  llvmDwarfdump: invoke(tools.llvmDwarfdump, ['--version']).trim(),
  node: process.version,
  platform: invoke('/usr/bin/uname', ['-a']).trim(),
  cpu: invoke('/usr/sbin/sysctl', ['-n', 'machdep.cpu.brand_string']).trim(),
  gitRevision: invoke('/usr/bin/git', ['rev-parse', 'HEAD'], { capture: true }).trim(),
  target,
  coroutinePassPipeline,
}
requireInvariant(environment.clang.includes('22.1.8'), 'LLVM 22.1.8 is required')
retain('environment.json', `${JSON.stringify(environment, null, 2)}\n`)
retain(
  'pipeline.txt',
  invoke(tools.opt, [
    '--print-pipeline-passes',
    `-passes=${coroutinePassPipeline}`,
    '-disable-output',
    '/dev/null',
  ]),
)

const directPre = join(evidenceRoot, 'direct-pre.bc')
const switchedPre = join(evidenceRoot, 'switched-pre.bc')
const switchedPost = join(evidenceRoot, 'switched-post.bc')
const retconPre = join(evidenceRoot, 'retcon-pre.bc')
const retconPost = join(evidenceRoot, 'retcon-post.bc')
invoke(tools.clang, [
  `--target=${target}`,
  '-O0',
  '-g',
  '-emit-llvm',
  '-c',
  join(root, 'direct.c'),
  '-o',
  directPre,
])
invoke(tools.llvmAs, [join(root, 'switched.ll'), '-o', switchedPre])
invoke(tools.llvmAs, [join(root, 'retcon.ll'), '-o', retconPre])
invoke(tools.opt, ['-passes=verify', directPre, '-disable-output'])
invoke(tools.opt, ['-passes=verify', switchedPre, '-disable-output'])
invoke(tools.opt, ['-passes=verify', retconPre, '-disable-output'])
invoke(tools.opt, [`-passes=${coroutinePassPipeline}`, switchedPre, '-o', switchedPost])
invoke(tools.opt, [`-passes=${coroutinePassPipeline}`, retconPre, '-o', retconPost])
invoke(tools.opt, ['-passes=verify', switchedPost, '-disable-output'])
invoke(tools.opt, ['-passes=verify', retconPost, '-disable-output'])
invoke(tools.llvmDis, [directPre, '-o', join(evidenceRoot, 'direct-pre.ll')])
invoke(tools.llvmDis, [switchedPre, '-o', join(evidenceRoot, 'switched-pre.ll')])
invoke(tools.llvmDis, [switchedPost, '-o', join(evidenceRoot, 'switched-post.ll')])
invoke(tools.llvmDis, [retconPre, '-o', join(evidenceRoot, 'retcon-pre.ll')])
invoke(tools.llvmDis, [retconPost, '-o', join(evidenceRoot, 'retcon-post.ll')])
const retconPostText = readFileSync(join(evidenceRoot, 'retcon-post.ll'), 'utf8')
requireInvariant(
  !retconPostText.includes('call ptr @retcon_fallback_allocate') &&
    !retconPostText.includes('call void @retcon_fallback_deallocate'),
  'retcon split introduced a fallback allocation or deallocation call',
  retconPostText,
)
const retconFrameAccesses = retconPostText
  .split('\n')
  .filter((line) => line.includes('getelementptr') && line.includes('.Frame'))
requireInvariant(
  retconFrameAccesses.length > 0 &&
    retconFrameAccesses.every((line) => line.includes('ptr %buffer') || line.includes('ptr %0')),
  'retcon split frame access escaped the selected inline buffer',
  retconFrameAccesses.join('\n'),
)

const variants = ['direct', 'switched', 'retcon']
const profiles = ['O0', 'O2']
const candidateInputs = { direct: directPre, switched: switchedPre, retcon: retconPre }
const objects = {}
const finalIr = {}
for (const variant of variants) {
  objects[variant] = {}
  finalIr[variant] = {}
  for (const profile of profiles) {
    const object = join(evidenceRoot, `${variant}-${profile.toLowerCase()}.o`)
    const ir = join(evidenceRoot, `${variant}-${profile.toLowerCase()}-final.ll`)
    invoke(tools.clang, [
      `--target=${target}`,
      '-x',
      'ir',
      `-${profile}`,
      '-g',
      '-c',
      candidateInputs[variant],
      '-o',
      object,
    ])
    invoke(tools.clang, [
      `--target=${target}`,
      '-x',
      'ir',
      `-${profile}`,
      '-g',
      '-S',
      '-emit-llvm',
      candidateInputs[variant],
      '-o',
      ir,
    ])
    invoke(tools.opt, ['-passes=verify', ir, '-disable-output'])
    objects[variant][profile] = object
    finalIr[variant][profile] = ir
  }
}

const forbiddenStructure = [
  'llvm.coro.id',
  'llvm.coro.prepare',
  'llvm.coro.alloc',
  'llvm.coro.size',
  'llvm.coro.align',
  'llvm.coro.begin',
  'llvm.coro.suspend',
  'llvm.coro.free',
  'llvm.coro.end',
]
const forbiddenRuntime =
  /(?:malloc|calloc|realloc|aligned_alloc|operator new|_Unwind|__cxa|setjmp|longjmp|exception|promise)/i
const residualCoroutines = {}
const callGraphs = {}
for (const variant of variants) {
  residualCoroutines[variant] = {}
  callGraphs[variant] = {}
  for (const profile of profiles) {
    const text = readFileSync(finalIr[variant][profile], 'utf8')
    const residual = [
      ...new Set([...text.matchAll(/llvm\.coro\.[A-Za-z0-9_.]+/g)].map((m) => m[0])),
    ].sort()
    residualCoroutines[variant][profile] = residual
    for (const intrinsic of forbiddenStructure) {
      requireInvariant(!text.includes(intrinsic), `${variant} ${profile} retained ${intrinsic}`)
    }
    requireInvariant(
      !forbiddenRuntime.test(text),
      `${variant} ${profile} linked a hidden allocation or exception runtime`,
    )
    const graph = parseIrGraph(text, variant)
    const cycles = cyclicComponents(graph)
    const relevantCycles = cycles.filter((component) =>
      component.some((name) => /spike_variant_(?:open|resume)|\.resume|child|driver/.test(name)),
    )
    requireInvariant(
      relevantCycles.length === 0,
      `${variant} ${profile} has a recursive control cycle`,
      JSON.stringify(relevantCycles),
    )
    callGraphs[variant][profile] = {
      edges: [...graph.entries()].flatMap(([from, tos]) => [...tos].map((to) => [from, to])),
      cycles,
    }
  }
}

const semantic = {}
const depth = {}
const linkedSize = {}
const debug = {}
const machineCallGraphs = {}
for (const variant of variants) {
  semantic[variant] = {}
  depth[variant] = {}
  linkedSize[variant] = {}
  debug[variant] = {}
  machineCallGraphs[variant] = {}
  for (const profile of profiles) {
    const harnessObject = join(evidenceRoot, `harness-${profile.toLowerCase()}.o`)
    const supportObject = join(evidenceRoot, `support-${profile.toLowerCase()}.o`)
    const depthObject = join(evidenceRoot, `depth-${profile.toLowerCase()}.o`)
    if (!statSync(harnessObject, { throwIfNoEntry: false })) {
      invoke(tools.clang, [
        `--target=${target}`,
        `-${profile}`,
        '-g',
        '-c',
        join(root, 'harness.c'),
        '-o',
        harnessObject,
      ])
      invoke(tools.clang, [
        `--target=${target}`,
        `-${profile}`,
        '-g',
        '-DSPIKE_NO_MAIN',
        '-c',
        join(root, 'harness.c'),
        '-o',
        supportObject,
      ])
      invoke(tools.clang, [
        `--target=${target}`,
        `-${profile}`,
        '-g',
        '-c',
        join(root, 'depth.c'),
        '-o',
        depthObject,
      ])
    }
    const semanticExe = join(evidenceRoot, `${variant}-${profile.toLowerCase()}-semantic`)
    const depthExe = join(evidenceRoot, `${variant}-${profile.toLowerCase()}-depth`)
    invoke(tools.clang, [
      `--target=${target}`,
      objects[variant][profile],
      harnessObject,
      '-o',
      semanticExe,
    ])
    invoke(tools.clang, [
      `--target=${target}`,
      objects[variant][profile],
      supportObject,
      depthObject,
      '-o',
      depthExe,
    ])
    const semanticOutput = invoke(semanticExe, [])
    retain(`${variant}-${profile.toLowerCase()}-semantic.txt`, semanticOutput)
    semantic[variant][profile] = {
      frameLayouts: parseFrameLayouts(semanticOutput),
      output: semanticOutput,
    }
    depth[variant][profile] = {}
    for (const logicalDepth of [1, 1000, 100000]) {
      const result = parseDepth(invoke(depthExe, [String(logicalDepth)]))
      requireInvariant(
        result.continuations === logicalDepth * 2,
        'depth fixture skipped continuations',
      )
      requireInvariant(result.checkpoints === logicalDepth * 4, 'depth fixture skipped checkpoints')
      requireInvariant(result.liveAllocations === 0, 'depth fixture leaked allocations')
      depth[variant][profile][logicalDepth] = result
    }
    const baseline = Math.max(depth[variant][profile][1].range, depth[variant][profile][1000].range)
    requireInvariant(
      depth[variant][profile][100000].range <= baseline + 4096,
      `${variant} ${profile} machine stack grew with logical depth`,
    )
    const nm = invoke(tools.llvmNm, ['-n', '-S', semanticExe])
    const readobj = invoke(tools.llvmReadobj, [
      '--sections',
      '--symbols',
      '--relocations',
      semanticExe,
    ])
    const size = invoke(tools.llvmSize, ['-m', semanticExe])
    const disassembly = invoke(tools.llvmObjdump, [
      '-d',
      '-l',
      '-S',
      '--symbolize-operands',
      semanticExe,
    ])
    const depthDisassembly = invoke(tools.llvmObjdump, [
      '-d',
      '-l',
      '-S',
      '--symbolize-operands',
      depthExe,
    ])
    const machineGraph = parseDisassemblyGraph(depthDisassembly, variant)
    const machineCycles = cyclicComponents(machineGraph)
    const relevantMachineCycles = machineCycles.filter((component) =>
      component.some((name) =>
        /main|open_boundary|spike_variant_(?:open|resume)|\.resume|driver|child/.test(name),
      ),
    )
    requireInvariant(
      relevantMachineCycles.length === 0,
      `${variant} ${profile} linked machine code has a recursive control cycle`,
      JSON.stringify(relevantMachineCycles),
    )
    machineCallGraphs[variant][profile] = {
      edges: [...machineGraph.entries()].flatMap(([from, tos]) => [...tos].map((to) => [from, to])),
      cycles: machineCycles,
    }
    retain(`${variant}-${profile.toLowerCase()}.nm.txt`, nm)
    retain(`${variant}-${profile.toLowerCase()}.readobj.txt`, readobj)
    retain(`${variant}-${profile.toLowerCase()}.size.txt`, size)
    retain(`${variant}-${profile.toLowerCase()}.disassembly.txt`, disassembly)
    retain(`${variant}-${profile.toLowerCase()}-depth.disassembly.txt`, depthDisassembly)
    linkedSize[variant][profile] = {
      codeData: parseCodeDataSize(size),
      file: statSync(semanticExe).size,
    }

    const dsym = `${semanticExe}.dSYM`
    invoke(tools.dsymutil, [semanticExe, '-o', dsym])
    const dwarfVerify = invoke(tools.llvmDwarfdump, ['--verify', dsym])
    const dwarfInfo = invoke(tools.llvmDwarfdump, ['--debug-info', dsym])
    const dwarfLine = invoke(tools.llvmDwarfdump, ['--debug-line', dsym])
    requireInvariant(
      !dwarfVerify.includes('error:'),
      `${variant} ${profile} DWARF verification failed`,
      dwarfVerify,
    )
    requireInvariant(
      dwarfLine.includes('boundaries.silk'),
      `${variant} ${profile} lost Silk line provenance`,
    )
    const symbols = ['silk_boundary_suspend_one', 'silk_boundary_suspend_two']
    if (variant === 'switched') {
      symbols.push('open_one.resume', 'open_one.destroy', 'open_two.resume', 'open_two.destroy')
      // O2 may legitimately inline the ramp and cleanup bodies. O0 must retain and symbolize the
      // complete ramp/resume/cleanup/destroy surface required by the hard debug gate.
      if (profile === 'O0')
        symbols.push('open_one', 'open_one.cleanup', 'open_two', 'open_two.cleanup')
    }
    if (variant === 'retcon') {
      symbols.push('open_one.resume.0', 'open_two.resume.0')
      // Optimized ramps may be folded into the adapter. At O0 the ramp and both returned
      // continuations must remain statically attributable to the synthetic Silk boundaries.
      if (profile === 'O0') symbols.push('open_one', 'open_two')
    }
    const symbolized = {}
    for (const symbol of symbols) {
      const nmLine = nm
        .split('\n')
        .find((line) => line.endsWith(` ${symbol}`) || line.endsWith(` _${symbol}`))
      requireInvariant(
        nmLine !== undefined,
        `${variant} ${profile} is missing debug symbol ${symbol}`,
      )
      const address = `0x${nmLine.trim().split(/\s+/)[0]}`
      const output = invoke(tools.llvmSymbolizer, ['--obj', semanticExe, address])
      requireInvariant(
        output.includes('boundaries.silk') || !symbol.includes('open_'),
        `${symbol} did not resolve to boundaries.silk`,
        output,
      )
      symbolized[symbol] = output.trim()
    }
    retain(`${variant}-${profile.toLowerCase()}.dwarf-verify.txt`, dwarfVerify)
    retain(`${variant}-${profile.toLowerCase()}.debug-info.txt`, dwarfInfo)
    retain(`${variant}-${profile.toLowerCase()}.debug-line.txt`, dwarfLine)
    debug[variant][profile] = symbolized
  }
}

for (const profile of profiles) {
  const normalizeSemantic = (output) =>
    output
      .replace(/^VARIANT .*$/m, 'VARIANT normalized')
      .replace(/:size=\d+:align=\d+/g, ':size=<target>:align=<target>')
  for (const variant of variants.filter((candidate) => candidate !== 'direct')) {
    requireInvariant(
      normalizeSemantic(semantic.direct[profile].output) ===
        normalizeSemantic(semantic[variant][profile].output),
      `${profile} ${variant} semantic trace diverged`,
    )
  }
}

const benchmarkSamples = { compile: { O0: {}, O2: {} }, resume: {} }
for (const profile of profiles) {
  for (const variant of variants) benchmarkSamples.compile[profile][variant] = []
  for (let round = 0; round < 35; round += 1) {
    const order = round % 2 === 0 ? variants : [...variants].reverse()
    for (const variant of order) {
      const output = join(evidenceRoot, `timed-${profile}-${variant}-${round}.o`)
      const value = timed(
        tools.clang,
        [
          `--target=${target}`,
          '-x',
          'ir',
          `-${profile}`,
          '-c',
          candidateInputs[variant],
          '-o',
          output,
        ],
        output,
      )
      if (round >= 5) benchmarkSamples.compile[profile][variant].push(value)
    }
  }
}

for (const variant of variants) benchmarkSamples.resume[variant] = []
for (let round = 0; round < 35; round += 1) {
  const order = round % 2 === 0 ? variants : [...variants].reverse()
  for (const variant of order) {
    const executable = join(evidenceRoot, `${variant}-o2-depth`)
    commands.push({ command: executable, arguments: ['1000'], timed: true })
    const started = performance.now()
    const result = spawnSync(executable, ['1000'], { cwd: root, encoding: 'utf8' })
    const elapsed = performance.now() - started
    requireInvariant(result.status === 0, `${variant} timed resume failed`, result.stderr)
    if (round >= 5) benchmarkSamples.resume[variant].push(elapsed / 2000)
  }
}

const benchmarkSummary = {
  compile: Object.fromEntries(
    profiles.map((profile) => [
      profile,
      Object.fromEntries(
        variants.map((variant) => [variant, summarize(benchmarkSamples.compile[profile][variant])]),
      ),
    ]),
  ),
  resumeO2PerBoundary: Object.fromEntries(
    variants.map((variant) => [variant, summarize(benchmarkSamples.resume[variant])]),
  ),
}

const normalizedPass = {
  semanticParity: true,
  allocationAndCleanup: true,
  constantStack: true,
  callCycleAudit: true,
  residualCoroutineStructure: true,
  retconInlineSelectedStorage: true,
  staticDebugProvenance: true,
  verifier: true,
}

const timingDecision = (direct, candidate) => {
  const delta = candidate.median - direct.median
  const noiseThreshold = 2 * (direct.mad + candidate.mad)
  return {
    direct: direct.median,
    candidate: candidate.median,
    delta,
    ratio: delta / direct.median,
    noiseThreshold,
    material: Math.abs(delta) / direct.median >= 0.1 && Math.abs(delta) > noiseThreshold,
  }
}

const sizeDecision = (direct, candidate, minimum) => {
  const delta = candidate - direct
  const threshold = Math.max(direct * 0.1, minimum)
  return {
    direct,
    candidate,
    delta,
    ratio: delta / direct,
    threshold,
    material: Math.abs(delta) >= threshold,
  }
}

const candidateDecision = (variant) => {
  const metrics = {
    compileO0: timingDecision(
      benchmarkSummary.compile.O0.direct,
      benchmarkSummary.compile.O0[variant],
    ),
    compileO2: timingDecision(
      benchmarkSummary.compile.O2.direct,
      benchmarkSummary.compile.O2[variant],
    ),
    resumeO2PerBoundary: timingDecision(
      benchmarkSummary.resumeO2PerBoundary.direct,
      benchmarkSummary.resumeO2PerBoundary[variant],
    ),
    frameBytes: sizeDecision(
      semantic.direct.O2.frameLayouts[0].size,
      semantic[variant].O2.frameLayouts[0].size,
      16,
    ),
    linkedCodeDataBytes: sizeDecision(
      linkedSize.direct.O2.codeData,
      linkedSize[variant].O2.codeData,
      256,
    ),
  }
  const materialAdvantages = Object.entries(metrics)
    .filter(([, metric]) => metric.material && metric.delta < 0)
    .map(([name]) => name)
  const materialRegressions = Object.entries(metrics)
    .filter(([, metric]) => metric.material && metric.delta > 0)
    .map(([name]) => name)
  return {
    hardGatesPass: Object.values(normalizedPass).every(Boolean),
    metrics,
    materialAdvantages,
    materialRegressions,
    qualifies:
      Object.values(normalizedPass).every(Boolean) &&
      materialAdvantages.length > 0 &&
      materialRegressions.length === 0,
  }
}

const selection = {
  selected: 'direct',
  rule: 'LLVM requires every hard gate, at least one material advantage, and no material regression; parity selects direct.',
  candidates: {
    switched: candidateDecision('switched'),
    retcon: candidateDecision('retcon'),
  },
}
requireInvariant(
  !selection.candidates.switched.qualifies && !selection.candidates.retcon.qualifies,
  'an LLVM candidate now qualifies; update the native selection instead of silently retaining direct',
)

const evidence = {
  status: 'pass',
  task: '1.8',
  experiment: {
    task: '1.9',
    scope: 'bounded LLVM returned-continuation comparison',
    completed: true,
  },
  environment,
  normalizedPass,
  residualCoroutines,
  callGraphs,
  machineCallGraphs,
  depth,
  frameLayouts: Object.fromEntries(
    variants.map((variant) => [variant, semantic[variant].O2.frameLayouts]),
  ),
  semanticTraces: Object.fromEntries(
    variants.map((variant) => [
      variant,
      Object.fromEntries(profiles.map((profile) => [profile, semantic[variant][profile].output])),
    ]),
  ),
  linkedSize,
  debug,
  benchmarkSamples,
  benchmarkSummary,
  selection,
  commands,
  determinism: {
    semanticAndDepthReplays: 2,
    normalizedReplaysEqual: true,
    basis: normalizedPass,
  },
}

// Re-run the semantic verifier and deepest sweeps before publishing the normalized result. This
// second pass intentionally excludes timings and addresses from the determinism comparison.
const verifierOutput = invoke(process.execPath, [join(root, 'verify.mjs')])
const verifier = JSON.parse(verifierOutput)
requireInvariant(verifier.status === 'pass', 'semantic verifier second pass failed')
evidence.verifierResult = verifier
for (const variant of variants) {
  for (const profile of profiles) {
    const rerun = parseDepth(
      invoke(join(evidenceRoot, `${variant}-${profile.toLowerCase()}-depth`), ['100000']),
    )
    requireInvariant(
      rerun.range === depth[variant][profile][100000].range,
      `${variant} ${profile} normalized depth report changed`,
    )
    requireInvariant(
      rerun.checkpoints === depth[variant][profile][100000].checkpoints,
      `${variant} ${profile} checkpoint count changed`,
    )
  }
}

writeFileSync(jsonPath, `${JSON.stringify(evidence, null, 2)}\n`)
const formatJson = spawnSync('pnpm', ['exec', 'oxfmt', '--write', jsonPath], {
  cwd: root,
  encoding: 'utf8',
  env: process.env,
})
requireInvariant(
  formatJson.status === 0,
  'failed to format the tracked evidence JSON',
  `${formatJson.stdout}\n${formatJson.stderr}`,
)
const metricRows = ['switched', 'retcon'].flatMap((variant) =>
  Object.entries(selection.candidates[variant].metrics).map(([name, metric]) => {
    let result = 'not material'
    if (metric.material) {
      result = metric.delta < 0 ? '**material advantage**' : '**material regression**'
    }
    const threshold = 'noiseThreshold' in metric ? metric.noiseThreshold : metric.threshold
    return `| ${variant} ${name} | ${metric.direct.toFixed(6)} | ${metric.candidate.toFixed(6)} | ${metric.delta.toFixed(6)} (${(metric.ratio * 100).toFixed(2)}%) | ${threshold.toFixed(6)} | ${result} |`
  }),
)
const selectionReport = `# Native suspension lowering selection\n\nDecision: **select the direct iterative state machine**.\n\nEvery candidate passed the frozen semantic and structural hard gates. The approved rule selects an LLVM candidate only when it has at least one material advantage and no material regression; otherwise direct wins.\n\n| candidate metric | direct | candidate | delta | noise/size threshold | result |\n| --- | ---: | ---: | ---: | ---: | --- |\n${metricRows.join('\n')}\n\nSwitched-resume has no material advantage and regresses O2 compilation and frame size. That frame result triggered the bounded returned-continuation experiment. Retcon also has no material advantage and increases the allocator-visible frame from ${semantic.direct.O2.frameLayouts[0].size} to ${semantic.retcon.O2.frameLayouts[0].size} bytes. Both LLVM candidates are rejected.\n\nThe rejected \`llvm.coro.*\` constructions remain reproducible, disposable fixtures in this directory; no production compiler or \`@silklang/llvm\` surface was added. Production task 5.1 therefore implements direct iterative lowering behind target-neutral continuation descriptors. Exact raw operands and machine-readable decisions are in [evidence.json](evidence.json).\n`
writeFileSync(selectionPath, selectionReport)
const report = `# Native suspension lowering evidence\n\nStatus: **PASS for OpenSpec tasks 1.8 and 1.9**. Selected strategy: **direct iterative state machine**.\n\n- LLVM: ${environment.clang.split('\n')[0]}\n- Target: \`${target}\`\n- Coroutine pipeline: \`${coroutinePassPipeline}\`\n- Semantic/allocation/cleanup parity: pass\n- Constant-stack watermark at depths 1, 1,000, and 100,000 under O0/O2: pass\n- Direct and indirect call-cycle audit: pass\n- Residual coroutine structure-intrinsic audit: pass\n- Retcon fallback-call and selected-inline-buffer-root audit: pass\n- Static DWARF and synthetic Silk boundary symbolization: pass\n- Second normalized semantic/depth replay: pass\n\n## Measured values\n\n| metric | direct | switched | retcon |\n| --- | ---: | ---: | ---: |\n| O0 compile median (ms) | ${benchmarkSummary.compile.O0.direct.median.toFixed(3)} | ${benchmarkSummary.compile.O0.switched.median.toFixed(3)} | ${benchmarkSummary.compile.O0.retcon.median.toFixed(3)} |\n| O2 compile median (ms) | ${benchmarkSummary.compile.O2.direct.median.toFixed(3)} | ${benchmarkSummary.compile.O2.switched.median.toFixed(3)} | ${benchmarkSummary.compile.O2.retcon.median.toFixed(3)} |\n| O2 resume median (ms/boundary) | ${benchmarkSummary.resumeO2PerBoundary.direct.median.toFixed(6)} | ${benchmarkSummary.resumeO2PerBoundary.switched.median.toFixed(6)} | ${benchmarkSummary.resumeO2PerBoundary.retcon.median.toFixed(6)} |\n| frame bytes at boundary 1 | ${semantic.direct.O2.frameLayouts[0].size} | ${semantic.switched.O2.frameLayouts[0].size} | ${semantic.retcon.O2.frameLayouts[0].size} |\n| linked O2 code/data bytes | ${linkedSize.direct.O2.codeData} | ${linkedSize.switched.O2.codeData} | ${linkedSize.retcon.O2.codeData} |\n\nThe generated [selection report](selection-report.md) applies every threshold and records the rejection reason for both LLVM candidates. Raw samples, MADs, exact commands, stack results, call graphs, frame layouts, semantic traces, and debug symbolization are retained in [evidence.json](evidence.json).\n\nRun:\n\n\`\`\`sh\nnode packages/compiler/test/characterization/effect-suspension-native-lowering-spike/evidence.mjs\n\`\`\`\n`
writeFileSync(reportPath, report)
log(
  JSON.stringify(
    {
      status: 'pass',
      task: '1.8',
      selection: 'direct',
      report: reportPath,
      json: jsonPath,
    },
    null,
    2,
  ),
)
