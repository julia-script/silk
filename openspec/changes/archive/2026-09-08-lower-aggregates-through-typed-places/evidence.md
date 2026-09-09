# Aggregate expansion census — 2026-09-08

Benchmark workloads, harnesses, and raw reports are retained locally, outside this pull request.
Benchmark paths and commands below record the historical experiment; they require those local
artifacts and are not runnable from this repository alone.

## Finding

The strongest measured target is the ownership-threaded parser result, not stdin or
a JavaScript-specific primitive.

| Pre-LLVM measurement                                  | Parser only | Full CLI |
| ----------------------------------------------------- | ----------: | -------: |
| Total instructions                                    |     705,913 |  802,864 |
| Loads                                                 |     222,284 |  246,523 |
| Loads with no SSA consumers                           |      90,378 |  100,437 |
| Loads attributed to `ElementsResult` roots            |     122,408 |  122,408 |
| Those loads with no SSA consumers                     |      69,952 |   69,952 |
| `ElementsResult` lane stores through observed helpers |     116,636 |  116,636 |

`ElementsResult` contributes 49.7% of CLI loads and 69.6% of its unused loads.
It contributes 55.1% of parser-only loads. These percentages describe generated code,
not the percentage of compilation time recoverable by changing it.

The source definition in `compiler/src/parser/ParseState.silk:46` contains four owning
containers plus cursor/depth fields: State is 42 calling lanes. ElementsResult at line 67
adds a 10-lane element Vector, making 52 lanes; NodeResult at line 80 adds one NodeId,
making 43. The parser intentionally transfers state ownership between grammar operations.
No source restructuring is required to demonstrate the compiler problem.

Across the CLI, ElementsResult receives 1,698 join refreshes and 656 alias refreshes:
2,354 whole-root events × 52 lanes = 122,408 actual LLVM loads. Its observed stores are
1,729 mutable-store events and 514 address-store events × 52 = 116,636 stores.
These are static compiler emission events, not counts of runtime loop iterations.

The largest single root is local 8 in the specialized `parser/Type.arguments` Effect
runner: 109 refreshes produce 5,668 loads, of which 4,526 have no consumers. Its 20
address materializations plus 9 mutable writes produce 1,508 lane stores. The containing
function has 24,506 LLVM instructions. The full report retains its MIR operation mix;
we do not attribute all of those instructions to that root or divide unlike instruction
sets into a claimed optimization factor.

Other observed root groups include State (42 lanes; 13,902 loads), NodeResult (43 lanes;
3,010 loads), and Bytes (10 lanes; 3,990 loads). A 53-lane EffectValue group returning
ElementsResult has 6,943 root loads, all unused. Effect groups use semantic spelling,
MIR kind, and width; they are not proof that the concrete captured environments are identical.

## Interpretation and scope

A typed place keeps aggregate bytes authoritative and loads a field when requested.
It removes the need to recreate a complete SSA lane cache at each join or possible
alias write. Destination construction and explicit transfers also give us a route
away from lane-by-lane stores when a real aggregate move/copy is needed.

This is not proof that every listed load or store can disappear: real reads, value-copy
semantics, ABI materialization, and union view conversion remain necessary. Unused-load
detection alone is not a safe-deletion analysis; effects, volatile/atomic semantics,
and control-flow validity still matter.

Keep the existing planned calling convention for this change. A later ABI proposal
can assess indirect aggregate arguments/results using residual boundary measurements.
Global liveness/use-def indexing and JS-specific tuning remain separate work.

## Method, provenance, and limitations

[evidence.json](evidence.json) retains both stage summaries, opcode totals, the top
type/root/function groups, source hashes, and compiler/toolchain/hook identities.
Function display names are shortened before the encoded signature; SHA-256 of the full
symbol distinguishes specializations. Type spelling may be truncated at 1,000 characters;
the raw census also stores its semantic-spelling hash.

The checkout is `815e0986b91d0985b4d8400d98cad65024becff9` plus the existing WIP.
Compiler diff, CLI bundle, toolchain identity, generated input, and every workload source
hash match `benchmarks/selfhost-stages/results/2026-09-08-reference-intervals.json`.
That separate uninstrumented batch remains the timing baseline: 33.69s parser and
44.69s CLI medians. No new timing result or speedup is claimed here.

One fresh process/output build per stage used an external Node load hook against the
built dist modules. It recorded local types and widths at NativeFunction root discovery,
counted NativeStorage join/alias reloads and mutable/address stores, then inspected the
final immutable LLVM snapshot immediately before bitcode encoding. It neither changed
the snapshot nor replaced compiler operations. Marker checks fail if the targeted code
changes. This is diagnostic instrumentation, not a supported compiler extension.

Actual loads are attributed by generated `mut<root>_` / `reload<root>_` names and
function symbol. The CLI has 166,725 attributed root loads, including 97,174 unused;
other load categories remain unassigned. A zero-consumer load has no local SSA operand
references anywhere in the final function body. Helper functions without original MIR
entries remain in LLVM totals but not local-type attribution. CLI totals include 900
defined LLVM bodies versus 878 original emitted MIR functions; parser totals are 656
versus 642. These counts must not be conflated with reachable-symbol counts.

The new CLI opcode totals match the previous interval census. Both instrumented binaries
pass their runtime oracles: parser-only's structural check returns successfully; CLI
passes the bootstrap differential checker on the 995-token, 437-node main.silk input.
No probe reads stdin. Existing native test activity overlapped the census, so instrumented
wall times are deliberately omitted. The parser scratch source snapshot is cleaned up
by the existing benchmark harness's temporary-directory bracket.

Full raw JSON and binaries are retained locally under
`/tmp/silk-aggregate-census.Pgi6EY`; those temporary files are not durable repository
artifacts. Their raw census hashes are in evidence.json. The diagnostic hook and parser
runner are preserved below so the census can be reproduced after those files expire.
The repository summary is deliberately bounded rather than retaining megabytes of
encoded symbols and all locals.

## Planning validation

Strict OpenSpec validation and scoped formatting pass for all six change files.
Census checks reconcile every CLI opcode total with the previous interval snapshot,
each reported type's root loads with its join/alias event lane totals, and the recorded
provenance with the uninstrumented baseline. Production compiler sources are unchanged.

`pnpm check` was run and stopped at root formatting before build/lint/typecheck/tests:

- `.claude/worktrees/agent-ac09fe0ffa27c76a5/packages/compiler/src/LowerExpression.ts`
- `.zuse/settings.toml`

Neither file was edited by this investigation. The worktree failure was already known;
the settings-file failure is additionally reported by this invocation. No clean root
check or implementation-completion claim is made. Separately, the previously running
interval-change validation finished: 2,438 compiler tests, 112 native shard-1/3 cases,
and all six tasks in that filtered workspace run passed. That is validation of the
existing implementation, not of the unimplemented aggregate-place proposal.

## Reproduction

Run from the repository root after a workspace build (do not rebuild dist concurrently
with tests or census processes). Save the following hook and parser runner to a new,
owned temporary directory. Replace the recorded absolute repository/scratch paths in
the runner with the current paths. No project source edits are needed.

For the CLI, with `<scratch>` replaced by that directory:

```sh
env -u NODE_OPTIONS -u NODE_COMPILE_CACHE -u NODE_V8_COVERAGE -u SILK_NATIVE_CACHE_DIR \
  NODE_DISABLE_COMPILE_CACHE=1 SILK_AGGREGATE_OUTPUT=<scratch>/cli.json \
  node --import <scratch>/census.mjs packages/cli/dist/bin.js build-exe \
  compiler/src/main.silk --source-root compiler/src --optimization debug \
  --output <scratch>/cli --timings
node compiler/scripts/test-parser.mjs <scratch>/cli compiler/src/main.silk
python3 <scratch>/parser.py
```

The runner reuses the existing benchmark's input generation and parser runtime oracle.
Do not compare its instrumented timing with clean cold samples. After replacing the
lowered-value representation, update the diagnostic observation points instead of
retaining old lane-cache operations merely to satisfy this hook.

### External snapshot and storage hook

```javascript
// External, read-only diagnostic instrumentation; never used for timing comparisons.
import { registerHooks } from 'node:module'
import { readFileSync, writeFileSync } from 'node:fs'
import { fileURLToPath } from 'node:url'
import { createHash } from 'node:crypto'

const entries = new Map()
const functions = new WeakMap()
const typeCache = new Map()
const decode = (x) => Buffer.from(x.bytes).toString('utf8')
export function enter(entry, roots, lanes, semantic, encode) {
  const types = entry.fn.localTypes.map((type) => {
    let result = typeCache.get(type)
    if (result) return result
    const spelling = encode(semantic(type))
    result = {
      kind: typeof type === 'string' ? type : type._tag,
      type: spelling.slice(0, 1000),
      typeHash: createHash('sha256').update(spelling).digest('hex'),
      lanes: lanes(type).length,
    }
    typeCache.set(type, result)
    return result
  })
  const operations = {}
  for (const block of entry.linear)
    for (const op of block.operations) operations[op._tag] = (operations[op._tag] ?? 0) + 1
  const row = {
    symbol: entry.symbol,
    operations,
    linearBlocks: entry.linear.length,
    locals: types.map((type, root) => ({
      root,
      ...type,
      mutable: roots.mutable.has(root),
      address: roots.address.has(root),
      events: {},
    })),
  }
  entries.set(entry.symbol, row)
  functions.set(entry.fn, row)
}
export function event(context, root, kind, width) {
  const local = functions.get(context.fn)?.locals[root]
  if (!local) throw new Error('Census lost root attribution')
  const count = (local.events[kind] ??= { invocations: 0, lanes: 0 })
  count.invocations++
  count.lanes += width
}
export function record(state) {
  if (state.functions.length < 100) return
  const output = []
  for (const fn of state.functions) {
    const body = fn.body
    if (!body) continue
    const symbol = decode(state.globals[fn.global].name)
    const row = entries.get(symbol) ?? { symbol, locals: [], operations: {} }
    const uses = new Uint32Array(body.values.length)
    const visit = (value) => {
      if (value === null || typeof value !== 'object') return
      if (value._tag === 'Local') {
        uses[value.value]++
        return
      }
      if (Array.isArray(value)) {
        for (const x of value) visit(x)
        return
      }
      for (const key of Object.keys(value))
        if (!['name', 'fastMath', 'access'].includes(key)) visit(value[key])
    }
    for (const inst of body.instructions) visit(inst)
    row.counts = {}
    row.loads = {}
    row.instructions = body.instructions.length
    row.unusedLoads = 0
    for (const inst of body.instructions) {
      row.counts[inst._tag] = (row.counts[inst._tag] ?? 0) + 1
      if (inst._tag !== 'Load') continue
      const name = decode(inst.name)
      const unused = uses[inst.result] === 0
      const prefix = name.replace(/[0-9].*$/, '').replace(/_.*/, '')
      const group = (row.loads[prefix] ??= { total: 0, unused: 0 })
      group.total++
      if (unused) {
        group.unused++
        row.unusedLoads++
      }
      const root = /^(?:mut|reload)(\d+)_/.exec(name)
      if (root) {
        const local = row.locals[Number(root[1])]
        if (!local) throw new Error(`Missing local ${symbol} ${name}`)
        local.loads = (local.loads ?? 0) + 1
        local.unusedLoads = (local.unusedLoads ?? 0) + Number(unused)
      }
    }
    output.push(row)
  }
  const totals = {}
  for (const row of output)
    for (const [tag, count] of Object.entries(row.counts)) totals[tag] = (totals[tag] ?? 0) + count
  writeFileSync(process.env.SILK_AGGREGATE_OUTPUT, JSON.stringify({ totals, functions: output }))
}
const replace = (source, marker, inserted) => {
  if (source.split(marker).length !== 2) throw new Error(`Census marker not unique: ${marker}`)
  return source.replace(marker, inserted)
}
registerHooks({
  load(url, context, nextLoad) {
    if (!url.startsWith('file:')) return nextLoad(url, context)
    let source
    if (url.endsWith('/packages/compiler/dist/NativeFunction.js')) {
      source = readFileSync(fileURLToPath(url), 'utf8')
      const marker = 'const roots = discoverRoots(entry.fn, entry.linear);'
      source =
        `import { enter } from ${JSON.stringify(import.meta.url)};\nimport * as CensusType from './Type.js';\n` +
        replace(
          source,
          marker,
          marker + '\nenter(entry, roots, valueLanesFor, Mir.semanticType, CensusType.encode);',
        )
    } else if (url.endsWith('/packages/compiler/dist/NativeStorage.js')) {
      source = readFileSync(fileURLToPath(url), 'utf8')
      source = replace(
        source,
        'const loaded = [];',
        "event(context, root, 'joinReload', storage.length);\nconst loaded = [];",
      )
      source = replace(
        source,
        'const values = [];',
        "event(context, root, 'aliasReload', pointers.length);\nconst values = [];",
      )
      source = replace(
        source,
        'for (const [lane, pointer] of storage.entries()) {\n        const stored',
        "event(context, root.ordinal, 'mutableStore', storage.length);\nfor (const [lane, pointer] of storage.entries()) {\n        const stored",
      )
      source = replace(
        source,
        'export const storeAddressValues = Effect.fnUntraced(function* (context, root, values, name) {',
        "export const storeAddressValues = Effect.fnUntraced(function* (context, root, values, name) {\nevent(context, root, 'addressStore', values.length);",
      )
      source = `import { event } from ${JSON.stringify(import.meta.url)};\n` + source
    } else if (url.endsWith('/packages/llvm/dist/Bitcode.js')) {
      source = readFileSync(fileURLToPath(url), 'utf8')
      const marker = "const state = yield* BuilderState.snapshot(self, 'Bitcode.encode');"
      source =
        `import { record } from ${JSON.stringify(import.meta.url)};\n` +
        replace(source, marker, marker + '\nrecord(state);')
    } else return nextLoad(url, context)
    return { format: 'module', shortCircuit: true, source }
  },
})
```

### Parser-stage runner

```python
import importlib.util
import json
import os
from pathlib import Path
import tempfile

root = Path('/Users/juliaortiz/Documents/dev.nosync/silk')
spec = importlib.util.spec_from_file_location('stages', root / 'benchmarks/selfhost-stages/run.py')
stages = importlib.util.module_from_spec(spec)
spec.loader.exec_module(stages)
env = os.environ.copy()
for name in stages.REMOVED_ENVIRONMENT:
    env.pop(name, None)
env['NODE_DISABLE_COMPILE_CACHE'] = '1'
with tempfile.TemporaryDirectory(prefix='.aggregate-census-', dir=root / 'benchmarks') as temporary:
    scratch = Path(temporary)
    snapshot, input_path, oracle, generated_hash = stages.prepare(scratch, env, 'node')
    env['NODE_OPTIONS'] = '--import=/tmp/silk-aggregate-census.Pgi6EY/census.mjs'
    env['SILK_AGGREGATE_OUTPUT'] = '/tmp/silk-aggregate-census.Pgi6EY/parser.json'
    stages.measure('parser', 0, 0, scratch, snapshot, input_path, oracle, env, 'node')
    print(json.dumps({'oracle': oracle, 'generatedInputSha256': generated_hash,
                      'sourceHashes': stages.source_hashes(), 'runtimeCheck': 'passed'}))
```
