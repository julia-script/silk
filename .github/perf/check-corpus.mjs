import { readFileSync } from 'node:fs'
const lines = readFileSync(process.argv[2], 'utf8').split('\n')
// Part one preserves merged #615's baseline; five timeouts remain in B11.
const baselinePasses = [
  'foreign-libc-pointer-roundtrip',
  'foreign-libc-floating',
  'else-if-chain',
  'scalar-reference-read',
  'scalar-reference-write-through',
  'literal',
  'identity',
  'second-parameter',
  'nested',
  'nested-siblings',
  'forward-call',
  'direct-recursion',
  'mutual-recursion',
  'binding',
  'binding-chain',
  'moved-binding',
  'operator-precedence',
  'operator-negation-overflow-trap',
  'operator-bool-not',
  'mutable-scalar-loop',
  'nested-loops',
  'foreign-libc-abs',
]
const acceptedFailures = new Set([
  'ecdsa-p256-verification',
  'http-client-request',
  'json-scanner',
  'toml-scanner',
  'borrowed-temporary-stream-suspension',
  'integer-operation-matrix',
  'float-operation-matrix',
  'chacha20-poly1305',
  'aes-gcm',
  'rsa-verification',
  'fixed-output-sha',
  'pointer-parameter-write',
  'foreign-libc-qsort-callback',
  'foreign-libc-environ-static',
  'secure-random-provider',
])
const passes = new Set(lines.filter((line) => line.startsWith('PASS ')).map((line) => line.slice(5)))
for (const name of baselinePasses) {
  if (!passes.has(name)) throw new Error(`Baseline PASS lost: ${name}`)
}
for (const line of lines.filter((line) => line.startsWith('FAIL '))) {
  const name = line.slice(5).split(':', 1)[0]
  if (!acceptedFailures.has(name)) throw new Error(`New corpus failure: ${line}`)
}
if (lines.some((line) => /(?:code=RUNTIME_MISMATCH|code=CORPUS_RUNNER_FAILURE|module-source:)/.test(line))) {
  throw new Error('Corpus has a runtime mismatch, runner failure, or module-source gap')
}
const timings = lines.filter((line) => line.startsWith('SELFHOST_CASE_TIMING='))
const outcomes = lines.filter((line) => /^(?:PASS|FAIL|UNSUPPORTED) /.test(line))
if (timings.length !== outcomes.length || outcomes.length === 0) throw new Error('Incomplete corpus timing receipt')
console.log(`Preserved ${baselinePasses.length} baseline passes; remaining failures are accepted baseline programs`)
