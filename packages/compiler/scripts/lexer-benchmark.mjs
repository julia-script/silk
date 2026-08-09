import * as Lexer from '../dist/Lexer.js'
import * as SourceFile from '../dist/SourceFile.js'

const kibibyte = 1024
const mebibyte = kibibyte * kibibyte
const fragment = Uint8Array.from(
  '// benchmark\npub fn main() -> i32 { return 123456 }\n',
  (character) => character.charCodeAt(0),
)
const bytes = new Uint8Array(mebibyte)
for (let offset = 0; offset < bytes.length; offset += fragment.length) {
  bytes.set(fragment.subarray(0, Math.min(fragment.length, bytes.length - offset)), offset)
}

const source = SourceFile.make('benchmark://one-mib.silk', bytes)
for (let index = 0; index < 3; index += 1) Lexer.lex(source)

const samples = []
let observedTokens = 0
for (let index = 0; index < 10; index += 1) {
  const started = process.hrtime.bigint()
  const result = Lexer.lex(source)
  const elapsedSeconds = Number(process.hrtime.bigint() - started) / 1_000_000_000
  samples.push(1 / elapsedSeconds)
  observedTokens += result.tokens.length
}

samples.sort((left, right) => left - right)
const median = samples[Math.floor(samples.length / 2)]
if (median === undefined) throw new Error('benchmark did not produce a median')

console.log(
  JSON.stringify(
    {
      workloadBytes: bytes.length,
      warmupRuns: 3,
      measuredRuns: samples.length,
      medianMiBPerSecond: Number(median.toFixed(2)),
      minimumMiBPerSecond: Number(samples[0]?.toFixed(2)),
      observedTokens,
    },
    undefined,
    2,
  ),
)
