import * as Option from 'effect/Option'
import * as Lexer from '../../dist/Lexer.js'
import * as Parser from '../../dist/Parser.js'
import * as SourceFile from '../../dist/SourceFile.js'
import * as SyntaxCorrespondence from '../../dist/SyntaxCorrespondence.js'

const encoder = new TextEncoder()
const parse = (source) =>
  Parser.parse(Lexer.lex(SourceFile.make('app/Main', encoder.encode(source))))
const declaration = (name, value) => `pub fn ${name}() -> i32 { return ${value} }`
const previous = parse(`${declaration('first', 1)}\n${declaration('second', 2)}`)
const current = parse(
  `${declaration('first', 1)}\n${declaration('inserted', 0)}\n${declaration('second', 2)}`,
)
const correspondence = Option.getOrThrow(SyntaxCorrespondence.between(previous, current))

process.stdout.write(
  JSON.stringify({ identities: correspondence.identities, counts: correspondence.counts }),
)
