import * as Data from 'effect/Data'

export class LexError extends Data.TaggedError('LexError')<{
  readonly message: string
  readonly start: number
  readonly end: number
  readonly found: string
}> {}

export class ParseError extends Data.TaggedError('ParseError')<{
  readonly message: string
  readonly start: number
  readonly end: number
  readonly expected: string
  readonly found: string
}> {}

export type CompileReason =
  | { readonly _tag: 'UnsupportedProgram' }
  | { readonly _tag: 'MissingCallResult'; readonly name: string }

export type ResolutionReason =
  | { readonly _tag: 'DuplicateFunction'; readonly name: string }
  | { readonly _tag: 'InvalidMain' }
  | { readonly _tag: 'UnknownName'; readonly name: string }
  | { readonly _tag: 'UnknownFunction'; readonly name: string }
  | {
      readonly _tag: 'WrongArity'
      readonly name: string
      readonly expected: number
      readonly actual: number
    }

export class ResolutionError extends Data.TaggedError('ResolutionError')<{
  readonly operation: string
  readonly message: string
  readonly start: number
  readonly end: number
  readonly reason: ResolutionReason
}> {}

export class CompileError extends Data.TaggedError('CompileError')<{
  readonly operation: string
  readonly message: string
  readonly start: number
  readonly end: number
  readonly reason: CompileReason
}> {}

export class CliError extends Data.TaggedError('CliError')<{
  readonly message: string
}> {}
