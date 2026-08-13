/**
 * The explicit process-input boundary: the command line, the environment, and the working
 * directory. Every value is raw bytes, exactly as the process received them, because neither an
 * argument nor an environment value is required to be UTF-8 on a POSIX host.
 */

/** One completed lookup observation retained by deterministic and in-memory providers. */
export type LookupEvent =
  | { readonly _tag: 'ArgumentCount' }
  | { readonly _tag: 'Argument'; readonly index: number }
  | { readonly _tag: 'Variable'; readonly name: ReadonlyArray<number> }
  | { readonly _tag: 'WorkingDirectory' }

/**
 * The typed provider result of one byte-valued lookup. Absence is an ordinary answer — an index
 * past the last argument, or an unset variable name — and is distinct from a host that could not
 * answer at all.
 */
export type Lookup =
  | { readonly _tag: 'Present'; readonly bytes: ReadonlyArray<number> }
  | { readonly _tag: 'Absent' }
  | { readonly _tag: 'LookupFailure'; readonly message: string }

/** The typed provider result of the argument-count lookup. */
export type Count =
  | { readonly _tag: 'Count'; readonly count: number }
  | { readonly _tag: 'LookupFailure'; readonly message: string }

/** The explicit host boundary used by evaluation. It reads and never writes. */
export interface Provider {
  readonly argumentCount: () => Count
  readonly argument: (index: number) => Lookup
  readonly variable: (name: ReadonlyArray<number>) => Lookup
  readonly workingDirectory: () => Lookup
}

/** A replaceable provider plus its immutable event snapshot. */
export interface Memory {
  readonly _tag: 'HostInputMemory'
  readonly provider: Provider
  readonly events: () => ReadonlyArray<LookupEvent>
}

/** The scripted process input an in-memory provider answers from. */
export interface Script {
  /** The complete argument sequence, program name first, as raw bytes. */
  readonly arguments?: ReadonlyArray<ReadonlyArray<number>>
  /** The set variable names and their raw byte values. A missing name is absent, not a failure. */
  readonly environment?: ReadonlyArray<readonly [ReadonlyArray<number>, ReadonlyArray<number>]>
  /** The working directory's raw bytes. */
  readonly workingDirectory?: ReadonlyArray<number>
  /** Lookups that report a host error instead of an answer. */
  readonly failing?: ReadonlyArray<'argumentCount' | 'argument' | 'variable' | 'workingDirectory'>
}

const sameBytes = (left: ReadonlyArray<number>, right: ReadonlyArray<number>): boolean =>
  left.length === right.length && left.every((byte, index) => byte === right[index])

/**
 * Builds an in-memory provider answering from a scripted command line, environment, and working
 * directory. It has no host boundary, so a test observes exactly the bytes it supplied, including
 * bytes that are not valid UTF-8.
 */
export const memory = (script: Script = {}): Memory => {
  const argumentList = (script.arguments ?? []).map((entry) => Object.freeze(Array.from(entry)))
  const environment = (script.environment ?? []).map(
    ([name, value]) => [Array.from(name), Array.from(value)] as const,
  )
  const workingDirectory = Object.freeze(Array.from(script.workingDirectory ?? []))
  const failing = new Set(script.failing ?? [])
  const recorded: Array<LookupEvent> = []
  const failure = (operation: string): Lookup & Count =>
    Object.freeze({ _tag: 'LookupFailure', message: `host input ${operation} failed` })
  const provider: Provider = Object.freeze({
    argumentCount: () => {
      recorded.push(Object.freeze({ _tag: 'ArgumentCount' }))
      if (failing.has('argumentCount')) return failure('argumentCount')
      return Object.freeze({ _tag: 'Count', count: argumentList.length })
    },
    argument: (index: number) => {
      recorded.push(Object.freeze({ _tag: 'Argument', index }))
      if (failing.has('argument')) return failure('argument')
      const selected = argumentList.at(index)
      if (index < 0 || selected === undefined) return Object.freeze({ _tag: 'Absent' })
      return Object.freeze({ _tag: 'Present', bytes: selected })
    },
    variable: (name: ReadonlyArray<number>) => {
      recorded.push(Object.freeze({ _tag: 'Variable', name: Object.freeze(Array.from(name)) }))
      if (failing.has('variable')) return failure('variable')
      const selected = environment.find((entry) => sameBytes(entry[0], name))
      if (selected === undefined) return Object.freeze({ _tag: 'Absent' })
      return Object.freeze({ _tag: 'Present', bytes: Object.freeze(Array.from(selected[1])) })
    },
    workingDirectory: () => {
      recorded.push(Object.freeze({ _tag: 'WorkingDirectory' }))
      if (failing.has('workingDirectory')) return failure('workingDirectory')
      return Object.freeze({ _tag: 'Present', bytes: workingDirectory })
    },
  })
  return Object.freeze({
    _tag: 'HostInputMemory',
    provider,
    events: () => Object.freeze(recorded.map((event) => event)),
  })
}

/** Builds the scripted byte sequence of one ASCII value, for tests written in text. */
export const ascii = (value: string): ReadonlyArray<number> =>
  Object.freeze(Array.from(value, (character) => character.charCodeAt(0)))
