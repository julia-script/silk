import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'

/** The closed set of target identities supported by the bootstrap compiler. */
export type Id =
  | 'aarch64-apple-darwin'
  | 'x86_64-unknown-linux-gnu'
  | 'aarch64-unknown-linux-gnu'
  | 'wasm32-unknown-unknown'

/** Backend-neutral target facts owned by the compiler. */
export interface Target {
  readonly _tag: 'Target'
  readonly id: Id
  readonly kind: 'Native' | 'WebAssembly'
  readonly pointerSize: 4 | 8
  readonly pointerAlignment: 4 | 8
  readonly endianness: 'little'
}

/** Expected failure while resolving a target request. */
export class TargetError extends Data.TaggedError('TargetError')<{
  readonly operation:
    | 'Target.resolve'
    | 'Target.host'
    | 'Target.requireNative'
    | 'Target.validateInventory'
  readonly requested: string
  readonly message: string
  readonly unavailableOperations?: ReadonlyArray<string>
}> {}

/** Queryable result of selecting a compilation target. */
export type Selection =
  | { readonly _tag: 'Resolved'; readonly target: Target }
  | { readonly _tag: 'Unavailable'; readonly error: TargetError }

const make = (id: Id, kind: Target['kind'], pointerSize: 4 | 8): Target =>
  Object.freeze({
    _tag: 'Target',
    id,
    kind,
    pointerSize,
    pointerAlignment: pointerSize,
    endianness: 'little',
  })

export const aarch64AppleDarwin = make('aarch64-apple-darwin', 'Native', 8)
export const x8664UnknownLinuxGnu = make('x86_64-unknown-linux-gnu', 'Native', 8)
export const aarch64UnknownLinuxGnu = make('aarch64-unknown-linux-gnu', 'Native', 8)
export const wasm32UnknownUnknown = make('wasm32-unknown-unknown', 'WebAssembly', 4)

/** Every canonical target in deterministic identity order. */
export const all: ReadonlyArray<Target> = Object.freeze([
  aarch64AppleDarwin,
  aarch64UnknownLinuxGnu,
  wasm32UnknownUnknown,
  x8664UnknownLinuxGnu,
])

/** The three targets required to host the stage-2 bootstrap compiler. */
export const native: ReadonlyArray<Target> = Object.freeze(all.filter(isNative))

/** Whether a target can host the native bootstrap compiler. */
export function isNative(self: Target): boolean {
  return self.kind === 'Native'
}

const unavailable = (
  operation: TargetError['operation'],
  requested: string,
  subject: 'target' | 'host',
): Selection =>
  Object.freeze({
    _tag: 'Unavailable',
    error: new TargetError({
      operation,
      requested,
      message: `Unsupported bootstrap ${subject} ${requested}`,
    }),
  })

/** Selects an explicit target or the supplied host as immutable queryable data. */
export const select = (
  requested: string | undefined,
  hostPlatform?: string,
  hostArch?: string,
): Selection => {
  if (requested !== undefined) {
    const found = all.find((candidate) => candidate.id === requested)
    return found === undefined
      ? unavailable('Target.resolve', requested, 'target')
      : Object.freeze({ _tag: 'Resolved', target: found })
  }
  if (hostPlatform === undefined || hostArch === undefined) {
    return unavailable('Target.host', 'unspecified-host', 'host')
  }
  const hostId =
    hostPlatform === 'darwin' && hostArch === 'arm64'
      ? aarch64AppleDarwin.id
      : hostPlatform === 'linux' && hostArch === 'x64'
        ? x8664UnknownLinuxGnu.id
        : hostPlatform === 'linux' && hostArch === 'arm64'
          ? aarch64UnknownLinuxGnu.id
          : `${hostArch}-${hostPlatform}`
  const found = native.find((candidate) => candidate.id === hostId)
  return found === undefined
    ? unavailable('Target.host', hostId, 'host')
    : Object.freeze({ _tag: 'Resolved', target: found })
}

/** Looks up a canonical target without introducing a fallback. */
export const resolve = Effect.fn('Target.resolve')(function* (
  requested: string,
): Effect.fn.Return<Target, TargetError> {
  const selected = select(requested)
  if (selected._tag === 'Resolved') return selected.target
  return yield* selected.error
})

/** Resolves host facts supplied by the application edge. */
export const fromHost = Effect.fn('Target.fromHost')(function* (
  hostPlatform: string,
  hostArch: string,
): Effect.fn.Return<Target, TargetError> {
  const selected = select(undefined, hostPlatform, hostArch)
  if (selected._tag === 'Resolved') return selected.target
  return yield* selected.error
})

/** Requires a compiler target that can proceed through native object emission and linking. */
export const requireNative = Effect.fn('Target.requireNative')(function* (
  self: Target,
): Effect.fn.Return<Target, TargetError> {
  if (isNative(self)) return self
  return yield* new TargetError({
    operation: 'Target.requireNative',
    requested: self.id,
    message: `Native compilation does not support target ${self.id}`,
  })
})

/** Reports a valid program whose reachable inventory has no implementation on one target. */
export const unavailableInventory = (
  self: Target,
  operations: ReadonlyArray<string>,
): TargetError =>
  new TargetError({
    operation: 'Target.validateInventory',
    requested: self.id,
    message: `Target ${self.id} does not support ${operations.join(', ')}`,
    unavailableOperations: Object.freeze([...operations]),
  })

/** Whether a target value exactly matches its compiler-owned canonical profile. */
export const isCanonical = (self: Target): boolean =>
  all.some(
    (candidate) =>
      candidate.id === self.id &&
      candidate.kind === self.kind &&
      candidate.pointerSize === self.pointerSize &&
      candidate.pointerAlignment === self.pointerAlignment &&
      candidate.endianness === self.endianness,
  )

/** Deterministic textual encoding of compiler-owned target facts. */
export const encode = (self: Target): string =>
  `${self.id} kind=${self.kind} pointer=${self.pointerSize}/${self.pointerAlignment} endian=${self.endianness}`
