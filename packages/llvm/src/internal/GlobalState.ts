import * as AddrSpace from '../AddrSpace.js'
import * as Alignment from '../Alignment.js'
import * as ByteString from '../ByteString.js'
import type * as Global from '../Global.js'
import { invalidInput, invalidState, type LlvmError } from '../LlvmError.js'
import type * as BuilderState from './BuilderState.js'
import * as CanonicalKey from './CanonicalKey.js'
import type * as GlobalDescription from './GlobalDescription.js'
import * as Handle from './Handle.js'
import type * as OwnedHandle from './OwnedHandle.js'

export interface CommonOptions {
  readonly addressSpace?: AddrSpace.AddrSpace
  readonly linkage?: GlobalDescription.Linkage
  readonly visibility?: GlobalDescription.Visibility
  readonly preemption?: GlobalDescription.Preemption
  readonly dllStorage?: GlobalDescription.DllStorage
  readonly unnamedAddress?: GlobalDescription.UnnamedAddress
  readonly section?: ByteString.ByteString
  readonly alignment?: Alignment.Alignment
}

/** @internal */
const defaults = (name: ByteString.ByteString, options: CommonOptions): GlobalDescription.Common =>
  Object.freeze({
    name,
    addressSpace: options.addressSpace ?? AddrSpace.defaultAddrSpace,
    linkage: options.linkage ?? 'external',
    visibility: options.visibility ?? 'default',
    preemption: options.preemption ?? 'dso_preemptable',
    dllStorage: options.dllStorage ?? 'default',
    unnamedAddress: options.unnamedAddress ?? 'none',
    section: options.section ?? ByteString.empty,
    alignment: options.alignment ?? Alignment.defaultAlignment,
  })

/** @internal */
const availableAnonymousName = (state: BuilderState.MutableState): ByteString.ByteString => {
  while (true) {
    const name: ByteString.ByteString = Object.freeze({
      _tag: 'ByteString',
      bytes: Object.freeze(
        Array.from(String(state.nextAnonymousGlobal), (character) => character.charCodeAt(0)),
      ),
    })
    state.nextAnonymousGlobal += 1
    if (!state.globalNames.has(CanonicalKey.bytes(name))) return name
  }
}

/** @internal */
export const allocate = (
  state: BuilderState.MutableState,
  owner: OwnedHandle.Owner,
  name: ByteString.ByteString,
  kind: GlobalDescription.GlobalDescription['kind'],
  actorIndex: number,
  options: CommonOptions,
  operation: string,
): Result.Result<{ readonly index: number; readonly handle: Global.Global }, LlvmError> => {
  const resolvedName = name.bytes.length === 0 ? availableAnonymousName(state) : name
  const key = CanonicalKey.bytes(resolvedName)
  const occupied = state.globalNames.get(key)
  if (occupied !== undefined) {
    return Result.fail(
      invalidState({
        operation,
        message: 'LLVM global name is already occupied',
        state: { name: resolvedName, occupied },
      }),
    )
  }
  const index = state.globals.length
  const handle = Handle.make('Global', owner, index)
  state.globals.push(
    Object.freeze({
      _tag: 'Global',
      ...defaults(resolvedName, options),
      kind,
      actorIndex,
      replacement: undefined,
      deleted: false,
    }),
  )
  state.globalHandles.push(handle)
  state.globalMetadata.push(Object.freeze([]))
  state.globalNames.set(key, index)
  return Result.succeed({ index, handle })
}

/** @internal */
export const resolveIndex = (
  state: BuilderState.MutableState,
  index: number,
  operation: string,
): Result.Result<number, LlvmError> => {
  const visited = new globalThis.Set<number>()
  let current = index
  while (true) {
    if (visited.has(current)) {
      return Result.fail(
        invalidInput({ operation, message: 'Global replacement cycle detected', input: index }),
      )
    }
    visited.add(current)
    const description = state.globals[current]
    if (description === undefined) {
      return Result.fail(
        invalidState({ operation, message: 'Global table entry is missing', state: current }),
      )
    }
    if (description.replacement === undefined) return Result.succeed(current)
    current = description.replacement
  }
}

/** @internal */
export const resolve = (
  builder: import('../Builder.js').Builder,
  state: BuilderState.MutableState,
  owner: OwnedHandle.Owner,
  self: Global.Global,
  operation: string,
): Result.Result<
  { readonly index: number; readonly description: GlobalDescription.GlobalDescription },
  LlvmError
> =>
  Result.gen(function* () {
    const original = yield* Handle.resolve(builder, owner, self, 'Global', operation)
    const index = yield* resolveIndex(state, original, operation)
    const description = state.globals[index]
    if (description === undefined || description.deleted) {
      return yield* Result.fail(
        invalidInput({ operation, message: 'Global has been deleted', input: self }),
      )
    }
    return { index, description }
  })

/** @internal */
export const handleAt = (
  state: Pick<BuilderState.MutableState, 'globalHandles'>,
  index: number,
  operation: string,
): Result.Result<Global.Global, LlvmError> => {
  const handle = state.globalHandles[index]
  if (handle === undefined) {
    return Result.fail(
      invalidState({ operation, message: 'Global table handle is missing', state: index }),
    )
  }
  return Result.succeed(handle)
}

import * as Result from 'effect/Result'
