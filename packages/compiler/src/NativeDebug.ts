import type * as Builder from '@silk-effect/llvm/Builder'
import type * as LlvmError from '@silk-effect/llvm/LlvmError'
import * as LlvmMetadata from '@silk-effect/llvm/Metadata'
import * as Effect from 'effect/Effect'
import * as Layout from './Layout.js'
import * as Mir from './Mir.js'
import * as SilkType from './Type.js'

/** Debug metadata state shared by all declarations in one native module. */
export interface LoweringContext {
  readonly builder: Builder.Builder
  readonly program: Mir.Module
  readonly enabled: boolean
  readonly file: LlvmMetadata.Optional
  readonly types: Map<string, LlvmMetadata.Optional>
}

/** Emits and memoizes the source-level debug type for one MIR value. */
export const typeOf = Effect.fn('NativeDebug.typeOf')(function* (
  context: LoweringContext,
  type: Mir.Type,
): Effect.fn.Return<LlvmMetadata.Optional, LlvmError.LlvmError> {
  if (!context.enabled) return undefined
  const semantic = Mir.semanticType(type)
  const key = SilkType.key(semantic)
  if (context.types.has(key)) return context.types.get(key)
  const selected = Layout.entry(context.program.layout, semantic)
  if (selected === undefined) {
    context.types.set(key, undefined)
    return undefined
  }
  let metadata: LlvmMetadata.Optional
  if (SilkType.isString(semantic)) {
    metadata = yield* LlvmMetadata.stringType(context.builder, {
      name: yield* LlvmMetadata.string(context.builder, 'string'),
      sizeInBits: selected.size * 8,
      alignInBits: selected.alignment * 8,
      encoding: 'utf',
    })
  } else if (SilkType.isSlice(semantic) && SilkType.equals(semantic.element, 'u8')) {
    metadata = yield* LlvmMetadata.structureType(context.builder, {
      name: yield* LlvmMetadata.string(context.builder, SilkType.encode(semantic)),
      file: context.file,
      sizeInBits: selected.size * 8,
      alignInBits: selected.alignment * 8,
    })
  }
  context.types.set(key, metadata)
  return metadata
})

/** First source byte covered by one lowered function. */
export const functionStart = (fn: Mir.MirFunction): number => {
  const region = fn.regions.find((candidate) => candidate.id.ordinal === fn.entry.ordinal)
  if (region === undefined) return 0
  if (region._tag === 'ConditionalRegion' || region._tag === 'LoopRegion')
    return region.provenance.span.start
  return (
    (region._tag === 'OperationRegion' ? region.operations : region.releases).at(0)?.provenance.span
      .start ?? region.outcome.provenance.span.start
  )
}
