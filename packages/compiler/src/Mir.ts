import * as Option from 'effect/Option'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'

/**
 * MIR: the monomorphic, backend-neutral basic-block control-flow graph over logical Silk types.
 * It contains no LLVM types, instructions, intrinsics, attributes, metadata, or physical field
 * offsets, and adopts no WebAssembly control shapes. The target layout is a separate input
 * consumed only at emission time.
 */

/** A logical Silk type at the MIR level. The frozen slice knows only `I32`. */
export type Type = { readonly _tag: 'I32' }

/** The explicit emission-time target-layout input. Never read by MIR itself. */
export interface TargetLayout {
  readonly _tag: 'TargetLayout'
  readonly triple: string
  readonly pointerWidth: 32 | 64
  readonly endianness: 'little' | 'big'
  readonly i32: { readonly size: number; readonly alignment: number }
}

/** One ordinal-indexed virtual register local to a function. */
export interface LocalId {
  readonly _tag: 'Local'
  readonly ordinal: number
}

/** One ordinal-indexed basic block local to a function. */
export interface BlockId {
  readonly _tag: 'Block'
  readonly ordinal: number
}

/** Source provenance: the causative span, with compiler-generated operations marked. */
export interface Provenance {
  readonly span: SourceSpan.SourceSpan
  readonly generated: boolean
}

/** One MIR operation. */
export type Operation =
  | {
      readonly _tag: 'Literal'
      readonly destination: LocalId
      readonly type: Type
      readonly value: number
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'Move'
      readonly destination: LocalId
      readonly source: LocalId
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'Call'
      readonly destination: LocalId
      readonly target: DeclarationIndex.CanonicalId
      readonly arguments: ReadonlyArray<LocalId>
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'Drop'
      readonly local: LocalId
      readonly provenance: Provenance
    }

/** One MIR terminator. */
export type Terminator =
  | { readonly _tag: 'Return'; readonly value: LocalId; readonly provenance: Provenance }
  | { readonly _tag: 'Jump'; readonly target: BlockId; readonly provenance: Provenance }
  | {
      readonly _tag: 'Branch'
      readonly condition: LocalId
      readonly taken: BlockId
      readonly otherwise: BlockId
      readonly provenance: Provenance
    }
  | { readonly _tag: 'Trap'; readonly reason: string; readonly provenance: Provenance }

/** One basic block. Cleanup paths are explicitly marked cleanup blocks. */
export interface Block {
  readonly _tag: 'MirBlock'
  readonly id: BlockId
  readonly kind: 'Normal' | 'Cleanup'
  readonly operations: ReadonlyArray<Operation>
  readonly terminator: Terminator
}

/** One monomorphic MIR function. Parameters pre-bind to the first locals. */
export interface MirFunction {
  readonly _tag: 'MirFunction'
  readonly id: DeclarationIndex.CanonicalId
  readonly parameterCount: number
  readonly localTypes: ReadonlyArray<Type>
  readonly result: Type
  readonly blocks: ReadonlyArray<Block>
}

/** One MIR module. */
export interface Module {
  readonly _tag: 'MirModule'
  readonly module: string
  readonly functions: ReadonlyArray<MirFunction>
}

/** One structural invariant violation, reported as data. */
export interface Violation {
  readonly _tag: 'Violation'
  readonly rule: 'MissingEntryBlock' | 'UnknownBlockTarget' | 'UndeclaredLocal'
  readonly function: DeclarationIndex.CanonicalId
  readonly block?: BlockId
  readonly detail: string
}

const localUses = (block: Block): ReadonlyArray<LocalId> => [
  ...block.operations.flatMap((operation): ReadonlyArray<LocalId> => {
    if (operation._tag === 'Literal') return [operation.destination]
    if (operation._tag === 'Move') return [operation.destination, operation.source]
    if (operation._tag === 'Call') return [operation.destination, ...operation.arguments]
    return [operation.local]
  }),
  ...(block.terminator._tag === 'Return'
    ? [block.terminator.value]
    : block.terminator._tag === 'Branch'
      ? [block.terminator.condition]
      : []),
]

const blockTargets = (block: Block): ReadonlyArray<BlockId> =>
  block.terminator._tag === 'Jump'
    ? [block.terminator.target]
    : block.terminator._tag === 'Branch'
      ? [block.terminator.taken, block.terminator.otherwise]
      : []

/** Verifies structural invariants, returning an ordered deterministic violation collection. */
export const verify = (self: Module): ReadonlyArray<Violation> => {
  const violations: Array<Violation> = []
  for (const fn of self.functions) {
    if (fn.blocks.length === 0) {
      violations.push(
        Object.freeze({
          _tag: 'Violation',
          rule: 'MissingEntryBlock',
          function: fn.id,
          detail: 'function has no entry block',
        }),
      )
      continue
    }
    for (const block of fn.blocks) {
      for (const target of blockTargets(block)) {
        if (target.ordinal < 0 || target.ordinal >= fn.blocks.length) {
          violations.push(
            Object.freeze({
              _tag: 'Violation',
              rule: 'UnknownBlockTarget',
              function: fn.id,
              block: block.id,
              detail: `terminator targets missing block bb${target.ordinal}`,
            }),
          )
        }
      }
      for (const local of localUses(block)) {
        if (local.ordinal < 0 || local.ordinal >= fn.localTypes.length) {
          violations.push(
            Object.freeze({
              _tag: 'Violation',
              rule: 'UndeclaredLocal',
              function: fn.id,
              block: block.id,
              detail: `references undeclared local %${local.ordinal}`,
            }),
          )
        }
      }
    }
  }
  return Object.freeze(violations)
}

const spanText = (span: SourceSpan.SourceSpan): string => `[${span.start}, ${span.end})`

const provenanceText = (provenance: Provenance): string =>
  `${spanText(provenance.span)}${provenance.generated ? ' generated' : ''}`

const localText = (local: LocalId): string => `%${local.ordinal}`

const targetText = (target: DeclarationIndex.CanonicalId): string =>
  `${target.module}.${target.name}`

const operationText = (operation: Operation): string => {
  switch (operation._tag) {
    case 'Literal':
      return `${localText(operation.destination)} = literal ${operation.value} : ${operation.type._tag} ${provenanceText(operation.provenance)}`
    case 'Move':
      return `${localText(operation.destination)} = move ${localText(operation.source)} ${provenanceText(operation.provenance)}`
    case 'Call':
      return `${localText(operation.destination)} = call ${targetText(operation.target)}(${operation.arguments.map(localText).join(', ')}) : ${operation.type._tag} ${provenanceText(operation.provenance)}`
    case 'Drop':
      return `drop ${localText(operation.local)} ${provenanceText(operation.provenance)}`
  }
}

const terminatorText = (terminator: Terminator): string => {
  switch (terminator._tag) {
    case 'Return':
      return `return ${localText(terminator.value)} ${provenanceText(terminator.provenance)}`
    case 'Jump':
      return `jump bb${terminator.target.ordinal} ${provenanceText(terminator.provenance)}`
    case 'Branch':
      return `branch ${localText(terminator.condition)} ? bb${terminator.taken.ordinal} : bb${terminator.otherwise.ordinal} ${provenanceText(terminator.provenance)}`
    case 'Trap':
      return `trap "${terminator.reason}" ${provenanceText(terminator.provenance)}`
  }
}

/**
 * Deterministic textual encoding of one MIR module for debugging, inspection, and golden tests.
 * Layout-free by construction; no compatibility promise attaches to this format.
 */
export const encode = (self: Module): string =>
  [
    `mir-module ${self.module}`,
    ...self.functions.flatMap((fn) => [
      `fn ${targetText(fn.id)} params=${fn.parameterCount} locals=${fn.localTypes.length} -> ${fn.result._tag}`,
      ...fn.blocks.flatMap((block) => [
        `  bb${block.id.ordinal}${block.kind === 'Cleanup' ? ' cleanup' : ''}:`,
        ...block.operations.map((operation) => `    ${operationText(operation)}`),
        `    ${terminatorText(block.terminator)}`,
      ]),
    ]),
    '',
  ].join('\n')

const sampleSpan = (
  source: SourceFile.SourceFile,
  start: number,
  end: number,
): SourceSpan.SourceSpan =>
  Option.getOrThrowWith(
    SourceSpan.make(source, start, end),
    () => new RangeError('MIR sample produced an invalid span'),
  )

const local = (ordinal: number): LocalId => Object.freeze({ _tag: 'Local', ordinal })
const block = (ordinal: number): BlockId => Object.freeze({ _tag: 'Block', ordinal })
const i32: Type = Object.freeze({ _tag: 'I32' })

const canonical = (module: string, name: string): DeclarationIndex.CanonicalId =>
  Object.freeze({ _tag: 'CanonicalDeclarationId', module, name })

/**
 * Hand-built sample modules for tests, goldens, and the CFG lab. They are dev fixtures pinned by
 * goldens until lowering produces real MIR, then retire.
 */
export const samples = (): ReadonlyArray<Module> => {
  const straightSource = SourceFile.make(
    'sample://straight.silk',
    Uint8Array.from(
      'pub fn answer() -> I32 { return 42 }\npub fn main() -> I32 { return answer() }',
      (c) => c.charCodeAt(0),
    ),
  )
  const straight: Module = Object.freeze({
    _tag: 'MirModule',
    module: 'sample://straight.silk',
    functions: Object.freeze([
      Object.freeze({
        _tag: 'MirFunction' as const,
        id: canonical('sample://straight.silk', 'answer'),
        parameterCount: 0,
        localTypes: Object.freeze([i32]),
        result: i32,
        blocks: Object.freeze([
          Object.freeze({
            _tag: 'MirBlock' as const,
            id: block(0),
            kind: 'Normal' as const,
            operations: Object.freeze([
              Object.freeze({
                _tag: 'Literal' as const,
                destination: local(0),
                type: i32,
                value: 42,
                provenance: Object.freeze({
                  span: sampleSpan(straightSource, 32, 34),
                  generated: false,
                }),
              }),
            ]),
            terminator: Object.freeze({
              _tag: 'Return' as const,
              value: local(0),
              provenance: Object.freeze({
                span: sampleSpan(straightSource, 25, 34),
                generated: false,
              }),
            }),
          }),
        ]),
      }),
      Object.freeze({
        _tag: 'MirFunction' as const,
        id: canonical('sample://straight.silk', 'main'),
        parameterCount: 0,
        localTypes: Object.freeze([i32]),
        result: i32,
        blocks: Object.freeze([
          Object.freeze({
            _tag: 'MirBlock' as const,
            id: block(0),
            kind: 'Normal' as const,
            operations: Object.freeze([
              Object.freeze({
                _tag: 'Call' as const,
                destination: local(0),
                target: canonical('sample://straight.silk', 'answer'),
                arguments: Object.freeze([]),
                type: i32,
                provenance: Object.freeze({
                  span: sampleSpan(straightSource, 67, 75),
                  generated: false,
                }),
              }),
            ]),
            terminator: Object.freeze({
              _tag: 'Return' as const,
              value: local(0),
              provenance: Object.freeze({
                span: sampleSpan(straightSource, 60, 75),
                generated: false,
              }),
            }),
          }),
        ]),
      }),
    ]),
  })

  const branchingSource = SourceFile.make(
    'sample://branching.silk',
    Uint8Array.from('pub fn choose(flag: I32) -> I32 { return flag }', (c) => c.charCodeAt(0)),
  )
  const branching: Module = Object.freeze({
    _tag: 'MirModule',
    module: 'sample://branching.silk',
    functions: Object.freeze([
      Object.freeze({
        _tag: 'MirFunction' as const,
        id: canonical('sample://branching.silk', 'choose'),
        parameterCount: 1,
        localTypes: Object.freeze([i32, i32]),
        result: i32,
        blocks: Object.freeze([
          Object.freeze({
            _tag: 'MirBlock' as const,
            id: block(0),
            kind: 'Normal' as const,
            operations: Object.freeze([]),
            terminator: Object.freeze({
              _tag: 'Branch' as const,
              condition: local(0),
              taken: block(1),
              otherwise: block(2),
              provenance: Object.freeze({
                span: sampleSpan(branchingSource, 34, 45),
                generated: false,
              }),
            }),
          }),
          Object.freeze({
            _tag: 'MirBlock' as const,
            id: block(1),
            kind: 'Normal' as const,
            operations: Object.freeze([
              Object.freeze({
                _tag: 'Move' as const,
                destination: local(1),
                source: local(0),
                provenance: Object.freeze({
                  span: sampleSpan(branchingSource, 41, 45),
                  generated: false,
                }),
              }),
            ]),
            terminator: Object.freeze({
              _tag: 'Jump' as const,
              target: block(3),
              provenance: Object.freeze({
                span: sampleSpan(branchingSource, 34, 45),
                generated: true,
              }),
            }),
          }),
          Object.freeze({
            _tag: 'MirBlock' as const,
            id: block(2),
            kind: 'Normal' as const,
            operations: Object.freeze([]),
            terminator: Object.freeze({
              _tag: 'Trap' as const,
              reason: 'unreachable flag state',
              provenance: Object.freeze({
                span: sampleSpan(branchingSource, 34, 45),
                generated: true,
              }),
            }),
          }),
          Object.freeze({
            _tag: 'MirBlock' as const,
            id: block(3),
            kind: 'Cleanup' as const,
            operations: Object.freeze([
              Object.freeze({
                _tag: 'Drop' as const,
                local: local(0),
                provenance: Object.freeze({
                  span: sampleSpan(branchingSource, 34, 45),
                  generated: true,
                }),
              }),
            ]),
            terminator: Object.freeze({
              _tag: 'Return' as const,
              value: local(1),
              provenance: Object.freeze({
                span: sampleSpan(branchingSource, 34, 45),
                generated: false,
              }),
            }),
          }),
        ]),
      }),
    ]),
  })

  return Object.freeze([straight, branching])
}
