import * as Option from 'effect/Option'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Layout from './Layout.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import * as Target from './Target.js'
import * as SilkType from './Type.js'

/**
 * MIR: the monomorphic, backend-neutral basic-block control-flow graph over logical Silk types.
 * It contains no LLVM types, instructions, intrinsics, attributes, metadata, or physical field
 * offsets, and adopts no WebAssembly control shapes. It carries the compiler-owned concrete
 * target and layout plan that every consumer must realize.
 */

/** A logical Silk type at the MIR level. */
export type Type =
  | { readonly _tag: 'I32' }
  | { readonly _tag: 'Bool' }
  | { readonly _tag: 'Nominal'; readonly type: SilkType.Nominal }

/** Converts a MIR logical type to the shared semantic type vocabulary. */
export const semanticType = (self: Type): DeclarationIndex.SemanticType =>
  self._tag === 'Nominal' ? self.type : self._tag

const typeText = (self: Type): string => SilkType.encode(semanticType(self))

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

/** The closed binary operator vocabulary: trapping arithmetic and non-trapping comparisons. */
export type BinaryOperator =
  | 'Add'
  | 'Subtract'
  | 'Multiply'
  | 'Divide'
  | 'Remainder'
  | 'Equals'
  | 'NotEquals'
  | 'LessThan'
  | 'LessOrEqual'
  | 'GreaterThan'
  | 'GreaterOrEqual'

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
      /**
       * Trapping arithmetic: signed overflow, division by zero, and MIN divided or remaindered
       * by -1 abort the function like an explicit trap. Division truncates toward zero and
       * remainder takes the dividend's sign.
       */
      readonly _tag: 'Binary'
      readonly operator: BinaryOperator
      readonly destination: LocalId
      readonly left: LocalId
      readonly right: LocalId
      readonly type: Type
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
      readonly _tag: 'Construct'
      readonly destination: LocalId
      readonly type: Extract<Type, { readonly _tag: 'Nominal' }>
      readonly fields: ReadonlyArray<{
        readonly field: DeclarationIndex.FieldId
        readonly value: LocalId
      }>
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'Project'
      readonly destination: LocalId
      readonly source: LocalId
      readonly field: DeclarationIndex.FieldId
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
  readonly layout: Layout.Plan
  readonly functions: ReadonlyArray<MirFunction>
}

/** One structural invariant violation, reported as data. */
export interface Violation {
  readonly _tag: 'Violation'
  readonly rule:
    | 'InvalidLayout'
    | 'MissingTypeLayout'
    | 'MissingEntryBlock'
    | 'UnknownBlockTarget'
    | 'UndeclaredLocal'
    | 'InvalidAggregateOperation'
    | 'InvalidCallShape'
  readonly function?: DeclarationIndex.CanonicalId
  readonly block?: BlockId
  readonly detail: string
}

const localUses = (block: Block): ReadonlyArray<LocalId> => [
  ...block.operations.flatMap((operation): ReadonlyArray<LocalId> => {
    if (operation._tag === 'Literal') return [operation.destination]
    if (operation._tag === 'Binary') {
      return [operation.destination, operation.left, operation.right]
    }
    if (operation._tag === 'Move') return [operation.destination, operation.source]
    if (operation._tag === 'Call') return [operation.destination, ...operation.arguments]
    if (operation._tag === 'Construct') {
      return [operation.destination, ...operation.fields.map((field) => field.value)]
    }
    if (operation._tag === 'Project') {
      return [operation.destination, operation.source]
    }
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
  const violations: Array<Violation> = Layout.verify(self.layout).map((violation) =>
    Object.freeze({
      _tag: 'Violation',
      rule: 'InvalidLayout',
      detail: `${violation.rule}: ${violation.detail}`,
    }),
  )
  for (const fn of self.functions) {
    const missingTypes = new Set(
      [...fn.localTypes, fn.result]
        .map(semanticType)
        .filter((type) => Layout.entry(self.layout, type) === undefined)
        .map(SilkType.key),
    )
    for (const type of [...missingTypes].sort()) {
      violations.push(
        Object.freeze({
          _tag: 'Violation',
          rule: 'MissingTypeLayout',
          function: fn.id,
          detail: `function references ${type} without a layout entry`,
        }),
      )
    }
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
      for (const operation of block.operations) {
        if (operation._tag === 'Construct') {
          const layout = Layout.entry(self.layout, operation.type.type)
          const expected =
            layout?.representation._tag === 'Aggregate' ? layout.representation.fields : []
          const valid =
            expected.length === operation.fields.length &&
            operation.fields.every((field, index) => {
              const declared = expected.at(index)
              const valueType = fn.localTypes.at(field.value.ordinal)
              return (
                declared !== undefined &&
                declared.id.ordinal === field.field.ordinal &&
                valueType !== undefined &&
                SilkType.equals(semanticType(valueType), declared.type)
              )
            })
          if (!valid) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidAggregateOperation',
                function: fn.id,
                block: block.id,
                detail: `construction of ${typeText(operation.type)} does not match its canonical fields`,
              }),
            )
          }
        }
        if (operation._tag === 'Project') {
          const sourceType = fn.localTypes.at(operation.source.ordinal)
          const sourceLayout =
            sourceType?._tag === 'Nominal' ? Layout.entry(self.layout, sourceType.type) : undefined
          const field =
            sourceLayout?.representation._tag === 'Aggregate'
              ? sourceLayout.representation.fields.find(
                  (candidate) => candidate.id.ordinal === operation.field.ordinal,
                )
              : undefined
          if (field === undefined || !SilkType.equals(field.type, semanticType(operation.type))) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidAggregateOperation',
                function: fn.id,
                block: block.id,
                detail: `projection field #${operation.field.ordinal} does not match its source type`,
              }),
            )
          }
        }
        if (operation._tag === 'Call') {
          const target = self.functions.find(
            (candidate) =>
              candidate.id.module === operation.target.module &&
              candidate.id.name === operation.target.name,
          )
          const valid =
            target !== undefined &&
            target.parameterCount === operation.arguments.length &&
            operation.arguments.every((argument, index) => {
              const actual = fn.localTypes.at(argument.ordinal)
              const expected = target.localTypes.at(index)
              return (
                actual !== undefined &&
                expected !== undefined &&
                SilkType.equals(semanticType(actual), semanticType(expected))
              )
            }) &&
            SilkType.equals(semanticType(operation.type), semanticType(target.result))
          if (!valid) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidCallShape',
                function: fn.id,
                block: block.id,
                detail: `call ${targetText(operation.target)} does not match its logical contract`,
              }),
            )
          }
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
      return `${localText(operation.destination)} = literal ${operation.value} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'Binary':
      return `${localText(operation.destination)} = ${operation.operator.toLowerCase()} ${localText(operation.left)}, ${localText(operation.right)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'Move':
      return `${localText(operation.destination)} = move ${localText(operation.source)} ${provenanceText(operation.provenance)}`
    case 'Call':
      return `${localText(operation.destination)} = call ${targetText(operation.target)}(${operation.arguments.map(localText).join(', ')}) : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'Construct':
      return `${localText(operation.destination)} = construct ${typeText(operation.type)} { ${operation.fields.map(({ field, value }) => `#${field.ordinal}: ${localText(value)}`).join(', ')} } ${provenanceText(operation.provenance)}`
    case 'Project':
      return `${localText(operation.destination)} = project ${localText(operation.source)}.#${operation.field.ordinal} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
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
 * The compiler-selected target and canonical layout lead the encoding; no compatibility promise
 * attaches to this inspection format.
 */
export const encode = (self: Module): string =>
  [
    `mir-module ${self.module}`,
    ...Layout.encode(self.layout).trimEnd().split('\n'),
    ...self.functions.flatMap((fn) => [
      `fn ${targetText(fn.id)} params=${fn.parameterCount} locals=${fn.localTypes.length} -> ${typeText(fn.result)}`,
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
    layout: Layout.make(Target.aarch64AppleDarwin, ['I32']),
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
    layout: Layout.make(Target.aarch64AppleDarwin, ['I32']),
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
