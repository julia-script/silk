import * as Option from 'effect/Option'
import type * as DeclarationIndex from './DeclarationIndex.js'
import * as Layout from './Layout.js'
import * as SourceFile from './SourceFile.js'
import * as SourceSpan from './SourceSpan.js'
import * as Target from './Target.js'
import * as SilkType from './Type.js'

/**
 * MIR is the monomorphic, target-aware, backend-neutral structured control DAG. Structural child
 * and continuation references are acyclic. Loop repetition and exit are lexical outcomes rather
 * than graph edges; only a backend-private lowering may introduce a cyclic CFG.
 */

export type Type =
  | { readonly _tag: 'I32' }
  | { readonly _tag: 'Bool' }
  | { readonly _tag: 'Nominal'; readonly type: SilkType.Nominal }
  | { readonly _tag: 'FixedArray'; readonly type: SilkType.FixedArray }

export const semanticType = (self: Type): DeclarationIndex.SemanticType =>
  self._tag === 'Nominal' || self._tag === 'FixedArray' ? self.type : self._tag

const typeText = (self: Type): string => SilkType.encode(semanticType(self))
const isCopyType = (type: DeclarationIndex.SemanticType): boolean =>
  SilkType.isBuiltin(type) || (SilkType.isFixedArray(type) && isCopyType(type.element))

export interface LocalId {
  readonly _tag: 'Local'
  readonly ordinal: number
}

export interface RegionId {
  readonly _tag: 'Region'
  readonly ordinal: number
}

export interface LoopId {
  readonly _tag: 'Loop'
  readonly ordinal: number
}

export interface Provenance {
  readonly span: SourceSpan.SourceSpan
  readonly generated: boolean
}

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

export type PlaceSelector =
  | {
      readonly _tag: 'FieldSelector'
      readonly field: DeclarationIndex.FieldId
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'ElementSelector'
      readonly length: number
      readonly index:
        | { readonly _tag: 'Proven'; readonly value: number }
        | { readonly _tag: 'Runtime'; readonly local: LocalId }
      readonly provenance: Provenance
    }

export type Operation =
  | {
      readonly _tag: 'Literal'
      readonly destination: LocalId
      readonly type: Type
      readonly value: number
      readonly provenance: Provenance
    }
  | {
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
      readonly _tag: 'ConstructArray'
      readonly destination: LocalId
      readonly type: Extract<Type, { readonly _tag: 'FixedArray' }>
      readonly elements: ReadonlyArray<LocalId>
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
      readonly _tag: 'ReadPlace'
      readonly destination: LocalId
      readonly root: LocalId
      readonly selectors: ReadonlyArray<PlaceSelector>
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'CheckPlace'
      readonly root: LocalId
      readonly selectors: ReadonlyArray<PlaceSelector>
      readonly type: Type
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'WritePlace'
      readonly root: LocalId
      readonly selectors: ReadonlyArray<PlaceSelector>
      readonly source: LocalId
      readonly rootType: Type
      readonly type: Type
      readonly mutable: true
      readonly replacement: 'Copy' | 'Owned'
      readonly commit: 'AfterCleanup'
      readonly provenance: Provenance
    }
  | {
      readonly _tag: 'Drop'
      readonly local: LocalId
      readonly provenance: Provenance
    }

export type Outcome =
  | { readonly _tag: 'Forward'; readonly target: RegionId; readonly provenance: Provenance }
  | { readonly _tag: 'Return'; readonly value: LocalId; readonly provenance: Provenance }
  | { readonly _tag: 'Trap'; readonly reason: string; readonly provenance: Provenance }
  | { readonly _tag: 'Repeat'; readonly loop: LoopId; readonly provenance: Provenance }
  | { readonly _tag: 'Exit'; readonly loop: LoopId; readonly provenance: Provenance }
  | { readonly _tag: 'Yield'; readonly provenance: Provenance }

interface RegionBase {
  readonly id: RegionId
  readonly ownerLoop?: LoopId
}

export interface OperationRegion extends RegionBase {
  readonly _tag: 'OperationRegion'
  readonly operations: ReadonlyArray<Operation>
  readonly outcome: Outcome
}

export interface CleanupRegion extends RegionBase {
  readonly _tag: 'CleanupRegion'
  readonly releases: ReadonlyArray<Extract<Operation, { readonly _tag: 'Drop' }>>
  readonly outcome: Outcome
}

export interface ConditionalRegion extends RegionBase {
  readonly _tag: 'ConditionalRegion'
  readonly condition: LocalId
  readonly taken: RegionId
  readonly otherwise: RegionId
  readonly following?: RegionId
  readonly provenance: Provenance
}

export interface LoopRegion extends RegionBase {
  readonly _tag: 'LoopRegion'
  readonly loop: LoopId
  readonly parent?: LoopId
  readonly condition: RegionId
  readonly conditionValue: LocalId
  readonly body: RegionId
  readonly following: RegionId
  readonly provenance: Provenance
}

export type Region = OperationRegion | CleanupRegion | ConditionalRegion | LoopRegion

export interface MirFunction {
  readonly _tag: 'MirFunction'
  readonly id: DeclarationIndex.CanonicalId
  readonly parameterCount: number
  readonly localTypes: ReadonlyArray<Type>
  readonly result: Type
  readonly entry: RegionId
  readonly regions: ReadonlyArray<Region>
}

export interface Module {
  readonly _tag: 'MirModule'
  readonly module: string
  readonly layout: Layout.Plan
  readonly functions: ReadonlyArray<MirFunction>
}

export interface ControlEdge {
  readonly _tag: 'ControlEdge'
  readonly from: RegionId
  readonly to: RegionId
  readonly kind: 'Forward' | 'Taken' | 'Otherwise' | 'Following' | 'Condition' | 'Body'
}

const outcomeTarget = (
  outcome: Outcome,
): ReadonlyArray<readonly [RegionId, ControlEdge['kind']]> =>
  outcome._tag === 'Forward' ? [[outcome.target, 'Forward']] : []

const regionTargets = (region: Region): ReadonlyArray<readonly [RegionId, ControlEdge['kind']]> => {
  switch (region._tag) {
    case 'OperationRegion':
    case 'CleanupRegion':
      return outcomeTarget(region.outcome)
    case 'ConditionalRegion':
      return [
        [region.taken, 'Taken'],
        [region.otherwise, 'Otherwise'],
        ...(region.following === undefined ? [] : ([[region.following, 'Following']] as const)),
      ]
    case 'LoopRegion':
      return [
        [region.condition, 'Condition'],
        [region.body, 'Body'],
        [region.following, 'Following'],
      ]
  }
}

export const controlEdges = (self: MirFunction): ReadonlyArray<ControlEdge> =>
  Object.freeze(
    self.regions.flatMap((region) =>
      regionTargets(region).map(([to, kind]) =>
        Object.freeze({ _tag: 'ControlEdge' as const, from: region.id, to, kind }),
      ),
    ),
  )

/** Canonical parent-before-child traversal over structural edges only. */
export const topologicalRegions = (self: MirFunction): ReadonlyArray<Region> => {
  const byId = new Map(self.regions.map((region) => [region.id.ordinal, region] as const))
  const visited = new Set<number>()
  const ordered: Array<Region> = []
  const visit = (id: RegionId): void => {
    if (visited.has(id.ordinal)) return
    visited.add(id.ordinal)
    const region = byId.get(id.ordinal)
    if (region === undefined) return
    ordered.push(region)
    for (const [target] of regionTargets(region)) visit(target)
  }
  visit(self.entry)
  for (const region of [...self.regions].sort(
    (left, right) => left.id.ordinal - right.id.ordinal,
  )) {
    visit(region.id)
  }
  return Object.freeze(ordered)
}

export interface Violation {
  readonly _tag: 'Violation'
  readonly rule:
    | 'InvalidLayout'
    | 'MissingTypeLayout'
    | 'MissingEntryRegion'
    | 'DuplicateRegionIdentity'
    | 'UnknownRegionTarget'
    | 'StructuralCycle'
    | 'InvalidLexicalOwner'
    | 'InvalidLoopTarget'
    | 'UndeclaredLocal'
    | 'InvalidAggregateOperation'
    | 'InvalidCallShape'
    | 'InvalidWrite'
  readonly function?: DeclarationIndex.CanonicalId
  readonly region?: RegionId
  readonly detail: string
}

const operationsOf = (region: Region): ReadonlyArray<Operation> =>
  region._tag === 'OperationRegion'
    ? region.operations
    : region._tag === 'CleanupRegion'
      ? region.releases
      : []

/** Source-stable operations across canonical topological region order. */
export const operations = (self: MirFunction): ReadonlyArray<Operation> =>
  Object.freeze(topologicalRegions(self).flatMap(operationsOf))

export const outcomes = (self: MirFunction): ReadonlyArray<Outcome> =>
  Object.freeze(topologicalRegions(self).flatMap((region) => outcomeOf(region) ?? []))

const outcomeOf = (region: Region): Outcome | undefined =>
  region._tag === 'OperationRegion' || region._tag === 'CleanupRegion' ? region.outcome : undefined

const operationLocals = (operation: Operation): ReadonlyArray<LocalId> => {
  switch (operation._tag) {
    case 'Literal':
      return [operation.destination]
    case 'Binary':
      return [operation.destination, operation.left, operation.right]
    case 'Move':
      return [operation.destination, operation.source]
    case 'Call':
      return [operation.destination, ...operation.arguments]
    case 'Construct':
      return [operation.destination, ...operation.fields.map((field) => field.value)]
    case 'ConstructArray':
      return [operation.destination, ...operation.elements]
    case 'Project':
      return [operation.destination, operation.source]
    case 'ReadPlace':
      return [operation.destination, operation.root, ...selectorLocals(operation.selectors)]
    case 'CheckPlace':
      return [operation.root, ...selectorLocals(operation.selectors)]
    case 'WritePlace':
      return [operation.root, operation.source, ...selectorLocals(operation.selectors)]
    case 'Drop':
      return [operation.local]
  }
}

const localUses = (region: Region): ReadonlyArray<LocalId> => [
  ...operationsOf(region).flatMap(operationLocals),
  ...(region._tag === 'ConditionalRegion' ? [region.condition] : []),
  ...(region._tag === 'LoopRegion' ? [region.conditionValue] : []),
  ...(outcomeOf(region)?._tag === 'Return'
    ? [(outcomeOf(region) as Extract<Outcome, { readonly _tag: 'Return' }>).value]
    : []),
]

const selectorLocals = (selectors: ReadonlyArray<PlaceSelector>): ReadonlyArray<LocalId> =>
  selectors.flatMap((selector) =>
    selector._tag === 'ElementSelector' && selector.index._tag === 'Runtime'
      ? [selector.index.local]
      : [],
  )

const placeType = (
  fn: MirFunction,
  layout: Layout.Plan,
  root: LocalId,
  selectors: ReadonlyArray<PlaceSelector>,
): DeclarationIndex.SemanticType | undefined => {
  const rootType = fn.localTypes.at(root.ordinal)
  let current = rootType === undefined ? undefined : semanticType(rootType)
  for (const selector of selectors) {
    if (selector._tag === 'FieldSelector') {
      const entry =
        current !== undefined && SilkType.isNominal(current)
          ? Layout.entry(layout, current)
          : undefined
      const field =
        entry?.representation._tag === 'Aggregate'
          ? entry.representation.fields.find(
              (candidate) =>
                candidate.id.ordinal === selector.field.ordinal &&
                candidate.id.struct.sourceId === selector.field.struct.sourceId &&
                candidate.id.struct.ordinal === selector.field.struct.ordinal,
            )
          : undefined
      current = field?.type
      continue
    }
    if (
      current === undefined ||
      !SilkType.isFixedArray(current) ||
      current.length !== selector.length
    ) {
      return undefined
    }
    if (selector.index._tag === 'Proven') {
      if (selector.index.value < 0 || selector.index.value >= selector.length) return undefined
    } else if (fn.localTypes.at(selector.index.local.ordinal)?._tag !== 'I32') return undefined
    current = current.element
  }
  return current
}

const targetText = (target: DeclarationIndex.CanonicalId): string =>
  `${target.module}.${target.name}`

export const verify = (self: Module): ReadonlyArray<Violation> => {
  const violations: Array<Violation> = Layout.verify(self.layout).map((violation) =>
    Object.freeze({
      _tag: 'Violation' as const,
      rule: 'InvalidLayout' as const,
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

    const byId = new Map<number, Region>()
    for (const region of fn.regions) {
      if (byId.has(region.id.ordinal)) {
        violations.push(
          Object.freeze({
            _tag: 'Violation',
            rule: 'DuplicateRegionIdentity',
            function: fn.id,
            region: region.id,
            detail: `region r${region.id.ordinal} is declared more than once`,
          }),
        )
      } else byId.set(region.id.ordinal, region)
    }
    if (!byId.has(fn.entry.ordinal)) {
      violations.push(
        Object.freeze({
          _tag: 'Violation',
          rule: 'MissingEntryRegion',
          function: fn.id,
          detail: `entry region r${fn.entry.ordinal} is missing`,
        }),
      )
    }
    for (const region of fn.regions) {
      for (const [target] of regionTargets(region)) {
        if (!byId.has(target.ordinal)) {
          violations.push(
            Object.freeze({
              _tag: 'Violation',
              rule: 'UnknownRegionTarget',
              function: fn.id,
              region: region.id,
              detail: `region references missing r${target.ordinal}`,
            }),
          )
        }
      }
    }

    const color = new Map<number, 0 | 1 | 2>()
    const visit = (region: Region): void => {
      color.set(region.id.ordinal, 1)
      for (const [target] of regionTargets(region)) {
        const targetRegion = byId.get(target.ordinal)
        if (targetRegion === undefined) continue
        if (color.get(target.ordinal) === 1) {
          violations.push(
            Object.freeze({
              _tag: 'Violation',
              rule: 'StructuralCycle',
              function: fn.id,
              region: region.id,
              detail: `structural edge r${region.id.ordinal} -> r${target.ordinal} forms a cycle`,
            }),
          )
        } else if (color.get(target.ordinal) !== 2) visit(targetRegion)
      }
      color.set(region.id.ordinal, 2)
    }
    for (const region of [...fn.regions].sort((a, b) => a.id.ordinal - b.id.ordinal)) {
      if (color.get(region.id.ordinal) === undefined) visit(region)
    }

    const loops = new Map<number, LoopRegion>()
    for (const region of fn.regions) {
      if (region._tag === 'LoopRegion') loops.set(region.loop.ordinal, region)
    }
    const isAncestor = (owner: LoopId | undefined, target: LoopId): boolean => {
      let current = owner
      const seen = new Set<number>()
      while (current !== undefined && !seen.has(current.ordinal)) {
        if (current.ordinal === target.ordinal) return true
        seen.add(current.ordinal)
        current = loops.get(current.ordinal)?.parent
      }
      return false
    }
    for (const region of fn.regions) {
      if (region.ownerLoop !== undefined && !loops.has(region.ownerLoop.ordinal)) {
        violations.push(
          Object.freeze({
            _tag: 'Violation',
            rule: 'InvalidLexicalOwner',
            function: fn.id,
            region: region.id,
            detail: `owner loop loop${region.ownerLoop.ordinal} is missing`,
          }),
        )
      }
      const outcome = outcomeOf(region)
      if (
        (outcome?._tag === 'Repeat' || outcome?._tag === 'Exit') &&
        !isAncestor(region.ownerLoop, outcome.loop)
      ) {
        violations.push(
          Object.freeze({
            _tag: 'Violation',
            rule: 'InvalidLoopTarget',
            function: fn.id,
            region: region.id,
            detail: `${outcome._tag.toLowerCase()} targets non-ancestor loop${outcome.loop.ordinal}`,
          }),
        )
      }
      for (const used of localUses(region)) {
        if (used.ordinal < 0 || used.ordinal >= fn.localTypes.length) {
          violations.push(
            Object.freeze({
              _tag: 'Violation',
              rule: 'UndeclaredLocal',
              function: fn.id,
              region: region.id,
              detail: `references undeclared local %${used.ordinal}`,
            }),
          )
        }
      }
      const operations = operationsOf(region)
      for (const [index, operation] of operations.entries()) {
        if (operation._tag === 'Construct') {
          const layout = Layout.entry(self.layout, operation.type.type)
          const expected =
            layout?.representation._tag === 'Aggregate' ? layout.representation.fields : []
          const valid =
            expected.length === operation.fields.length &&
            operation.fields.every((field, ordinal) => {
              const declared = expected.at(ordinal)
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
                region: region.id,
                detail: `construction of ${typeText(operation.type)} does not match its canonical fields`,
              }),
            )
          }
        }
        if (operation._tag === 'ConstructArray') {
          const semantic = operation.type.type
          const destination = fn.localTypes.at(operation.destination.ordinal)
          const valid =
            operation.elements.length === semantic.length &&
            destination !== undefined &&
            SilkType.equals(semanticType(destination), semantic) &&
            operation.elements.every((element) => {
              const elementType = fn.localTypes.at(element.ordinal)
              return (
                elementType !== undefined &&
                SilkType.equals(semanticType(elementType), semantic.element)
              )
            })
          if (!valid) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidAggregateOperation',
                function: fn.id,
                region: region.id,
                detail: `construction of ${typeText(operation.type)} does not match its canonical element count or type`,
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
                region: region.id,
                detail: `projection field #${operation.field.ordinal} does not match its source type`,
              }),
            )
          }
        }
        if (operation._tag === 'ReadPlace' || operation._tag === 'CheckPlace') {
          const selected = placeType(fn, self.layout, operation.root, operation.selectors)
          if (
            selected === undefined ||
            !SilkType.equals(selected, semanticType(operation.type)) ||
            (operation._tag === 'ReadPlace' && !isCopyType(selected))
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidAggregateOperation',
                function: fn.id,
                region: region.id,
                detail: `${operation._tag} does not match its root, selectors, or type`,
              }),
            )
          }
        }
        if (operation._tag === 'WritePlace') {
          const selected = placeType(fn, self.layout, operation.root, operation.selectors)
          const source = fn.localTypes.at(operation.source.ordinal)
          const root = fn.localTypes.at(operation.root.ordinal)
          const checked = operations
            .slice(0, index)
            .some(
              (candidate) =>
                candidate._tag === 'CheckPlace' &&
                candidate.root.ordinal === operation.root.ordinal &&
                candidate.selectors === operation.selectors,
            )
          if (
            selected === undefined ||
            source === undefined ||
            root === undefined ||
            !checked ||
            !SilkType.equals(selected, semanticType(operation.type)) ||
            !SilkType.equals(semanticType(source), selected) ||
            !SilkType.equals(semanticType(root), semanticType(operation.rootType))
          ) {
            violations.push(
              Object.freeze({
                _tag: 'Violation',
                rule: 'InvalidWrite',
                function: fn.id,
                region: region.id,
                detail:
                  'write lacks a matching precheck or has inconsistent root/source/place types',
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
            operation.arguments.every((argument, ordinal) => {
              const actual = fn.localTypes.at(argument.ordinal)
              const expected = target?.localTypes.at(ordinal)
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
                region: region.id,
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
const regionText = (region: RegionId): string => `r${region.ordinal}`
const loopText = (loop: LoopId): string => `loop${loop.ordinal}`
const selectorText = (selectors: ReadonlyArray<PlaceSelector>): string =>
  selectors
    .map((selector) =>
      selector._tag === 'FieldSelector'
        ? `.#${selector.field.ordinal}`
        : `[${selector.index._tag === 'Proven' ? selector.index.value : localText(selector.index.local)}/${selector.length}]`,
    )
    .join('')

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
    case 'ConstructArray':
      return `${localText(operation.destination)} = construct-array ${typeText(operation.type)} [${operation.elements.map(localText).join(', ')}] ${provenanceText(operation.provenance)}`
    case 'Project':
      return `${localText(operation.destination)} = project ${localText(operation.source)}.#${operation.field.ordinal} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'ReadPlace':
      return `${localText(operation.destination)} = read-place ${localText(operation.root)}${selectorText(operation.selectors)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'CheckPlace':
      return `check-place ${localText(operation.root)}${selectorText(operation.selectors)} : ${typeText(operation.type)} ${provenanceText(operation.provenance)}`
    case 'WritePlace':
      return `write-place ${localText(operation.root)}${selectorText(operation.selectors)} <- ${localText(operation.source)} : ${typeText(operation.type)} replacement=${operation.replacement} commit=${operation.commit} ${provenanceText(operation.provenance)}`
    case 'Drop':
      return `drop ${localText(operation.local)} ${provenanceText(operation.provenance)}`
  }
}

const outcomeText = (outcome: Outcome): string => {
  switch (outcome._tag) {
    case 'Forward':
      return `forward ${regionText(outcome.target)} ${provenanceText(outcome.provenance)}`
    case 'Return':
      return `return ${localText(outcome.value)} ${provenanceText(outcome.provenance)}`
    case 'Trap':
      return `trap "${outcome.reason}" ${provenanceText(outcome.provenance)}`
    case 'Repeat':
      return `repeat ${loopText(outcome.loop)} ${provenanceText(outcome.provenance)}`
    case 'Exit':
      return `exit ${loopText(outcome.loop)} ${provenanceText(outcome.provenance)}`
    case 'Yield':
      return `yield ${provenanceText(outcome.provenance)}`
  }
}

const regionLines = (region: Region): ReadonlyArray<string> => {
  const owner = region.ownerLoop === undefined ? '' : ` owner=${loopText(region.ownerLoop)}`
  switch (region._tag) {
    case 'OperationRegion':
      return [
        `  ${regionText(region.id)} operation${owner}:`,
        ...region.operations.map((operation) => `    ${operationText(operation)}`),
        `    ${outcomeText(region.outcome)}`,
      ]
    case 'CleanupRegion':
      return [
        `  ${regionText(region.id)} cleanup${owner}:`,
        ...region.releases.map((release) => `    ${operationText(release)}`),
        `    ${outcomeText(region.outcome)}`,
      ]
    case 'ConditionalRegion':
      return [
        `  ${regionText(region.id)} conditional${owner} condition=${localText(region.condition)} taken=${regionText(region.taken)} otherwise=${regionText(region.otherwise)}${region.following === undefined ? '' : ` following=${regionText(region.following)}`} ${provenanceText(region.provenance)}`,
      ]
    case 'LoopRegion':
      return [
        `  ${regionText(region.id)} loop ${loopText(region.loop)}${region.parent === undefined ? '' : ` parent=${loopText(region.parent)}`} condition=${regionText(region.condition)} value=${localText(region.conditionValue)} body=${regionText(region.body)} following=${regionText(region.following)} ${provenanceText(region.provenance)}`,
      ]
  }
}

export const encode = (self: Module): string =>
  [
    `mir-module ${self.module}`,
    ...Layout.encode(self.layout).trimEnd().split('\n'),
    ...self.functions.flatMap((fn) => [
      `fn ${targetText(fn.id)} params=${fn.parameterCount} locals=${fn.localTypes.length} -> ${typeText(fn.result)} entry=${regionText(fn.entry)}`,
      ...topologicalRegions(fn).flatMap(regionLines),
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
const region = (ordinal: number): RegionId => Object.freeze({ _tag: 'Region', ordinal })
const i32: Type = Object.freeze({ _tag: 'I32' })
const bool: Type = Object.freeze({ _tag: 'Bool' })
const canonical = (module: string, name: string): DeclarationIndex.CanonicalId =>
  Object.freeze({ _tag: 'CanonicalDeclarationId', module, name })

export const samples = (): ReadonlyArray<Module> => {
  const source = SourceFile.make(
    'sample://regions.silk',
    Uint8Array.from('pub fn answer() -> I32 { return 42 }', (char) => char.charCodeAt(0)),
  )
  const provenance = (start: number, end: number, generated = false): Provenance =>
    Object.freeze({ span: sampleSpan(source, start, end), generated })
  const straight: Module = Object.freeze({
    _tag: 'MirModule',
    module: source.id,
    layout: Layout.make(Target.aarch64AppleDarwin, ['I32']),
    functions: Object.freeze([
      Object.freeze({
        _tag: 'MirFunction' as const,
        id: canonical(source.id, 'answer'),
        parameterCount: 0,
        localTypes: Object.freeze([i32]),
        result: i32,
        entry: region(0),
        regions: Object.freeze([
          Object.freeze({
            _tag: 'OperationRegion' as const,
            id: region(0),
            operations: Object.freeze([
              Object.freeze({
                _tag: 'Literal' as const,
                destination: local(0),
                type: i32,
                value: 42,
                provenance: provenance(32, 34),
              }),
            ]),
            outcome: Object.freeze({
              _tag: 'Return' as const,
              value: local(0),
              provenance: provenance(25, 34),
            }),
          }),
        ]),
      }),
    ]),
  })
  const conditional: Module = Object.freeze({
    _tag: 'MirModule',
    module: source.id,
    layout: Layout.make(Target.aarch64AppleDarwin, ['I32', 'Bool']),
    functions: Object.freeze([
      Object.freeze({
        _tag: 'MirFunction' as const,
        id: canonical(source.id, 'choose'),
        parameterCount: 1,
        localTypes: Object.freeze([bool, i32]),
        result: i32,
        entry: region(0),
        regions: Object.freeze([
          Object.freeze({
            _tag: 'ConditionalRegion' as const,
            id: region(0),
            condition: local(0),
            taken: region(1),
            otherwise: region(2),
            provenance: provenance(25, 34),
          }),
          Object.freeze({
            _tag: 'OperationRegion' as const,
            id: region(1),
            operations: Object.freeze([
              Object.freeze({
                _tag: 'Literal' as const,
                destination: local(1),
                type: i32,
                value: 1,
                provenance: provenance(32, 33),
              }),
            ]),
            outcome: Object.freeze({
              _tag: 'Return' as const,
              value: local(1),
              provenance: provenance(25, 34),
            }),
          }),
          Object.freeze({
            _tag: 'OperationRegion' as const,
            id: region(2),
            operations: Object.freeze([]),
            outcome: Object.freeze({
              _tag: 'Trap' as const,
              reason: 'otherwise',
              provenance: provenance(25, 34, true),
            }),
          }),
        ]),
      }),
    ]),
  })
  return Object.freeze([straight, conditional])
}
