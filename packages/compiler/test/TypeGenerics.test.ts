import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as Backend from '../src/Backend.js'
import * as BootstrapEvaluation from '../src/BootstrapEvaluation.js'
import * as DeclarationIndex from '../src/DeclarationIndex.js'
import * as Diagnostic from '../src/Diagnostic.js'
import * as FormattedDocument from '../src/FormattedDocument.js'
import * as Instances from '../src/Instances.js'
import * as Layout from '../src/Layout.js'
import * as Lexer from '../src/Lexer.js'
import * as Mir from '../src/Mir.js'
import * as ModuleSurface from '../src/ModuleSurface.js'
import * as OwnershipEncoding from '../src/OwnershipEncoding.js'
import * as Parser from '../src/Parser.js'
import * as RowAlgebra from '../src/RowAlgebra.js'
import * as SourceFile from '../src/SourceFile.js'
import * as SourceResolver from '../src/SourceResolver.js'
import * as SourceSpan from '../src/SourceSpan.js'
import * as SyntaxFormatter from '../src/SyntaxFormatter.js'
import * as SyntaxTree from '../src/SyntaxTree.js'
import * as Type from '../src/Type.js'
import * as Json from './support/Json.js'
import * as Projections from './support/projections.js'
import { unreachable } from './support/raise.js'

const source = `fn identity<T>(value: T) -> T { return move value }
pub fn main() -> i32 {
  let flag = identity(true)
  return identity<i32>(42)
}`

const file = SourceFile.make('generics/Main', new TextEncoder().encode(source))

const descendants = (node: SyntaxTree.Node): ReadonlyArray<SyntaxTree.Node> =>
  node.children.flatMap(
    (child): ReadonlyArray<SyntaxTree.Node> =>
      SyntaxTree.isNode(child) ? [child, ...descendants(child)] : [],
  )

it.effect('retains source-shaped row expressions and callable constraints in module facts', () =>
  Effect.gen(function* () {
    const constrained = `service Binder {
  effect fn bind<?S, A, P, E, ?R>(self: once Effect<A ! E ? R>, provider: &mut P) -> A
  ! E
  ? Without<R, S>
  where &mut P provides S from R, S in R
}
pub fn main() -> i32 { return 0 }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/constraint-facts',
      new TextEncoder().encode(constrained),
    )
    const index = Analysis.declarationIndex(snapshot)
    const operation = index.modules.at(0)?.services.at(0)?.operations.at(0)

    assert.isDefined(operation)
    if (operation === undefined) return
    assert.strictEqual(operation.requirementRow.expression._tag, 'WithoutRowExpression')
    assert.deepEqual(
      operation.constraints.map((constraint) => constraint._tag),
      ['ProviderConstraint', 'MembershipConstraint'],
    )
    assert.deepEqual(
      operation.constraintContracts.map((constraint) => constraint._tag),
      ['ProviderSelectionConstraint', 'RequirementSubsetConstraint'],
    )
    assert.strictEqual(
      RowAlgebra.encode(
        Type.requirementRowPolicy(),
        operation.requirementRow.row,
        (member) => `${member.access}:${Type.encode(member.capability)}@${member.role}`,
        Type.encode,
        (member) => `${member.access}:${member.capability.name}@${member.role}`,
      ),
      'Without<R, S>',
    )
    const provider = operation.constraints.at(0)
    assert.strictEqual(provider?._tag, 'ProviderConstraint')
    if (provider?._tag === 'ProviderConstraint') {
      assert.strictEqual(provider.mode, 'Exclusive')
      assert.strictEqual(provider.selected._tag, 'RowParameterExpression')
      assert.strictEqual(provider.source._tag, 'RowParameterExpression')
    }
    const contract = DeclarationIndex.callableContract(operation)
    assert.strictEqual(contract.constraints.length, 2)
    assert.strictEqual(Type.isEffect(contract.result), true)
    assert.include(Type.encode(contract.result), 'Without<R, S>')
    const surface = ModuleSurface.fromIndex(index).get('generics/constraint-facts')
    assert.isDefined(surface)
    assert.include(surface?.canonical ?? '', 'ProviderConstraint')
    assert.include(surface?.canonical ?? '', 'WithoutRowExpression')
  }),
)

it.effect('rejects residual rows at the complete-application specialization frontier', () =>
  Effect.gen(function* () {
    const module = 'generics/frontier'
    const snapshot = yield* Analysis.ofSourceRealized(
      module,
      new TextEncoder().encode(`effect fn forward<A, E, ?R>(self: once Effect<A ! E ? R>) -> A ! E ? R {
  return run self
}
pub fn main() -> i32 { return 0 }`),
    )
    const fn = Projections.hirOf(snapshot, module)?.functions.find(
      (candidate) =>
        candidate.declaration.canonical._tag === 'Canonical' &&
        candidate.declaration.canonical.id.name === 'forward',
    )
    assert.isDefined(fn)
    if (fn === undefined) return
    assert.isUndefined(Instances.specialize(fn, new Map(), Analysis.declarationIndex(snapshot)))
    const diagnostic = Diagnostic.nonConcreteSpecialization(
      `${module}.forward`,
      fn.declaration.syntax.span,
    )
    assert.strictEqual(diagnostic.code, 'SEM0122')
    assert.strictEqual(diagnostic.reason._tag, 'NonConcreteSpecialization')
  }),
)

it.effect('renormalizes concrete difference after generic nominal keys collide', () =>
  Effect.gen(function* () {
    const self = yield* Analysis.ofSource(
      'generics/without-substitution-collision',
      new TextEncoder().encode(`struct Problem<T> { value: T }
effect fn erase<A, B>() -> () ! Without<Problem<A>, Problem<B>> { return () }
pub effect fn main() -> () { return run erase<i32, i32>() }`),
    )
    assert.deepEqual(Analysis.diagnostics(self), [])
  }),
)

it('parses declaration parameters and explicit call specialization losslessly', () => {
  const syntax = Parser.parse(Lexer.lex(file))
  const kinds = descendants(syntax.root).map((node) => node.kind)

  assert.include(kinds, 'TypeParameterList')
  assert.include(kinds, 'TypeParameter')
  assert.include(kinds, 'CallTypeArgumentList')
  assert.deepEqual(syntax.parserDiagnostics, [])
})

it('parses channel-kinded generic binders losslessly', () => {
  const channelSource = `effect fn transform<A, E, ?R>(self: Effect<A ! E ? R>) -> Effect<A ! E ? R> {
  return self
}`
  const syntax = Parser.parse(
    Lexer.lex(SourceFile.make('generics/Channels', new TextEncoder().encode(channelSource))),
  )
  const parameters = descendants(syntax.root).filter((node) => node.kind === 'TypeParameter')

  assert.deepEqual(
    parameters.map((parameter) =>
      SyntaxTree.tokens(parameter)
        .filter((token) => token.kind !== 'Whitespace')
        .map((token) => token.kind),
    ),
    [['Identifier'], ['Identifier'], ['Question', 'Identifier']],
  )
  assert.deepEqual(syntax.parserDiagnostics, [])
})

it('normalizes contract rows and infers selected-entry remainders', () => {
  const owner = { module: 'generics/Rows', name: 'transform' }
  const failureRemainder = Type.parameter(owner, 0, 'E')
  const requirementRemainder = Type.parameter(owner, 1, 'R', 'RequirementRow')
  const problem = Type.nominal('generics/Rows', 'Problem')
  const other = Type.nominal('generics/Rows', 'Other')
  const clock = Type.nominal('generics/Rows', 'Clock')
  const allocator = Type.nominal('generics/Rows', 'Allocator')
  const pattern = Type.effect(
    'i32',
    [failureRemainder],
    'Shared',
    [{ capability: clock, role: 'Primary', access: 'Shared' }],
    [requirementRemainder],
  )
  const actual = Type.effect('i32', [other, problem, other], 'Shared', [
    { capability: clock, role: 'Primary', access: 'Shared' },
    { capability: allocator, role: 'DefaultRole', access: 'Exclusive' },
  ])
  const inferred = new Map<string, Type.GenericArgument>()

  assert.strictEqual(Type.infer(pattern, actual, inferred), true)
  assert.strictEqual(
    Type.encodeGenericArgument(inferred.get(Type.key(failureRemainder)) ?? 'never'),
    'generics/Rows.Other | generics/Rows.Problem',
  )
  assert.strictEqual(
    Type.encodeGenericArgument(inferred.get(Type.key(requirementRemainder)) ?? 'never'),
    '? &mut generics/Rows.Allocator',
  )
  assert.strictEqual(Type.encode(Type.substitute(pattern, inferred)), Type.encode(actual))
})

it('checks computed rows forward-only without reconstructing their operands', () => {
  const owner = { module: 'generics/ForwardRows', name: 'without' }
  const source = Type.parameter(owner, 0, 'E')
  const selected = Type.parameter(owner, 1, 'S')
  const problem = Type.nominal('generics/ForwardRows', 'Problem')
  const other = Type.nominal('generics/ForwardRows', 'Other')
  const origin =
    SourceSpan.fromOffsets('generics/ForwardRows', 10, 11) ??
    unreachable('expected a valid source span')
  const computed = RowAlgebra.without(
    Type.failureRowPolicy(),
    RowAlgebra.singleton(Type.failureRowPolicy(), Type.failureMemberShape(source), origin),
    RowAlgebra.singleton(Type.failureRowPolicy(), Type.failureMemberShape(selected), origin),
  )
  const independentlyBound = new Map<string, Type.GenericArgument>([
    [Type.key(source), Type.failureValue([problem, other])],
    [Type.key(selected), problem],
  ])
  assert.strictEqual(
    Type.encode(Type.failureType(Type.substituteFailureRow(computed, independentlyBound))),
    'generics/ForwardRows.Other',
  )
})

it('infers failure and requirement row arguments nested in nominal applications', () => {
  const owner = { module: 'generics/NominalRows', name: 'Carrier' }
  const failures = Type.parameter(owner, 0, 'E')
  const requirements = Type.parameter(owner, 1, 'R', 'RequirementRow')
  const problem = Type.nominal('generics/NominalRows', 'Problem')
  const other = Type.nominal('generics/NominalRows', 'Other')
  const clock = Type.nominal('generics/NominalRows', 'Clock')
  const allocator = Type.nominal('generics/NominalRows', 'Allocator')
  const pattern = Type.nominal('generics/NominalRows', 'Carrier', [
    failures,
    Type.requirementRowArgument(
      [{ capability: clock, role: 'DefaultRole', access: 'Shared' }],
      [requirements],
    ),
  ])
  const actual = Type.nominal('generics/NominalRows', 'Carrier', [
    Type.failureValue([problem, other]),
    Type.requirementRowArgument([
      { capability: clock, role: 'DefaultRole', access: 'Shared' },
      { capability: allocator, role: 'DefaultRole', access: 'Exclusive' },
    ]),
  ])
  const inferred = new Map<string, Type.GenericArgument>()

  assert.isTrue(Type.infer(pattern, actual, inferred))
  assert.strictEqual(
    Type.encodeGenericArgument(inferred.get(Type.key(failures)) ?? 'never'),
    'generics/NominalRows.Other | generics/NominalRows.Problem',
  )
  assert.strictEqual(
    Type.encodeGenericArgument(inferred.get(Type.key(requirements)) ?? 'never'),
    '? &mut generics/NominalRows.Allocator',
  )

  const repeated = Type.nominal('generics/NominalRows', 'Repeated', [failures, failures])
  assert.isFalse(
    Type.infer(
      repeated,
      Type.nominal('generics/NominalRows', 'Repeated', [
        Type.failureValue([problem]),
        Type.failureValue([other]),
      ]),
      new Map(),
    ),
  )

  const repeatedRequirements = Type.nominal('generics/NominalRows', 'Repeated', [
    Type.requirementRowArgument([], [requirements]),
    Type.requirementRowArgument([], [requirements]),
  ])
  assert.isFalse(
    Type.infer(
      repeatedRequirements,
      Type.nominal('generics/NominalRows', 'Repeated', [
        Type.requirementRowArgument([{ capability: clock, role: 'DefaultRole', access: 'Shared' }]),
        Type.requirementRowArgument([
          { capability: allocator, role: 'DefaultRole', access: 'Exclusive' },
        ]),
      ]),
      new Map(),
    ),
  )

  const openFailures = Type.parameter(owner, 2, 'OpenE')
  const openRequirements = Type.parameter(owner, 3, 'OpenR', 'RequirementRow')
  assert.isFalse(
    Type.infer(
      pattern,
      Type.nominal('generics/NominalRows', 'Carrier', [
        Type.failureValue([problem, openFailures]),
        Type.requirementRowArgument(
          [{ capability: clock, role: 'DefaultRole', access: 'Shared' }],
          [openRequirements],
        ),
      ]),
      new Map(),
    ),
  )

  const x = Type.parameter(owner, 4, 'X', 'Value')
  const y = Type.parameter(owner, 5, 'Y', 'Value')
  const a = Type.nominal('generics/NominalRows', 'A')
  const b = Type.nominal('generics/NominalRows', 'B')
  const c = Type.nominal('generics/NominalRows', 'C')
  const pair = (left: Type.Type, right: Type.Type): Type.Nominal =>
    Type.nominal('generics/NominalRows', 'Pair', [left, right])
  const requirement = (capability: Type.Nominal): Type.Requirement =>
    Object.freeze({ capability, role: 'DefaultRole', access: 'Shared' })
  const ambiguousRequirements = new Map<string, Type.GenericArgument>()
  assert.isTrue(
    Type.infer(
      Type.nominal('generics/NominalRows', 'Backtracking', [
        Type.requirementRowArgument([requirement(pair(a, y)), requirement(pair(x, b))]),
      ]),
      Type.nominal('generics/NominalRows', 'Backtracking', [
        Type.requirementRowArgument([requirement(pair(a, b)), requirement(pair(a, c))]),
      ]),
      ambiguousRequirements,
    ),
  )
  assert.strictEqual(
    Type.encodeGenericArgument(ambiguousRequirements.get(Type.key(x)) ?? 'never'),
    Type.encodeGenericArgument(a),
  )
  assert.strictEqual(
    Type.encodeGenericArgument(ambiguousRequirements.get(Type.key(y)) ?? 'never'),
    Type.encodeGenericArgument(c),
  )
})

it('uses an ordinary type parameter directly as an effect failure value', () => {
  const owner = { module: 'generics/Rows', name: 'result' }
  const failures = Type.parameter(owner, 0, 'E')
  const problem = Type.nominal('generics/Rows', 'Problem')
  const other = Type.nominal('generics/Rows', 'Other')
  const concrete = Type.union([problem, other])
  assert.strictEqual(concrete._tag, 'Normalized')
  if (concrete._tag !== 'Normalized') return

  const inferred = new Map<string, Type.GenericArgument>()
  assert.strictEqual(Type.infer(failures, concrete.type, inferred), true)
  assert.strictEqual(
    Type.encode(Type.substitute(failures, inferred)),
    'generics/Rows.Other | generics/Rows.Problem',
  )

  const syntax = Parser.parse(
    Lexer.lex(
      SourceFile.make(
        'generics/Projection',
        new TextEncoder().encode(
          'effect fn keep<E>(value: Effect<i32 ! E>) -> Effect<i32 ! E> { return value }',
        ),
      ),
    ),
  )
  assert.deepEqual(syntax.parserDiagnostics, [])
  assert.strictEqual(
    descendants(syntax.root).filter((node) => node.kind === 'FailureRow').length,
    2,
  )
})

it('distinguishes row inference failure causes deterministically', () => {
  const owner = { module: 'generics/Rows', name: 'diagnose' }
  const firstRequirement = Type.parameter(owner, 2, 'R', 'RequirementRow')
  const secondRequirement = Type.parameter(owner, 3, 'S', 'RequirementRow')
  const problem = Type.nominal('generics/Rows', 'Problem')
  const clock = Type.nominal('generics/Rows', 'Clock')
  const requirement = { capability: clock, role: 'Primary', access: 'Shared' as const }
  const closed = Type.effect('i32', [], 'Shared')
  const absentFailure = Type.rowInferenceFailure(Type.effect('i32', [problem]), closed)

  assert.strictEqual(absentFailure?._tag, 'AbsentFailureMember')
  if (absentFailure !== undefined)
    assert.deepEqual(
      {
        code: Diagnostic.contractRowInference(
          absentFailure,
          Parser.parse(Lexer.lex(file)).root.span,
        ).code,
        reason: Diagnostic.contractRowInference(
          absentFailure,
          Parser.parse(Lexer.lex(file)).root.span,
        ).reason._tag,
      },
      { code: 'SEM0089', reason: 'ContractRowInference' },
    )
  assert.strictEqual(
    Type.rowInferenceFailure(Type.effect('i32', [], 'Shared', [requirement]), closed)?._tag,
    'AbsentRequirementMember',
  )
  assert.strictEqual(
    Type.rowInferenceFailure(
      Type.effect('i32', [], 'Shared', [requirement]),
      Type.effect('i32', [], 'Shared', [{ ...requirement, role: 'Secondary' }]),
    )?._tag,
    'IncompatibleRequirementRole',
  )
  assert.strictEqual(
    Type.rowInferenceFailure(
      Type.effect('i32', [], 'Shared', [requirement]),
      Type.effect('i32', [], 'Shared', [{ ...requirement, access: 'Exclusive' }]),
    )?._tag,
    'IncompatibleRequirementAccess',
  )
  assert.strictEqual(
    Type.rowInferenceFailure(
      Type.effect('i32', [], 'Shared', [], [firstRequirement, secondRequirement]),
      closed,
    )?._tag,
    'AmbiguousRequirementRemainder',
  )
  assert.strictEqual(
    Type.rowInferenceFailure(
      Type.effect('i32', [], 'Shared', [], [firstRequirement]),
      Type.effect('i32', [], 'Shared', [], [secondRequirement]),
    )?._tag,
    'NonFiniteRequirementRow',
  )
})

it('orders Effect access bounds from reusable through take-capable', () => {
  const shared = Type.effect('i32', [], 'Shared')
  const exclusive = Type.effect('i32', [], 'Exclusive')
  const take = Type.effect('i32', [], 'Take')

  assert.isTrue(Type.infer(take, shared, new Map()))
  assert.isTrue(Type.infer(take, exclusive, new Map()))
  assert.isTrue(Type.infer(take, take, new Map()))
  assert.isTrue(Type.infer(exclusive, shared, new Map()))
  assert.isTrue(Type.infer(exclusive, exclusive, new Map()))
  assert.isFalse(Type.infer(exclusive, take, new Map()))
  assert.isFalse(Type.infer(shared, exclusive, new Map()))
  assert.isFalse(Type.infer(shared, take, new Map()))
})

it('keeps generic angles contextual and recovers damaged lists deterministically', () => {
  const comparisons = Parser.parse(
    Lexer.lex(
      SourceFile.make(
        'generics/Comparison',
        new TextEncoder().encode('pub fn main() -> i32 { if 1 < 2 { return 42 } return 0 }'),
      ),
    ),
  )
  assert.notInclude(
    descendants(comparisons.root).map((node) => node.kind),
    'CallTypeArgumentList',
  )
  assert.deepEqual(comparisons.parserDiagnostics, [])

  const missingArgument = Parser.parse(
    Lexer.lex(
      SourceFile.make(
        'generics/MissingArgument',
        new TextEncoder().encode(
          'fn identity<T>(value: T) -> T { return move value }\npub fn main() -> i32 { return identity<>(1) }',
        ),
      ),
    ),
  )
  assert.include(
    descendants(missingArgument.root).map((node) => node.kind),
    'CallTypeArgumentList',
  )
  assert.include(
    missingArgument.parserDiagnostics.map((diagnostic) => diagnostic.code),
    'PAR0001',
  )

  const missingClose = Parser.parse(
    Lexer.lex(
      SourceFile.make(
        'generics/MissingClose',
        new TextEncoder().encode(
          'struct Box<T> { value: T }\nfn broken(value: Box<i32) -> i32 { return 0 }',
        ),
      ),
    ),
  )
  assert.include(
    missingClose.parserDiagnostics.map((diagnostic) => diagnostic.code),
    'PAR0001',
  )
})

it.effect('formats generic declarations, applications, and calls idempotently', () =>
  Effect.gen(function* () {
    const syntax = Parser.parse(
      Lexer.lex(
        SourceFile.make(
          'generics/format',
          new TextEncoder().encode(
            'struct Box < T >{value:T}\nfn keep < T >(value:Box < T >)->Box<T>{return identity < T >(value)}',
          ),
        ),
      ),
    )
    const formatted = yield* SyntaxFormatter.format(syntax)
    const text = new TextDecoder().decode(FormattedDocument.toUint8Array(formatted))
    assert.strictEqual(
      text,
      'struct Box<T> {\n  value: T\n}\n\nfn keep<T>(value: Box<T>) -> Box<T> {\n  return identity<T>(value)\n}\n',
    )
    const again = yield* SyntaxFormatter.format(
      Parser.parse(Lexer.lex(SourceFile.make('generics/format', new TextEncoder().encode(text)))),
    )
    assert.strictEqual(new TextDecoder().decode(FormattedDocument.toUint8Array(again)), text)
  }),
)

it.effect('formats channel-kinded generic binders idempotently', () =>
  Effect.gen(function* () {
    const syntax = Parser.parse(
      Lexer.lex(
        SourceFile.make(
          'generics/channel-format',
          new TextEncoder().encode(
            'effect fn transform < A , E , ? R >(self:Effect<A ! E ? R>)->Effect<A ! E ? R>{return self}',
          ),
        ),
      ),
    )
    const formatted = yield* SyntaxFormatter.format(syntax)
    const text = new TextDecoder().decode(FormattedDocument.toUint8Array(formatted))
    assert.strictEqual(
      text,
      `effect fn transform<A, E, ?R>(self: Effect<A ! E ? R>) -> Effect<A ! E ? R> {
  return self
}
`,
    )
    const again = yield* SyntaxFormatter.format(
      Parser.parse(
        Lexer.lex(SourceFile.make('generics/channel-format', new TextEncoder().encode(text))),
      ),
    )
    assert.strictEqual(new TextDecoder().decode(FormattedDocument.toUint8Array(again)), text)
  }),
)

it.effect('infers and explicitly selects finite concrete instances before MIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.makeRealized({ root: file }).pipe(
      Effect.provide(SourceResolver.memory(new Map())),
    )

    assert.deepEqual(snapshot.diagnostics, [])
    assert.deepEqual(
      snapshot.instances.instances.map((instance) => ({
        name: instance.key.declaration.name,
        arguments: instance.key.typeArguments.map(Type.encodeGenericArgument),
      })),
      [
        { name: 'main', arguments: [] },
        { name: 'identity', arguments: ['bool'] },
        { name: 'identity', arguments: ['i32'] },
      ],
    )
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    assert.strictEqual(snapshot.mir.value.functions.length, 3)
    assert.notInclude(Mir.encode(snapshot.mir.value), 'TypeParameter')
    const outcome = BootstrapEvaluation.evaluate(snapshot.instances, snapshot.mir.value)
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer))
    if (outcome._tag === 'Completed') {
      assert.strictEqual(outcome.result.value, 42n)
      assert.deepEqual(
        outcome.trace.flatMap((event) =>
          event._tag === 'Call' && event.target.name === 'identity'
            ? [event.targetInstance.typeArguments.map(Type.encodeGenericArgument)]
            : [],
        ),
        [['bool'], ['i32']],
      )
    }
  }),
)

it.effect('infers the parameters an explicit type-argument prefix leaves open', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/PartialExplicit',
      new TextEncoder().encode(`fn pair<A, B>(left: A, right: B) -> A { return move left }
pub fn main() -> i32 { return pair<i32>(42, true) }`),
    )

    assert.deepEqual(snapshot.diagnostics, [])
    assert.deepEqual(
      snapshot.instances.instances.map((instance) => ({
        name: instance.key.declaration.name,
        arguments: instance.key.typeArguments.map(Type.encodeGenericArgument),
      })),
      [
        { name: 'main', arguments: [] },
        { name: 'pair', arguments: ['i32', 'bool'] },
      ],
    )
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('accepts failure-row and requirement-row arguments in an explicit prefix', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/RowPrefix',
      new TextEncoder().encode(`struct First {}
struct Second {}
service Clock {}
effect fn risky() -> i32 ! First | Second { fail First {} }
effect fn read() -> i32 ? &Clock { return 42 }
effect fn keepFailures<E>(self: once Effect<i32 ! E>) -> i32 ! E { return run self }
effect fn keepRequirements<?R>(self: once Effect<i32 ? R>) -> i32 ? R { return run self }
effect fn useFailures() -> i32 ! First | Second {
  return run keepFailures<First | Second>(risky())
}
effect fn useRequirements() -> i32 ? &Clock {
  return run keepRequirements<Clock>(read())
}
pub fn main() -> i32 { return 0 }`),
    )

    assert.deepEqual(snapshot.diagnostics, [])
  }),
)

it.effect('rejects a borrowed explicit failure type', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/WrongRowPrefix',
      new TextEncoder().encode(`struct Problem {}
struct Clock {}
effect fn risky() -> i32 ! Problem { fail Problem {} }
effect fn keepFailures<E>(self: once Effect<i32 ! E>) -> i32 ! E { return run self }
effect fn invalid() -> i32 ! Problem { return run keepFailures<Clock>(risky()) }
pub fn main() -> i32 { return 0 }`),
    )

    assert.include(
      snapshot.diagnostics.map((diagnostic) => diagnostic.code),
      'SEM0100',
    )
  }),
)

it.effect('uses an explicit prefix as value-argument context for the arguments it binds', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/PartialExplicitContext',
      new TextEncoder().encode(`struct Left { value: i32 }
struct Right { value: i32 }
fn accept<T, U>(value: T, other: U) -> i32 { return 42 }
pub fn main() -> i32 { return accept<Left | Right>(Left { value: 0 }, true) }`),
    )

    assert.deepEqual(snapshot.diagnostics, [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('names the parameter an explicit prefix leaves undetermined', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/UninferredRemainder',
      new TextEncoder().encode(`fn phantom<A, B>(value: A) -> A { return move value }
pub fn main() -> i32 { return phantom<i32>(1) }`),
    )

    assert.deepEqual(
      snapshot.diagnostics.map((diagnostic) => [diagnostic.code, diagnostic.message]),
      [['SEM0099', 'Cannot infer type argument B of phantom from supplied values']],
    )
    assert.strictEqual(snapshot.mir._tag, 'Unavailable')
  }),
)

it.effect('reports a contradicted explicit type argument at what the call wrote', () =>
  Effect.gen(function* () {
    const text = `fn pair<A, B>(left: A, right: B) -> A { return move left }
pub fn main() -> i32 { return pair<bool>(1, true) }`
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/ContradictedPrefix',
      new TextEncoder().encode(text),
    )

    const diagnostic = snapshot.diagnostics.at(0)
    assert.strictEqual(snapshot.diagnostics.length, 1, JSON.stringify(snapshot.diagnostics))
    assert.strictEqual(diagnostic?.code, 'SEM0100')
    assert.strictEqual(
      diagnostic?.message,
      'Type argument A of pair is bool, but the supplied values imply i32',
    )
    // The span covers the written type argument itself, not the call and not the argument that
    // disagrees with it.
    assert.strictEqual(text.slice(diagnostic?.span.start, diagnostic?.span.end), 'bool')
  }),
)

it.effect('uses complete explicit arguments as value-argument context', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/ExplicitContext',
      new TextEncoder().encode(`struct Left { value: i32 }
struct Right { value: i32 }
fn accept<T>(value: T) -> i32 { return 42 }
pub fn main() -> i32 {
  let empty = accept<[i32; 0]>([])
  return accept<Left | Right>(Left { value: empty })
}`),
    )
    assert.deepEqual(snapshot.diagnostics, [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('retains unresolved type-argument causes without fabricating arity failures', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/UnresolvedArgument',
      new TextEncoder().encode(
        'struct Box<T> { value: T }\nfn bad(value: Box<Missing>) -> i32 { return 0 }\npub fn main() -> i32 { return 42 }',
      ),
    )
    const codes = snapshot.diagnostics.map((diagnostic) => diagnostic.code)
    assert.include(codes, 'SEM0001')
    assert.notInclude(codes, 'SEM0051')
  }),
)

it.effect('does not fabricate a second identity for duplicate type parameters', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/DuplicateIdentity',
      new TextEncoder().encode(
        'fn bad<T, T>(value: T) -> T { return move value }\npub fn main() -> i32 { return 42 }',
      ),
    )
    const declaration = Projections.genericDeclarationsOf(snapshot).at(0)
    const first = declaration?.typeParameters.at(0)
    const duplicate = declaration?.typeParameters.at(1)
    assert.notStrictEqual(first, undefined)
    assert.strictEqual(duplicate?.type, first?.type)
    assert.strictEqual(duplicate?.duplicateOf, first?.type)
  }),
)

it.effect('keeps open cleanup symbolic and specializes it before MIR', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/Cleanup',
      new TextEncoder().encode(`struct Payload {}
fn discard<T>(value: T) -> i32 { return 42 }
pub fn main() -> i32 { return discard<Payload>(Payload {}) }`),
    )
    assert.deepEqual(snapshot.diagnostics, [])
    const ownership = Analysis.ownershipOf(snapshot, 'generics/Cleanup')
    assert.notStrictEqual(ownership, undefined)
    if (ownership !== undefined) {
      assert.include(OwnershipEncoding.encode(ownership), 'release p0')
      const discard = ownership.functions.find(
        (fn) =>
          fn.declaration.canonical._tag === 'Canonical' &&
          fn.declaration.canonical.id.name === 'discard',
      )
      assert.strictEqual(discard?.exits.at(0)?.releases.at(0)?.cleanup._tag, 'ParameterCleanup')
    }
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    assert.notInclude(Mir.encode(snapshot.mir.value), 'TypeParameter')
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
  }),
)

it.effect('lays out and evaluates only the reached applied struct', () =>
  Effect.gen(function* () {
    const structFile = SourceFile.make(
      'generics/Box',
      new TextEncoder().encode(`struct Box<T> { value: T }
pub fn main() -> i32 {
  let box = Box<i32> { value: 42 }
  return box.value
}`),
    )
    const snapshot = yield* Analysis.makeRealized({ root: structFile }).pipe(
      Effect.provide(SourceResolver.memory(new Map())),
    )

    assert.deepEqual(snapshot.diagnostics, [])
    assert.strictEqual(snapshot.layout._tag, 'Available')
    if (snapshot.layout._tag !== 'Available' || snapshot.mir._tag !== 'Available') return
    assert.deepEqual(
      snapshot.layout.value.entries.map((entry) => Type.encode(entry.type)),
      ['i32', 'generics/Box.Box<i32>'],
    )
    const outcome = BootstrapEvaluation.evaluate(snapshot.instances, snapshot.mir.value)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('resolves inferred and explicit generic calls across module namespaces', () =>
  Effect.gen(function* () {
    const root = SourceFile.make(
      'app/Main',
      new TextEncoder().encode(`import library.Generic
pub fn main() -> i32 { return Generic.identity<i32>(Generic.identity(42)) }`),
    )
    const snapshot = yield* Analysis.makeRealized({ root }).pipe(
      Effect.provide(
        SourceResolver.memory(
          new Map([
            [
              'library/Generic',
              new TextEncoder().encode('pub fn identity<T>(value: T) -> T { return move value }'),
            ],
          ]),
        ),
      ),
    )

    assert.deepEqual(snapshot.diagnostics, [])
    assert.strictEqual(snapshot.instances.instances.length, 2)
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    const outcome = BootstrapEvaluation.evaluate(snapshot.instances, snapshot.mir.value)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('cuts off recursive generic calls that change an ancestor specialization', () =>
  Effect.gen(function* () {
    const recursive = SourceFile.make(
      'generics/Recursive',
      new TextEncoder().encode(`fn expand<T>(value: T) -> i32 {
  return expand<[T; 1]>([move value])
}
pub fn main() -> i32 { return expand<i32>(1) }`),
    )
    const snapshot = yield* Analysis.makeRealized({ root: recursive }).pipe(
      Effect.provide(SourceResolver.memory(new Map())),
    )

    assert.strictEqual(snapshot.instances.violations.length, 1)
    assert.deepEqual(
      snapshot.instances.violations.at(0)?.target.typeArguments.map(Type.encodeGenericArgument),
      ['Array<i32, 1>'],
    )
    assert.strictEqual(snapshot.instances.instances.length, 2)
    assert.deepEqual(
      snapshot.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0053'],
    )
    assert.strictEqual(snapshot.layout._tag, 'Unavailable')
    assert.strictEqual(snapshot.mir._tag, 'Unavailable')
  }),
)

it.effect('keeps same-argument generic recursion finite and omits unused declarations', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/SameRecursion',
      new TextEncoder().encode(`fn recurse<T>(value: T) -> i32 {
  if false { return recurse<T>(move value) }
  return 42
}
fn unused<T>(value: T) -> T { return move value }
pub fn main() -> i32 { return recurse<i32>(1) }`),
    )
    assert.deepEqual(snapshot.diagnostics, [])
    assert.deepEqual(
      snapshot.instances.instances.map((instance) => instance.key.declaration.name),
      ['main', 'recurse'],
    )
    assert.deepEqual(snapshot.instances.violations, [])
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('detects parameter-changing recursion across a mutual generic cycle', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/MutualRecursion',
      new TextEncoder().encode(`fn first<T>(value: T) -> i32 {
  return second<[T; 1]>([move value])
}
fn second<U>(value: U) -> i32 {
  return first<U>(move value)
}
pub fn main() -> i32 { return first<i32>(1) }`),
    )
    assert.deepEqual(
      snapshot.diagnostics.map((diagnostic) => diagnostic.code),
      ['SEM0053'],
    )
    assert.strictEqual(snapshot.instances.violations.length, 1)
    assert.strictEqual(snapshot.layout._tag, 'Unavailable')
    assert.strictEqual(snapshot.mir._tag, 'Unavailable')
  }),
)

it.effect('checks repeated instances under every recursive ancestor context', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/ContextualRecursion',
      new TextEncoder().encode(`fn a<T>(value: T) -> i32 { return x<T>(move value) }
fn x<T>(value: T) -> i32 {
  if false { return a<bool>(true) }
  return 0
}
pub fn main() -> i32 {
  let first = a<i32>(1)
  let second = a<bool>(true)
  return x<i32>(first + second)
}`),
    )
    assert.include(
      snapshot.diagnostics.map((diagnostic) => diagnostic.code),
      'SEM0053',
    )
    assert.isAtLeast(snapshot.instances.violations.length, 1)
    assert.strictEqual(snapshot.layout._tag, 'Unavailable')
    assert.strictEqual(snapshot.mir._tag, 'Unavailable')
  }),
)

const invalidCases: ReadonlyArray<readonly [string, string, string]> = [
  [
    'duplicate parameter',
    'fn bad<T, T>(value: T) -> T { return move value }\npub fn main() -> i32 { return 0 }',
    'SEM0050',
  ],
  [
    'unbound parameter',
    'fn bad<T>(value: U) -> T { return move value }\npub fn main() -> i32 { return 0 }',
    'SEM0001',
  ],
  [
    'missing nominal arguments',
    'struct Box<T> { value: T }\nfn bad(value: Box) -> i32 { return 0 }\npub fn main() -> i32 { return 0 }',
    'SEM0051',
  ],
  [
    'non-generic nominal application',
    'struct Plain { value: i32 }\nfn bad(value: Plain<i32>) -> i32 { return 0 }\npub fn main() -> i32 { return 0 }',
    'SEM0051',
  ],
  [
    'excess explicit arguments',
    'fn id<T>(value: T) -> T { return move value }\npub fn main() -> i32 { return id<i32, bool>(1) }',
    'SEM0051',
  ],
  [
    'excess explicit arguments past a complete prefix',
    'fn pair<A, B>(left: A, right: B) -> A { return move left }\npub fn main() -> i32 { return pair<i32, bool, u8>(1, true) }',
    'SEM0051',
  ],
  [
    'non-generic builtin specialization',
    'import silk.i32 as i32\npub fn main() -> i32 { return i32.add<i32>(40, 2) }',
    'SEM0051',
  ],
  [
    'conflicting inference',
    'fn same<T>(left: T, right: T) -> T { return move left }\npub fn main() -> i32 { return same(1, true) }',
    'SEM0052',
  ],
  [
    'return-only inference',
    'fn make<T>() -> T {}\npub fn main() -> i32 { return make() }',
    'SEM0052',
  ],
  [
    'concrete-only operation in an open body',
    'import silk.i32 as i32\nfn addOne<T>(value: T) -> i32 { return i32.add(move value, 1) }\npub fn main() -> i32 { return addOne<i32>(41) }',
    'SEM0012',
  ],
]

for (const [name, text, code] of invalidCases) {
  it.effect(`diagnoses ${name} before target-dependent phases`, () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        `generics/invalid/${name.replaceAll(' ', '-')}`,
        new TextEncoder().encode(text),
      )
      assert.include(
        snapshot.diagnostics.map((diagnostic) => diagnostic.code),
        code,
      )
      assert.strictEqual(snapshot.instances.instances.length, 0)
      assert.strictEqual(snapshot.layout._tag, 'Unavailable')
      assert.strictEqual(snapshot.mir._tag, 'Unavailable')
    }),
  )
}

it.effect(
  'substitutes generic nominal pattern fields without rechecking the declaration body',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'generics/Pattern',
        new TextEncoder().encode(`struct Box<T> { value: T }
fn take<T>(input: Box<T>) -> T {
  return match move input { Box<T> { value } => move value }
}
pub fn main() -> i32 { return take(Box<i32> { value: 42 }) }`),
      )
      assert.deepEqual(snapshot.diagnostics, [])
      const outcome = Analysis.evaluate(snapshot)
      assert.strictEqual(outcome._tag, 'Completed', JSON.stringify(outcome, Json.bigIntReplacer))
      if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
    }),
)

it.effect('substitutes cleanup through applied pattern paths and omitted fields', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/PatternCleanup',
      new TextEncoder().encode(`struct Token { value: i32 }
struct Pair<A, B> { first: A second: B }
fn take<T>(pair: Pair<i32, T>) -> i32 {
  return match move pair { Pair<i32, T> { first, .. } => first }
}
pub fn main() -> i32 {
  return take<Token>(Pair<i32, Token> { first: 42, second: Token { value: 0 } })
}`),
    )
    assert.deepEqual(snapshot.diagnostics, [])
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    const cleanup = snapshot.mir.value.functions
      .flatMap((fn) => fn.regions)
      .flatMap((region) => (region._tag === 'OperationRegion' ? region.operations : []))
      .flatMap(Mir.operationTree)
      .flatMap((operation) =>
        operation._tag === 'Match'
          ? operation.arms.flatMap((arm) => arm.selected.cleanup.map((entry) => entry.cleanup))
          : [],
      )
    assert.include(
      cleanup.map((entry) => entry._tag),
      'StructCleanup',
    )
    assert.include(
      cleanup.map((entry) => Type.encode(entry.type)),
      'generics/PatternCleanup.Token',
    )
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed')
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('classifies generic writes after substituting the concrete element type', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/Write',
      new TextEncoder().encode(`fn replace<T>(values: [T; 1], value: T) -> i32 {
  let mut result = move values
  result[0] = move value
  return 42
}
pub fn main() -> i32 { return replace<i32>([1], 2) }`),
    )
    assert.deepEqual(snapshot.diagnostics, [])
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    const replacements = snapshot.mir.value.functions
      .flatMap((fn) => fn.regions)
      .flatMap((region) => (region._tag === 'OperationRegion' ? region.operations : []))
      .flatMap(Mir.operationTree)
      .flatMap((operation) => (operation._tag === 'WritePlace' ? [operation.replacement] : []))
    assert.deepEqual(replacements, ['Copy'])
  }),
)

it.effect('links an open HIR call through every reached caller instance', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/Facade',
      new TextEncoder().encode(`fn inner<T>(value: T) -> T { return move value }
fn outer<T>(value: T) -> T { return inner<T>(move value) }
pub fn main() -> i32 {
  let flag = outer(true)
  if flag { return outer(42) }
  return 0
}`),
    )
    const call = Projections.genericCallsOf(snapshot).find(
      (candidate) => candidate.target.name === 'inner',
    )
    assert.notStrictEqual(call, undefined)
    if (call === undefined) return
    assert.deepEqual(
      Projections.instancesOfCall(snapshot, call).map((link) =>
        link.target.key.typeArguments.map(Type.encodeGenericArgument),
      ),
      [['bool'], ['i32']],
    )
  }),
)

it.effect('rejects residual open MIR and keeps specialization symbols injective', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'generics/MirBoundary',
      new TextEncoder().encode(source),
    )
    assert.strictEqual(snapshot.mir._tag, 'Available')
    if (snapshot.mir._tag !== 'Available') return
    const fn = snapshot.mir.value.functions.at(0)
    assert.notStrictEqual(fn, undefined)
    if (fn === undefined) return
    const parameter = Type.parameter({ module: 'malformed', name: 'fn' }, 0, 'T')
    const malformed = Object.freeze({
      ...snapshot.mir.value,
      functions: Object.freeze([
        Object.freeze({
          ...fn,
          instance: Object.freeze({ ...fn.instance, typeArguments: Object.freeze([parameter]) }),
        }),
      ]),
    })
    assert.include(
      Mir.verify(malformed).map((violation) => violation.rule),
      'InvalidInstance',
    )

    const collision = (module: string): Mir.MirFunction => {
      const declaration = Object.freeze({
        _tag: 'CanonicalDeclarationId' as const,
        module,
        name: 'same',
      })
      return Object.freeze({
        ...fn,
        id: declaration,
        instance: Object.freeze({ ...fn.instance, declaration }),
      })
    }
    assert.notStrictEqual(
      Backend.symbolFor(collision('a/b'), fn.instance),
      Backend.symbolFor(collision('a_b'), fn.instance),
    )
  }),
)

it.effect('emits the same concrete specialization set through LLVM and WebAssembly', () =>
  Effect.gen(function* () {
    const text = `struct Pair { left: i32 right: i32 }
struct Box<T> { value: T }
fn identity<T>(value: T) -> T { return move value }
pub fn main() -> i32 {
  let scalar = Box<i32> { value: identity(0) }
  let pair = Box<Pair> { value: identity<Pair>(Pair { left: 40, right: 2 }) }
  return scalar.value + pair.value.left + pair.value.right
}`
    const native = yield* Analysis.ofSourceRealized(
      'generics/Backend',
      new TextEncoder().encode(text),
      'aarch64-apple-darwin',
    )
    const wasm = yield* Analysis.ofSourceRealized(
      'generics/Backend',
      new TextEncoder().encode(text),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(native.diagnostics, [])
    assert.deepEqual(wasm.diagnostics, [])
    const nativeArtifact = yield* Analysis.codegen(native, { mode: 'release' })
    const wasmArtifact = yield* Analysis.codegenWasm(wasm, { mode: 'release' })
    assert.deepEqual(
      nativeArtifact.symbols.map((entry) => entry.symbol),
      wasmArtifact.symbols.map((entry) => entry.symbol),
    )
    assert.deepEqual(
      wasmArtifact.symbols.map((entry) =>
        entry.instance.typeArguments.map(Type.encodeGenericArgument),
      ),
      [[], ['i32'], ['generics/Backend.Pair']],
    )
    assert.deepEqual(
      native.layout._tag === 'Available'
        ? native.layout.value.entries
            .map((entry) => Type.encode(entry.type))
            .filter((type) => type.includes('Box<'))
        : [],
      ['generics/Backend.Box<i32>', 'generics/Backend.Box<generics/Backend.Pair>'],
    )
    if (native.layout._tag === 'Available') {
      const plan = native.layout.value
      const boxes = plan.entries.filter(
        (entry) => Type.isNominal(entry.type) && entry.type.name === 'Box',
      )
      assert.deepEqual(
        boxes.map((entry) => entry.size),
        [4, 8],
      )
      assert.deepEqual(
        boxes.map((entry) => Layout.callingShape(plan, entry.type)?.laneCount),
        [1, 2],
      )
    }
    const instance = new WebAssembly.Instance(
      new WebAssembly.Module(wasmArtifact.bytes.slice()),
      {},
    )
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)
