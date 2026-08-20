import { NodeServices } from '@effect/platform-node'
import { assert, layer } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import * as Analysis from '../src/Analysis.js'
import type * as DeclarationIndex from '../src/DeclarationIndex.js'
import * as FormattedDocument from '../src/FormattedDocument.js'
import * as Formatter from '../src/Formatter.js'
import * as Lexer from '../src/Lexer.js'
import * as Parser from '../src/Parser.js'
import * as SourceFile from '../src/SourceFile.js'
import * as Type from '../src/Type.js'

const decoder = new TextDecoder()
const fixture = Effect.fnUntraced(function* (name: string) {
  const fileSystem = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const fixturePath = yield* path.fromFileUrl(
    new URL(`./fixtures/complete-interface-contracts/${name}.silk`, import.meta.url),
  )
  return yield* fileSystem.readFile(fixturePath)
})
const parse = Effect.fnUntraced(function* (name: string) {
  const source = yield* fixture(name)
  return Parser.parse(Lexer.lex(SourceFile.make(`complete-interface-contracts/${name}`, source)))
})
const analyze = Effect.fnUntraced(function* (name: string) {
  const source = yield* fixture(name)
  return yield* Analysis.ofSourceRealized(
    `complete-interface-contracts/${name}`,
    source,
    'wasm32-unknown-unknown',
  )
})
const namedInterface = (
  index: DeclarationIndex.Index,
  name: string,
): DeclarationIndex.InterfaceFact | undefined =>
  index.modules
    .flatMap((module) => module.interfaces)
    .find((candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === name)
const namedDeclaration = (
  index: DeclarationIndex.Index,
  name: string,
): DeclarationIndex.DeclarationFact | undefined =>
  index.modules
    .flatMap((module) => module.declarations)
    .find((candidate) => candidate.name._tag === 'Present' && candidate.name.spelling === name)
const encodedOperand = (operand: DeclarationIndex.InterfaceOperandFact): string =>
  operand.type._tag === 'Resolved' ? Type.encode(operand.type.type) : operand.type._tag

layer(NodeServices.layer)('complete interface contract fixtures', (it) => {
  it.effect('parses and formats complete kinded interface syntax idempotently', () =>
    Effect.gen(function* () {
      const syntax = yield* parse('syntax')
      assert.deepEqual(syntax.parserDiagnostics, [])
      const first = yield* Formatter.format(syntax)
      const text = decoder.decode(FormattedDocument.toUint8Array(first))
      assert.include(text, 'pub interface Decoder<S, Arguments, A, E, ?R>')
      assert.include(text, 'effect fn decode(self: &S, encoded: Arguments) -> A ! E ? R')
      const second = yield* Formatter.format(
        Parser.parse(
          Lexer.lex(
            SourceFile.make('complete-interface-contracts/formatted', Uint8Array.from(first.bytes)),
          ),
        ),
      )
      assert.deepEqual(second.bytes, first.bytes)
      assert.strictEqual(second.changed, false)
    }),
  )

  it.effect('retains declaration flow, literal operands, success, rows, access, and kinds', () =>
    Effect.gen(function* () {
      const snapshot = yield* analyze('syntax')
      assert.deepEqual(Analysis.diagnostics(snapshot), [])
      const declaration = namedInterface(Analysis.declarationIndex(snapshot), 'Decoder')
      assert.strictEqual(declaration?.visibility, 'Public')
      assert.deepEqual(
        declaration?.typeParameters.map((parameter) => parameter.type.kind),
        ['Value', 'Value', 'Value', 'FailureRow', 'RequirementRow'],
      )
      const contract = declaration?.operationContracts.at(0)
      assert.strictEqual(contract?.functionKind, 'Effect')
      assert.deepEqual(contract?.operands.map(encodedOperand), ['&S', 'Arguments'])
      assert.deepEqual(
        contract?.operands.map((operand) => operand.access),
        ['Shared', 'Take'],
      )
      assert.strictEqual(contract?.receiverAccess, 'Shared')
      assert.strictEqual(
        contract?.success._tag === 'Resolved' ? Type.encode(contract.success.type) : undefined,
        'A',
      )
      assert.deepEqual(
        contract?.failureRow.parameters.map((parameter) => parameter.name),
        ['E'],
      )
      assert.deepEqual(
        contract?.requirementRow.parameters.map((parameter) => parameter.name),
        ['R'],
      )
      const bound = namedDeclaration(
        Analysis.declarationIndex(snapshot),
        'generic',
      )?.typeParameters.at(5)?.bound
      assert.strictEqual(bound?._tag, 'ResolvedBound')
      if (bound?._tag !== 'ResolvedBound') return
      assert.strictEqual(bound.application.available, true)
      assert.deepEqual(
        bound.application.operations
          .at(0)
          ?.failureRow.parameters.map((parameter) => parameter.name),
        ['E'],
      )
      assert.deepEqual(
        bound.application.operations
          .at(0)
          ?.requirementRow.parameters.map((parameter) => parameter.name),
        ['R'],
      )
    }),
  )

  it.effect('records complete applications and makes provider equality explicit', () =>
    Effect.gen(function* () {
      const snapshot = yield* analyze('provider-equality')
      const index = Analysis.declarationIndex(snapshot)
      const matching = namedDeclaration(index, 'matching')?.typeParameters.at(0)?.bound
      const mismatched = namedDeclaration(index, 'mismatched')?.typeParameters.at(0)?.bound
      assert.strictEqual(matching?._tag, 'ResolvedBound')
      assert.strictEqual(mismatched?._tag, 'ResolvedBound')
      if (matching?._tag !== 'ResolvedBound' || mismatched?._tag !== 'ResolvedBound') return
      assert.strictEqual(matching.application.providerMatches, true)
      assert.strictEqual(matching.application.available, true)
      assert.strictEqual(mismatched.application.providerMatches, false)
      assert.strictEqual(mismatched.application.available, false)
      assert.strictEqual(mismatched.application.operations.length, 1)
      const mismatchedOperation = mismatched.application.operations.at(0)
      assert.strictEqual(
        mismatchedOperation?.declaration.name._tag === 'Present'
          ? mismatchedOperation.declaration.name.spelling
          : undefined,
        'decode',
      )
      assert.isTrue(
        Analysis.diagnostics(snapshot).some((diagnostic) =>
          diagnostic.message.includes('Decoder must be applied to its own provider'),
        ),
      )
      const contract = matching.application.operations.at(0)
      assert.deepEqual(contract?.operands.map(encodedOperand), ['&T', 'i32'])
      assert.strictEqual(
        contract?.success._tag === 'Resolved' ? Type.encode(contract.success.type) : undefined,
        'bool',
      )
      assert.deepEqual(contract?.failureRow.failures.map(Type.encode), [
        'complete-interface-contracts/provider-equality.DecodeError',
      ])
      assert.strictEqual(contract?.failureRow.available, true)
      assert.deepEqual(
        contract?.requirementRow.requirements.map((requirement) => ({
          capability: Type.encode(requirement.capability),
          access: requirement.access,
        })),
        [
          {
            capability: 'complete-interface-contracts/provider-equality.Clock',
            access: 'Shared',
          },
        ],
      )
      assert.strictEqual(contract?.requirementRow.available, true)
    }),
  )

  it.effect('retains damaged contracts without presenting unavailable shapes as complete', () =>
    Effect.gen(function* () {
      const snapshot = yield* analyze('damaged')
      assert.isAbove(Analysis.diagnostics(snapshot).length, 0)
      const declaration = namedInterface(Analysis.declarationIndex(snapshot), 'Broken')
      const contract = declaration?.operationContracts.at(0)
      assert.strictEqual(declaration?.operationContracts.length, 1)
      assert.deepEqual(contract?.operands.map(encodedOperand), [
        '&S',
        'Unresolved',
        '&[u8]',
        '&mut [u8]',
        'Reference',
      ])
      assert.deepEqual(
        contract?.operands.map((operand) => operand.access),
        ['Shared', 'Unavailable', 'Shared', 'Exclusive', 'Exclusive'],
      )
      assert.strictEqual(contract?.success._tag, 'Unresolved')
      assert.strictEqual(contract?.requirementRow.available, false)
      const bound = namedDeclaration(
        Analysis.declarationIndex(snapshot),
        'broken',
      )?.typeParameters.at(0)?.bound
      assert.strictEqual(bound?._tag, 'ResolvedBound')
      if (bound?._tag !== 'ResolvedBound') return
      assert.strictEqual(bound.application.available, false)
      assert.strictEqual(bound.application.operations.length, 1)
      assert.strictEqual(bound.application.operations.at(0)?.success._tag, 'Unresolved')
    }),
  )

  it.effect('carries declaration visibility into each bound application', () =>
    Effect.gen(function* () {
      const snapshot = yield* analyze('visibility')
      const index = Analysis.declarationIndex(snapshot)
      const privateBound = namedDeclaration(index, 'privateBound')?.typeParameters.at(0)?.bound
      const publicBound = namedDeclaration(index, 'publicBound')?.typeParameters.at(0)?.bound
      assert.strictEqual(
        privateBound?._tag === 'ResolvedBound' ? privateBound.application.visibility : undefined,
        'Private',
      )
      assert.strictEqual(
        publicBound?._tag === 'ResolvedBound' ? publicBound.application.visibility : undefined,
        'Public',
      )
    }),
  )

  it.effect('keeps representation parameters available to conditional witness binders', () =>
    Effect.gen(function* () {
      const snapshot = yield* analyze('representation-witness')
      assert.deepEqual(
        Analysis.diagnostics(snapshot).filter(
          (diagnostic) =>
            diagnostic.span.sourceId === 'complete-interface-contracts/representation-witness',
        ),
        [],
      )
      const index = Analysis.declarationIndex(snapshot)
      const conformance = index.modules.flatMap((module) => module.conformances).at(0)
      const witness = namedDeclaration(index, 'render')
      assert.strictEqual(conformance?.requirements.length, 1)
      assert.strictEqual(
        conformance?.typeParameters.find(
          (parameter) => parameter.type.kind === 'CallableRepresentation',
        )?.type.kind,
        'CallableRepresentation',
      )
      assert.strictEqual(
        witness?.typeParameters.find(
          (parameter) => parameter.type.kind === 'CallableRepresentation',
        )?.type.kind,
        'CallableRepresentation',
      )
      assert.strictEqual(conformance?.operations.at(0)?.contract?.receiverAccess, 'Shared')
      assert.strictEqual(conformance?.validity._tag, 'ValidConformance')
    }),
  )

  it.effect('admits a complete effect witness through the applied compatibility contract', () =>
    Effect.gen(function* () {
      const snapshot = yield* analyze('mapped-effect')
      const conformance = Analysis.declarationIndex(snapshot)
        .modules.flatMap((module) => module.conformances)
        .at(0)
      const contract = conformance?.operations.at(0)?.contract
      assert.strictEqual(contract?.functionKind, 'Effect')
      assert.deepEqual(contract?.operands.map(encodedOperand), [
        '&complete-interface-contracts/mapped-effect.Schema',
        'i32',
      ])
      assert.strictEqual(contract?.receiverAccess, 'Shared')
      assert.deepEqual(contract?.failureRow.failures.map(Type.encode), [
        'complete-interface-contracts/mapped-effect.DecodeError',
      ])
      assert.deepEqual(
        contract?.requirementRow.requirements.map((requirement) =>
          Type.encode(requirement.capability),
        ),
        ['complete-interface-contracts/mapped-effect.Clock'],
      )
      assert.strictEqual(conformance?.validity._tag, 'ValidConformance')
    }),
  )
})
