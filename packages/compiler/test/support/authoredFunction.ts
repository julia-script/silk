import * as Effect from 'effect/Effect'
import type * as AuthoredHir from '../../src/AuthoredHir.js'
import * as AuthoredIdentity from '../../src/AuthoredIdentity.js'
import * as AuthoredPool from '../../src/AuthoredPool.js'

/** One source-free callable, with controllable pool layout and an exact authored body. */
export const make = Effect.fnUntraced(function* (
  texts: readonly string[] = ['identity', 'i32', 'value'],
  value = 9007199254740993n,
  functionName = 'identity',
) {
  const moduleOwner = AuthoredIdentity.module('demo', 'app/Value')
  const owner = AuthoredIdentity.children(moduleOwner, [
    { kind: 'function', name: functionName },
  ]).at(0)
  if (owner === undefined) return yield* Effect.die('Expected a fixture owner')
  const node = (role: string): AuthoredHir.Node => ({
    anchor: {
      _tag: 'AuthoredAnchor',
      owner,
      path: [{ _tag: 'LocalSegment', role, occurrence: 0 }],
    },
    origin: { _tag: 'Authored' },
    causes: [],
  })
  const name = (text: string, role: string): AuthoredHir.Name => ({
    _tag: 'Name',
    ...node(role),
    text: { _tag: 'TextRef', index: texts.indexOf(text) },
  })
  const type: AuthoredHir.Type = {
    _tag: 'NamedType',
    ...node('result'),
    mode: undefined,
    path: { _tag: 'Path', ...node('result-path'), segments: [name('i32', 'result-name')] },
  }
  const literal: AuthoredHir.IntegerLiteral = {
    _tag: 'IntegerLiteral',
    ...node('literal'),
    value,
    radix: 10,
    suffix: undefined,
  }
  const statements: AuthoredHir.Statement[] = [
    { _tag: 'ReturnStatement', ...node('return'), value: literal },
  ]
  const declaration: AuthoredHir.Declaration = {
    _tag: 'Declaration',
    owner,
    header: {
      _tag: 'FunctionHeader',
      ...node('header'),
      name: name(functionName, 'name'),
      public: true,
      linkage: undefined,
      properties: [],
      contract: {
        _tag: 'CallableContract',
        ...node('contract'),
        generics: [],
        parameters: [],
        variadic: false,
        result: type,
        failures: undefined,
        requirements: undefined,
        constraints: [],
        effect: false,
        test: false,
        environment: undefined,
        unsafe: false,
        static: false,
        effectAnchor: undefined,
        testAnchor: undefined,
        unsafeAnchor: undefined,
        staticAnchor: undefined,
        genericsAnchor: undefined,
        failuresAnchor: undefined,
        constraintsAnchor: undefined,
      },
    },
    body: { _tag: 'CallableBody', block: { _tag: 'Block', ...node('body'), statements } },
  }
  const module: AuthoredHir.Module = {
    _tag: 'AuthoredModule',
    owner: moduleOwner,
    pool: yield* AuthoredPool.make(texts, [[65, 0, 255]]),
    declarations: [declaration],
  }
  return { module, declaration, statements, literal }
})
