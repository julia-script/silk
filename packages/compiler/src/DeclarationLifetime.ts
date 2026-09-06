import * as Diagnostic from './Diagnostic.js'
import * as BodyLifetime from './BodyLifetime.js'
import * as Lifetime from './Lifetime.js'
import type * as SourceFile from './SourceFile.js'
import * as SyntaxTree from './SyntaxTree.js'
import type * as Token from './Token.js'
import * as Type from './Type.js'

/** One implicit declaration parameter with the exact annotation which introduced it. */
export interface ImplicitBinder {
  readonly parameter: Type.Parameter
  readonly syntax: SyntaxTree.Node
  readonly token: Token.Token
}

/** Header-only region elaboration shared by declaration collection and source presentation. */
export interface Context {
  readonly owner: Lifetime.Owner
  readonly source: SourceFile.SourceFile
  readonly syntax: SyntaxTree.Node
  readonly parameters: ReadonlyMap<string, Type.Parameter>
  readonly nominalArguments: ReadonlyMap<SyntaxTree.Node, ReadonlyArray<Lifetime.Lifetime>>
  readonly regions: ReadonlyMap<SyntaxTree.Node, Lifetime.Lifetime>
  readonly callables: ReadonlyMap<SyntaxTree.Node, Type.ExecutableLifetimes>
  readonly implicit: ReadonlyArray<ImplicitBinder>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly explicitEnvironment?: Lifetime.Lifetime
}

const spelling = (source: SourceFile.SourceFile, token: Token.Token): string =>
  String.fromCharCode(...source.bytes.slice(token.span.start, token.span.end))

/** Resolves only lexical lifetime names; ordinary type names never enter this lookup. */
export const named = (
  source: SourceFile.SourceFile,
  token: Token.Token,
  parameters: ReadonlyMap<string, Type.Parameter>,
): Lifetime.Lifetime | undefined => {
  const name = spelling(source, token)
  if (name === "'static") return Lifetime.staticLifetime
  const parameter = parameters.get(name)
  return parameter?.kind === 'Lifetime'
    ? Lifetime.bound(parameter.owner, parameter.ordinal, parameter.name)
    : undefined
}

const typeChild = (node: SyntaxTree.Node): SyntaxTree.Node | undefined =>
  node.children.find(
    (child): child is SyntaxTree.Node =>
      SyntaxTree.isNode(child) &&
      child.kind !== 'TypeParameterList' &&
      child.kind !== 'LifetimeBinderList',
  )

const isString = (source: SourceFile.SourceFile, node: SyntaxTree.Node): boolean => {
  if (node.kind !== 'TypePath') return false
  const tokens = SyntaxTree.tokens(node).filter((token) => token.kind === 'Identifier')
  const token = tokens.at(0)
  return tokens.length === 1 && token !== undefined && spelling(source, token) === 'string'
}

/**
 * Assigns regions using only a declaration header. Syntax-node keys locate authored occurrences;
 * semantic identities use owner and traversal ordinal and never contain source offsets.
 */
export const forHeader = (
  source: SourceFile.SourceFile,
  owner: Lifetime.Owner,
  syntax: SyntaxTree.Node,
  parameters: ReadonlyMap<string, Type.Parameter>,
  body?: BodyLifetime.BodyLifetime,
  nominalParameters?: (path: SyntaxTree.Node) => ReadonlyArray<Type.Parameter> | undefined,
): Context => {
  const nominalArguments = new Map<SyntaxTree.Node, ReadonlyArray<Lifetime.Lifetime>>()
  const regions = new Map<SyntaxTree.Node, Lifetime.Lifetime>()
  const callables = new Map<SyntaxTree.Node, Type.ExecutableLifetimes>()
  const implicit: Array<ImplicitBinder> = []
  const diagnostics: Array<Diagnostic.Diagnostic> = []
  const names = new Set(parameters.keys())
  const bindings = new Map<string, Lifetime.Lifetime>()
  for (const [name, parameter] of parameters)
    if (parameter.kind === 'Lifetime')
      bindings.set(name, Lifetime.bound(parameter.owner, parameter.ordinal, parameter.name))
  bindings.set("'static", Lifetime.staticLifetime)
  let ordinal = Math.max(-1, ...[...parameters.values()].map((parameter) => parameter.ordinal)) + 1
  let callableOrdinal = 0

  const fresh = (node: SyntaxTree.Node): Lifetime.Lifetime | undefined => {
    const position = ordinal++
    if (body !== undefined) return BodyLifetime.region(body, node, 'Annotation')
    let name = `'life${position}`
    while (names.has(name)) name += '_'
    names.add(name)
    const parameter = Type.parameter(owner, position, name, 'Lifetime')
    const token = SyntaxTree.tokens(node).find((candidate) => candidate.kind !== 'Whitespace')
    if (token !== undefined) implicit.push(Object.freeze({ parameter, syntax: node, token }))
    return Lifetime.bound(owner, position, name)
  }
  const resolve = (
    node: SyntaxTree.Node,
    token: Token.Token,
    scope: ReadonlyMap<string, Lifetime.Lifetime>,
  ) => {
    const name = spelling(source, token)
    const lifetime = scope.get(name)
    if (lifetime === undefined) diagnostics.push(Diagnostic.unknownLifetime(name, token.span))
    else regions.set(node, lifetime)
    return lifetime
  }
  const region = (
    node: SyntaxTree.Node,
    scope: ReadonlyMap<string, Lifetime.Lifetime>,
    output: boolean,
    defaultOutput: Lifetime.Lifetime | undefined,
    allocate: (node: SyntaxTree.Node) => Lifetime.Lifetime | undefined,
  ): Lifetime.Lifetime | undefined => {
    const explicit = SyntaxTree.directToken(node, 'Lifetime')
    if (explicit !== undefined) return resolve(node, explicit, scope)
    const value = output && body === undefined ? defaultOutput : allocate(node)
    if (value === undefined) diagnostics.push(Diagnostic.ambiguousLifetimeElision(node.span))
    else regions.set(node, value)
    return value
  }
  const nominal = (
    node: SyntaxTree.Node,
    path: SyntaxTree.Node,
    scope: ReadonlyMap<string, Lifetime.Lifetime>,
    output: boolean,
    defaultOutput: Lifetime.Lifetime | undefined,
    allocate: (node: SyntaxTree.Node) => Lifetime.Lifetime | undefined,
  ): void => {
    const binders =
      nominalParameters?.(path)?.filter((parameter) => parameter.kind === 'Lifetime') ?? []
    if (binders.length === 0) return
    const list = SyntaxTree.directNode(node, 'TypeArgumentList')
    if (list !== undefined && SyntaxTree.directNodes(list, 'LifetimeType').length > 0) return
    const arguments_ = binders.flatMap(() => {
      const value = region(node, scope, output, defaultOutput, allocate)
      return value === undefined ? [] : [value]
    })
    if (arguments_.length === binders.length) nominalArguments.set(node, Object.freeze(arguments_))
  }
  const walk = (
    node: SyntaxTree.Node,
    scope: ReadonlyMap<string, Lifetime.Lifetime>,
    output = false,
    defaultOutput?: Lifetime.Lifetime,
    allocate: (node: SyntaxTree.Node) => Lifetime.Lifetime | undefined = fresh,
    quantified = false,
  ): void => {
    if (node.kind === 'Block' || node.kind === 'TypeParameterList') return
    if (
      node !== syntax &&
      (node.kind === 'ServiceOperation' ||
        node.kind === 'FunctionDeclaration' ||
        node.kind === 'ImplOperation')
    )
      return
    if (node.kind === 'LifetimeType' || node.kind === 'EffectEnvironment') {
      const token = SyntaxTree.directToken(node, 'Lifetime')
      if (token !== undefined) resolve(node, token, scope)
      return
    }
    if (node.kind === 'CallableType' || node.kind === 'ForeignFunctionType') {
      const annotation = SyntaxTree.directNode(node, 'CallableEnvironment')
      const environment =
        node.kind === 'ForeignFunctionType'
          ? Lifetime.staticLifetime
          : region(annotation ?? node, scope, output, defaultOutput, allocate)
      if (environment !== undefined) regions.set(node, environment)
      const binderPath = [callableOrdinal++]
      const scopeBindings = new Map(scope)
      const binders: Array<Lifetime.Bound> = []
      const lists = SyntaxTree.directNodes(node, 'LifetimeBinderList')
      if (lists.length > 1 || (quantified && lists.length > 0))
        diagnostics.push(
          Diagnostic.invalidLifetimeBinder(
            'Nested quantified callable contracts are not supported',
            node.span,
          ),
        )
      for (const list of lists) {
        for (const parameter of list.children.filter(SyntaxTree.isNode)) {
          const token = SyntaxTree.directToken(parameter, 'Lifetime')
          if (token === undefined || parameter.kind !== 'LifetimeParameter') {
            diagnostics.push(
              Diagnostic.invalidLifetimeBinder(
                'A callable lifetime binder accepts only lifetime parameters',
                parameter.span,
              ),
            )
            continue
          }
          const name = spelling(source, token)
          if (name === "'static" || binders.some((binder) => Lifetime.display(binder) === name)) {
            diagnostics.push(
              Diagnostic.invalidLifetimeBinder(
                'Lifetime binders must have distinct names other than static',
                token.span,
              ),
            )
            continue
          }
          const binder = Lifetime.bound(owner, binders.length, name, binderPath)
          binders.push(binder)
          scopeBindings.set(name, binder)
        }
      }
      const lifetimeBounds: Array<Lifetime.Outlives> = []
      for (const list of lists)
        for (const parameter of list.children.filter(SyntaxTree.isNode)) {
          const token = SyntaxTree.directToken(parameter, 'Lifetime')
          const longer =
            token === undefined ? undefined : scopeBindings.get(spelling(source, token))
          if (longer === undefined) continue
          for (const bound of SyntaxTree.directNodes(parameter, 'LifetimeType')) {
            const token = SyntaxTree.directToken(bound, 'Lifetime')
            const shorter = token === undefined ? undefined : resolve(bound, token, scopeBindings)
            if (shorter !== undefined) lifetimeBounds.push({ longer, shorter })
          }
        }
      const invocationRegion = (_node: SyntaxTree.Node): Lifetime.Lifetime => {
        let name = `call${binders.length}`
        while (scopeBindings.has(`'${name}`)) name += '_'
        const binder = Lifetime.bound(owner, binders.length, name, binderPath)
        scopeBindings.set(`'${name}`, binder)
        binders.push(binder)
        return binder
      }
      const types = node.children.filter(
        (child): child is SyntaxTree.Node =>
          SyntaxTree.isNode(child) &&
          child.kind !== 'LifetimeBinderList' &&
          child.kind !== 'CallableEnvironment' &&
          child.kind !== 'FunctionPropertyClause',
      )
      const inputs = types.slice(0, -1)
      for (const input of inputs)
        walk(input, scopeBindings, false, undefined, invocationRegion, true)
      const candidates = inputs.flatMap((input) => {
        const value = regions.get(input)
        return value === undefined ? [] : [value]
      })
      const result = types.at(-1)
      if (result !== undefined)
        walk(
          result,
          scopeBindings,
          true,
          candidates.length === 1 ? candidates.at(0) : undefined,
          invocationRegion,
          true,
        )
      if (quantified && lists.length === 0 && binders.length > 0)
        diagnostics.push(
          Diagnostic.invalidLifetimeBinder(
            'Nested quantified callable contracts are not supported',
            node.span,
          ),
        )
      if (environment !== undefined)
        callables.set(
          node,
          Object.freeze({
            environment,
            lifetimeBinders: Object.freeze(binders),
            lifetimeBounds: Lifetime.assumptions(lifetimeBounds).bounds,
          }),
        )
      return
    }
    if (node.kind === 'ReferenceType' || node.kind === 'SliceType' || isString(source, node))
      region(node, scope, output, defaultOutput, allocate)
    if (node.kind === 'AppliedType') {
      const path = SyntaxTree.directNode(node, 'TypePath')
      const list = SyntaxTree.directNode(node, 'TypeArgumentList')
      const pathToken = path === undefined ? undefined : SyntaxTree.directToken(path, 'Identifier')
      const builtin = pathToken === undefined ? undefined : spelling(source, pathToken)
      if (list !== undefined && (builtin === 'Effect' || builtin === 'string')) {
        const annotation =
          builtin === 'Effect'
            ? SyntaxTree.directNode(list, 'EffectEnvironment')
            : SyntaxTree.directNode(list, 'LifetimeType')
        const value =
          annotation === undefined
            ? region(node, scope, output, defaultOutput, allocate)
            : region(annotation, scope, output, defaultOutput, allocate)
        if (value !== undefined) regions.set(node, value)
        for (const child of list.children.filter(SyntaxTree.isNode))
          if (child !== annotation) walk(child, scope, output, defaultOutput, allocate, quantified)
        return
      }
      if (path !== undefined) nominal(node, path, scope, output, defaultOutput, allocate)
      // The path names a nominal; it is not itself a separate borrowed occurrence.
      if (list !== undefined) walk(list, scope, output, defaultOutput, allocate, quantified)
      return
    }
    if (node.kind === 'TypePath' && !isString(source, node))
      nominal(node, node, scope, output, defaultOutput, allocate)
    for (const child of node.children.filter(SyntaxTree.isNode))
      walk(child, scope, output, defaultOutput, allocate, quantified)
  }

  const candidate = (node: SyntaxTree.Node): boolean =>
    node.kind === 'ReferenceType' ||
    node.kind === 'SliceType' ||
    isString(source, node) ||
    (node.kind === 'AppliedType' &&
      (() => {
        const path = SyntaxTree.directNode(node, 'TypePath')
        return path !== undefined && isString(source, path)
      })())
  const parameterList = SyntaxTree.directNode(syntax, 'ParameterList')
  const genericList = SyntaxTree.directNode(syntax, 'TypeParameterList')
  if (genericList !== undefined)
    for (const parameter of genericList.children.filter(SyntaxTree.isNode))
      for (const bound of parameter.children.filter(SyntaxTree.isNode)) walk(bound, bindings)
  if (parameterList === undefined || body !== undefined) walk(syntax, bindings)
  else {
    const inputs = SyntaxTree.directNodes(parameterList, 'ParameterDeclaration')
    const candidates: Array<Lifetime.Lifetime> = []
    let receiver: Lifetime.Lifetime | undefined
    for (const input of inputs) {
      const type = typeChild(input)
      if (type === undefined) continue
      walk(type, bindings)
      const value = regions.get(type)
      if (value === undefined) continue
      if (candidate(type)) candidates.push(value)
      const token = SyntaxTree.directToken(input, 'Identifier')
      if (candidate(type) && token !== undefined && spelling(source, token) === 'self')
        receiver = value
    }
    const result = SyntaxTree.directNode(syntax, 'ReturnType')
    if (result !== undefined)
      walk(
        result,
        bindings,
        true,
        receiver ?? (candidates.length === 1 ? candidates.at(0) : undefined),
      )
    for (const kind of ['FailureRow', 'RequirementRow'] as const) {
      const row = SyntaxTree.directNode(syntax, kind)
      if (row !== undefined)
        walk(
          row,
          bindings,
          true,
          receiver ?? (candidates.length === 1 ? candidates.at(0) : undefined),
        )
    }
  }
  const environmentNode = SyntaxTree.directNode(syntax, 'EffectEnvironment')
  const environmentToken =
    environmentNode === undefined ? undefined : SyntaxTree.directToken(environmentNode, 'Lifetime')
  const explicitEnvironment =
    environmentNode !== undefined && environmentToken !== undefined
      ? resolve(environmentNode, environmentToken, bindings)
      : undefined
  return Object.freeze({
    owner,
    source,
    syntax,
    ...(explicitEnvironment === undefined ? {} : { explicitEnvironment }),
    parameters: new Map(parameters),
    nominalArguments,
    regions,
    callables,
    implicit: Object.freeze(implicit),
    diagnostics: Object.freeze(diagnostics),
  })
}

/** Assigns local annotation variables in the enclosing body's canonical declaration scope. */
export const forBody = (
  source: SourceFile.SourceFile,
  body: BodyLifetime.BodyLifetime,
  syntax: SyntaxTree.Node,
  parameters: ReadonlyMap<string, Type.Parameter>,
): Context => forHeader(source, body.owner, syntax, parameters, body)
