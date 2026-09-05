import * as Lifetime from './Lifetime.js'
import * as SyntaxTree from './SyntaxTree.js'
import * as Type from './Type.js'
import * as TypeCompatibility from './TypeCompatibility.js'

/** A declaration-local, finite syntax domain shared by annotation and expression inference. */
export interface BodyLifetime {
  readonly owner: Lifetime.Owner
  readonly points: ReadonlyMap<SyntaxTree.Node, number>
  readonly constraints: Map<string, Lifetime.Outlives>
  readonly parameterBounds: ReadonlyMap<string, ReadonlyArray<Lifetime.Lifetime>>
  readonly genericStorage: Map<
    string,
    { readonly lifetime: Lifetime.Local; readonly parameter: Type.Parameter }
  >
}

/** Assigns stable preorder points once; whitespace and preceding declarations have no effect. */
export const make = (
  owner: Lifetime.Owner,
  body: SyntaxTree.Node,
  parameterBounds: ReadonlyMap<string, ReadonlyArray<Lifetime.Lifetime>> = new Map(),
): BodyLifetime => {
  const points = new Map<SyntaxTree.Node, number>()
  const pending = [body]
  while (pending.length > 0) {
    const node = pending.pop()
    if (node === undefined) break
    points.set(node, points.size)
    const children = node.children.filter(SyntaxTree.isNode)
    for (let ordinal = children.length - 1; ordinal >= 0; ordinal -= 1) {
      const child = children.at(ordinal)
      if (child !== undefined) pending.push(child)
    }
  }
  return {
    owner: Object.freeze({ ...owner }),
    points,
    constraints: new Map(),
    parameterBounds,
    genericStorage: new Map(),
  }
}

/** Allocates an occurrence region only inside the already registered declaration domain. */
export const region = (
  self: BodyLifetime,
  node: SyntaxTree.Node,
  role: 'Borrow' | 'Annotation' | 'Environment' | 'Call',
  binderOrdinal = 0,
): Lifetime.Local | undefined => {
  const ordinal = self.points.get(node)
  return ordinal === undefined
    ? undefined
    : Lifetime.local(self.owner, `${role}:${binderOrdinal}`, ordinal)
}

/** Creates one body comparison cache; local obligations are retained for its finite region solve. */
export const compatibility = (
  self: BodyLifetime,
  assumptions: Lifetime.Assumptions,
  nominalVariance: ReadonlyMap<string, ReadonlyArray<TypeCompatibility.Variance>> = new Map(),
): TypeCompatibility.Context => {
  const proves = (longer: Lifetime.Lifetime, shorter: Lifetime.Lifetime): boolean => {
    if (Lifetime.outlives(assumptions, longer, shorter)) return true
    if (longer._tag === 'PlaceholderLifetime' || shorter._tag === 'PlaceholderLifetime')
      return false
    if (longer._tag !== 'LocalLifetime' && shorter._tag !== 'LocalLifetime') return false
    return true
  }
  return TypeCompatibility.context({
    assumptions,
    nominalVariance,
    outlives: proves,
    commitOutlives: (longer, shorter) => constrain(self, longer, shorter),
    typeOutlives: (type, lifetime) => {
      const parameters = Type.storageParameters(type)
      const bounds = parameters.flatMap((parameter) =>
        (self.parameterBounds.get(Type.key(parameter)) ?? []).map((region) => ({
          type: parameter,
          lifetime: region,
        })),
      )
      if (
        Type.satisfiesOutlives(type, lifetime, bounds, (longer, shorter) =>
          Lifetime.outlives(assumptions, longer, shorter),
        )
      )
        return true
      if (lifetime._tag === 'PlaceholderLifetime') return false
      if (!Type.storageLifetimes(type).every((region) => proves(region, lifetime))) return false
      return true
    },
    commitTypeOutlives: (type, lifetime) => {
      for (const region of Type.storageLifetimes(type)) constrain(self, region, lifetime)
      for (const parameter of Type.storageParameters(type)) {
        if (parameter.staticProperties.includes('Intrinsic.Detached')) continue
        constrain(self, genericRegion(self, parameter), lifetime)
      }
    },
  })
}

const genericRegion = (self: BodyLifetime, parameter: Type.Parameter): Lifetime.Local => {
  const identity = Type.key(parameter)
  const previous = self.genericStorage.get(identity)
  if (previous !== undefined) return previous.lifetime
  const lifetime = Lifetime.local(self.owner, `GenericStorage:${identity}`, 0)
  self.genericStorage.set(identity, { lifetime, parameter })
  return lifetime
}

/** Retains one selected compatibility obligation without starting resolution or code emission. */
export const constrain = (
  self: BodyLifetime,
  longer: Lifetime.Lifetime,
  shorter: Lifetime.Lifetime,
): void => {
  const bound = { longer, shorter }
  const normalized = Lifetime.assumptions([bound])
  self.constraints.set(normalized.key, Object.freeze(bound))
}

/** Derives environment validity from every retained semantic dependency, including nested views. */
export const environment = (
  self: BodyLifetime | undefined,
  node: SyntaxTree.Node,
  retained: ReadonlyArray<Type.Type>,
  borrowed: ReadonlyArray<SyntaxTree.Node> = [],
): Type.ExecutableLifetimes | undefined => {
  const dependencies: Array<Lifetime.Lifetime> = [
    ...new Map(
      retained.flatMap(Type.storageLifetimes).map((lifetime) => [Lifetime.key(lifetime), lifetime]),
    ).values(),
  ].filter((lifetime) => lifetime._tag !== 'StaticLifetime')
  const retainedParameters = retained
    .flatMap(Type.storageParameters)
    .filter((parameter) => !parameter.staticProperties.includes('Intrinsic.Detached'))
  for (const parameter of retainedParameters) {
    if (self === undefined) return undefined
    const bounds = self.parameterBounds.get(Type.key(parameter)) ?? []
    if (bounds.some((bound) => bound._tag === 'StaticLifetime')) continue
    dependencies.push(genericRegion(self, parameter))
  }
  if (dependencies.length === 0 && borrowed.length === 0)
    return {
      environment: Lifetime.staticLifetime,
      lifetimeBinders: [],
      lifetimeBounds: [],
    }
  if (self === undefined) return undefined
  for (const occurrence of borrowed) {
    const dependency = region(self, occurrence, 'Borrow')
    if (dependency === undefined) return undefined
    dependencies.push(dependency)
  }
  const lifetime = region(self, node, 'Environment')
  if (lifetime === undefined) return undefined
  const lifetimeBounds = dependencies.map((longer) => ({ longer, shorter: lifetime }))
  for (const bound of lifetimeBounds) constrain(self, bound.longer, bound.shorter)
  return {
    environment: lifetime,
    lifetimeBinders: [],
    lifetimeBounds,
    typeOutlives: retainedParameters.map((type) => ({ type, lifetime })),
  }
}
