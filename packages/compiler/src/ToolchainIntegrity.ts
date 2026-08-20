import type * as Instances from './Instances.js'
import * as Intrinsic from './Intrinsic.js'
import * as Stdlib from './Stdlib.js'
import { compilerDigest } from './ToolchainIntegrity.generated.js'

/** The stable schema of a matched bootstrap toolchain identity graph. */
export const schema = 'silk-toolchain-v1' as const

export type ComponentKind =
  | 'Compiler'
  | 'Catalog'
  | 'Source'
  | 'IntrinsicInventory'
  | 'Provider'
  | 'RuntimeSupport'

/** One content-addressed component and the identities it was generated against. */
export interface Component {
  readonly kind: ComponentKind
  readonly id: string
  readonly digest: string
  readonly dependencies: ReadonlyArray<string>
}

/** The immutable matched-set metadata shipped by one compiler distribution. */
export interface Graph {
  readonly _tag: 'ToolchainIdentityGraph'
  readonly schema: typeof schema
  readonly digest: string
  readonly components: ReadonlyArray<Component>
}

export interface IntegrityFailure {
  readonly _tag: 'ToolchainIntegrityFailure'
  readonly boundary: 'Frontend' | 'Target'
  readonly reason:
    | { readonly _tag: 'MalformedGraph'; readonly detail: string }
    | { readonly _tag: 'MissingComponent'; readonly kind: ComponentKind; readonly id: string }
    | { readonly _tag: 'UnexpectedComponent'; readonly kind: ComponentKind; readonly id: string }
    | {
        readonly _tag: 'UnreadableComponent'
        readonly kind: ComponentKind
        readonly id: string
        readonly detail: string
      }
    | {
        readonly _tag: 'DigestMismatch'
        readonly kind: ComponentKind
        readonly id: string
        readonly expected: string
        readonly observed: string
      }
}

export type Validation =
  | { readonly _tag: 'Matched'; readonly graph: Graph }
  | { readonly _tag: 'Invalid'; readonly failures: ReadonlyArray<IntegrityFailure> }

export type TargetValidation =
  | {
      readonly _tag: 'Matched'
      readonly providers: ReadonlyArray<Component>
      readonly runtimeSupport: ReadonlyArray<Component>
    }
  | {
      readonly _tag: 'UnsupportedTarget'
      readonly target: Intrinsic.ExecutionTarget
      readonly operations: ReadonlyArray<string>
    }
  | { readonly _tag: 'Invalid'; readonly failures: ReadonlyArray<IntegrityFailure> }

const sha256Constants = Object.freeze([
  0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
  0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
  0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
  0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
  0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
  0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
  0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
  0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
])

const rotateRight = (value: number, count: number): number =>
  (value >>> count) | (value << (32 - count))

/** Computes the path- and timestamp-independent SHA-256 identity of exact bytes. */
export const contentDigest = (value: Uint8Array | string): string => {
  const source = typeof value === 'string' ? new TextEncoder().encode(value) : value
  const bitLength = BigInt(source.length) * 8n
  const paddedLength = Math.ceil((source.length + 9) / 64) * 64
  const bytes = new Uint8Array(paddedLength)
  bytes.set(source)
  bytes[source.length] = 0x80
  for (let index = 0; index < 8; index += 1)
    bytes[paddedLength - 1 - index] = Number((bitLength >> BigInt(index * 8)) & 0xffn)

  const state = [
    0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19,
  ]
  const words = new Uint32Array(64)
  for (let offset = 0; offset < bytes.length; offset += 64) {
    for (let index = 0; index < 16; index += 1) {
      const at = offset + index * 4
      words[index] =
        ((bytes[at] ?? 0) << 24) |
        ((bytes[at + 1] ?? 0) << 16) |
        ((bytes[at + 2] ?? 0) << 8) |
        (bytes[at + 3] ?? 0)
    }
    for (let index = 16; index < 64; index += 1) {
      const prior15 = words[index - 15] ?? 0
      const prior2 = words[index - 2] ?? 0
      const sigma0 = rotateRight(prior15, 7) ^ rotateRight(prior15, 18) ^ (prior15 >>> 3)
      const sigma1 = rotateRight(prior2, 17) ^ rotateRight(prior2, 19) ^ (prior2 >>> 10)
      words[index] = ((words[index - 16] ?? 0) + sigma0 + (words[index - 7] ?? 0) + sigma1) >>> 0
    }
    let [a, b, c, d, e, f, g, h] = state
    for (let index = 0; index < 64; index += 1) {
      const upper1 = rotateRight(e ?? 0, 6) ^ rotateRight(e ?? 0, 11) ^ rotateRight(e ?? 0, 25)
      const choice = ((e ?? 0) & (f ?? 0)) ^ (~(e ?? 0) & (g ?? 0))
      const temporary1 =
        ((h ?? 0) + upper1 + choice + (sha256Constants[index] ?? 0) + (words[index] ?? 0)) >>> 0
      const upper0 = rotateRight(a ?? 0, 2) ^ rotateRight(a ?? 0, 13) ^ rotateRight(a ?? 0, 22)
      const majority = ((a ?? 0) & (b ?? 0)) ^ ((a ?? 0) & (c ?? 0)) ^ ((b ?? 0) & (c ?? 0))
      const temporary2 = (upper0 + majority) >>> 0
      h = g
      g = f
      f = e
      e = ((d ?? 0) + temporary1) >>> 0
      d = c
      c = b
      b = a
      a = (temporary1 + temporary2) >>> 0
    }
    state[0] = ((state[0] ?? 0) + (a ?? 0)) >>> 0
    state[1] = ((state[1] ?? 0) + (b ?? 0)) >>> 0
    state[2] = ((state[2] ?? 0) + (c ?? 0)) >>> 0
    state[3] = ((state[3] ?? 0) + (d ?? 0)) >>> 0
    state[4] = ((state[4] ?? 0) + (e ?? 0)) >>> 0
    state[5] = ((state[5] ?? 0) + (f ?? 0)) >>> 0
    state[6] = ((state[6] ?? 0) + (g ?? 0)) >>> 0
    state[7] = ((state[7] ?? 0) + (h ?? 0)) >>> 0
  }
  return state.map((word) => word.toString(16).padStart(8, '0')).join('')
}

const field = (value: string): string => `${new TextEncoder().encode(value).length}:${value}`

const encodeComponent = (self: Component): string =>
  [self.kind, self.id, self.digest, ...self.dependencies].map(field).join('')

const compareComponent = (left: Component, right: Component): number =>
  left.kind.localeCompare(right.kind) || left.id.localeCompare(right.id)

const graphDigest = (components: ReadonlyArray<Component>): string =>
  contentDigest([schema, ...components.map(encodeComponent)].map(field).join(''))

/** Builds a normalized graph, primarily for distribution generation and embedding tests. */
export const make = (components: Iterable<Component>): Graph => {
  const normalized = Object.freeze(
    [...components]
      .map((component) =>
        Object.freeze({
          ...component,
          dependencies: Object.freeze([...component.dependencies].sort()),
        }),
      )
      .sort(compareComponent),
  )
  return Object.freeze({
    _tag: 'ToolchainIdentityGraph',
    schema,
    digest: graphDigest(normalized),
    components: normalized,
  })
}

const catalogDigest = (): string =>
  contentDigest(
    Stdlib.manifest
      .map((entry) =>
        [
          entry.module,
          entry.digest,
          entry.layer,
          ...(entry.providerTargets ?? []),
          ...entry.runtimeInventory,
          entry.namespace ?? '',
          ...(entry.aliases ?? []),
        ]
          .map(field)
          .join(''),
      )
      .join(''),
  )

const inventoryDigest = (): string => contentDigest(JSON.stringify(Intrinsic.inventory()))

const sourceId = (module: string): string => `source/${module}`
const providerId = (module: string): string => `provider/${module}`
const runtimeId = (target: Intrinsic.ExecutionTarget, operation: string): string =>
  `runtime/${target}/${operation}`

const installedComponents = (): ReadonlyArray<Component> => {
  const compiler: Component = Object.freeze({
    kind: 'Compiler',
    id: '@silk-effect/compiler',
    digest: compilerDigest,
    dependencies: Object.freeze([]),
  })
  const catalog: Component = Object.freeze({
    kind: 'Catalog',
    id: 'silk/stdlib/catalog',
    digest: catalogDigest(),
    dependencies: Object.freeze([compiler.id]),
  })
  const intrinsic: Component = Object.freeze({
    kind: 'IntrinsicInventory',
    id: 'silk/intrinsic/inventory',
    digest: inventoryDigest(),
    dependencies: Object.freeze([compiler.id]),
  })
  const sources = Stdlib.manifest.map<Component>((entry) =>
    Object.freeze({
      kind: 'Source',
      id: sourceId(entry.module),
      digest: entry.digest,
      dependencies: Object.freeze([catalog.id]),
    }),
  )
  const providers = Stdlib.manifest.flatMap<Component>((entry) =>
    entry.layer === 'portable'
      ? []
      : [
          Object.freeze({
            kind: 'Provider',
            id: providerId(entry.module),
            digest: contentDigest(
              [
                entry.module,
                entry.digest,
                ...(entry.providerTargets ?? []),
                ...entry.runtimeInventory,
              ]
                .map(field)
                .join(''),
            ),
            dependencies: Object.freeze([sourceId(entry.module), intrinsic.id]),
          }),
        ],
  )
  const runtime = Intrinsic.inventory().flatMap<Component>((entry) =>
    entry.targets.map((target) =>
      Object.freeze({
        kind: 'RuntimeSupport',
        id: runtimeId(target, entry.operation),
        digest: contentDigest(
          JSON.stringify({
            target,
            operation: entry.operation,
            evaluator: entry.evaluator,
            hir: entry.hir,
            mir: entry.mir,
            ...(entry.hostImport === undefined ? {} : { hostImport: entry.hostImport }),
          }),
        ),
        dependencies: Object.freeze([intrinsic.id]),
      }),
    ),
  )
  return Object.freeze([compiler, catalog, intrinsic, ...sources, ...providers, ...runtime])
}

const installedGraph = make(installedComponents())

/** Returns the identity graph embedded in this compiler build. */
export const installed = (): Graph => installedGraph

const failure = (
  boundary: IntegrityFailure['boundary'],
  reason: IntegrityFailure['reason'],
): IntegrityFailure => Object.freeze({ _tag: 'ToolchainIntegrityFailure', boundary, reason })

/** Classifies failure to read one promised packaged source at the distribution boundary. */
export const unreadableSource = (module: string, detail: string): IntegrityFailure =>
  failure('Frontend', {
    _tag: 'UnreadableComponent',
    kind: 'Source',
    id: sourceId(module),
    detail,
  })

const componentKey = (component: Pick<Component, 'kind' | 'id'>): string =>
  `${component.kind}:${component.id}`

const validateShape = (
  candidate: Graph,
  boundary: IntegrityFailure['boundary'],
): IntegrityFailure[] => {
  const failures: IntegrityFailure[] = []
  if (candidate._tag !== 'ToolchainIdentityGraph' || candidate.schema !== schema)
    failures.push(failure(boundary, { _tag: 'MalformedGraph', detail: 'unsupported graph schema' }))
  const keys = candidate.components.map(componentKey)
  if (new Set(keys).size !== keys.length)
    failures.push(
      failure(boundary, { _tag: 'MalformedGraph', detail: 'duplicate component identity' }),
    )
  const normalized = [...candidate.components].sort(compareComponent)
  if (normalized.some((component, index) => componentKey(component) !== keys[index]))
    failures.push(
      failure(boundary, { _tag: 'MalformedGraph', detail: 'components are not normalized' }),
    )
  const observedDigest = graphDigest(candidate.components)
  if (observedDigest !== candidate.digest)
    failures.push(
      failure(boundary, {
        _tag: 'DigestMismatch',
        kind: 'Compiler',
        id: 'toolchain-identity-graph',
        expected: observedDigest,
        observed: candidate.digest,
      }),
    )
  return failures
}

const compareSelection = (
  candidate: Graph,
  kinds: ReadonlySet<ComponentKind>,
  boundary: IntegrityFailure['boundary'],
): IntegrityFailure[] => {
  const expected = new Map(
    installedGraph.components
      .filter((component) => kinds.has(component.kind))
      .map((component) => [componentKey(component), component] as const),
  )
  const observed = new Map(
    candidate.components
      .filter((component) => kinds.has(component.kind))
      .map((component) => [componentKey(component), component] as const),
  )
  const failures: IntegrityFailure[] = []
  for (const [key, component] of expected) {
    const found = observed.get(key)
    if (found === undefined) {
      failures.push(
        failure(boundary, { _tag: 'MissingComponent', kind: component.kind, id: component.id }),
      )
    } else if (found.digest !== component.digest) {
      failures.push(
        failure(boundary, {
          _tag: 'DigestMismatch',
          kind: component.kind,
          id: component.id,
          expected: component.digest,
          observed: found.digest,
        }),
      )
    }
  }
  for (const [key, component] of observed)
    if (!expected.has(key))
      failures.push(
        failure(boundary, { _tag: 'UnexpectedComponent', kind: component.kind, id: component.id }),
      )
  return failures
}

/** Validates the compiler/catalog/source/intrinsic set before user source resolution begins. */
export const validateFrontend = (
  candidate: Graph,
  sources: ReadonlyMap<string, Uint8Array> = Stdlib.sources,
): Validation => {
  const failures = [
    ...validateShape(candidate, 'Frontend'),
    ...compareSelection(
      candidate,
      new Set<ComponentKind>(['Compiler', 'Catalog', 'Source', 'IntrinsicInventory']),
      'Frontend',
    ),
  ]
  for (const entry of Stdlib.manifest) {
    const bytes = sources.get(entry.module)
    if (bytes === undefined) {
      failures.push(
        failure('Frontend', {
          _tag: 'MissingComponent',
          kind: 'Source',
          id: sourceId(entry.module),
        }),
      )
      continue
    }
    const observed = contentDigest(bytes)
    if (observed !== entry.digest)
      failures.push(
        failure('Frontend', {
          _tag: 'DigestMismatch',
          kind: 'Source',
          id: sourceId(entry.module),
          expected: entry.digest,
          observed,
        }),
      )
  }
  for (const module of sources.keys())
    if (Stdlib.find(module) === undefined)
      failures.push(
        failure('Frontend', { _tag: 'UnexpectedComponent', kind: 'Source', id: sourceId(module) }),
      )
  return failures.length === 0
    ? Object.freeze({ _tag: 'Matched', graph: candidate })
    : Object.freeze({ _tag: 'Invalid', failures: Object.freeze(failures) })
}

/** Validates only providers and runtime implementations reached by one prepared program. */
export const validateTarget = (
  candidate: Graph,
  target: Intrinsic.ExecutionTarget,
  calls: ReadonlyArray<Instances.IntrinsicCall>,
  loadedModules: Iterable<string>,
): TargetValidation => {
  const unsupported = [
    ...new Set(
      calls.flatMap((call) => {
        const operation = Intrinsic.findOperationById(call.operation)
        return operation === undefined || operation.targets.includes(target)
          ? []
          : [Intrinsic.operationText(call.operation)]
      }),
    ),
  ].sort()
  const providerModules = [...new Set(loadedModules)]
    .filter((module) => Stdlib.find(module)?.layer === 'target-provider')
    .sort()
  for (const module of providerModules) {
    const entry = Stdlib.find(module)
    if (entry !== undefined && !entry.providerTargets?.includes(target)) unsupported.push(module)
  }
  if (unsupported.length > 0)
    return Object.freeze({
      _tag: 'UnsupportedTarget',
      target,
      operations: Object.freeze([...new Set(unsupported)].sort()),
    })

  const selectedIds = new Set([
    ...providerModules.map(providerId),
    ...calls.map((call) => runtimeId(target, Intrinsic.operationText(call.operation))),
  ])
  const expected = new Map(
    installedGraph.components
      .filter(
        (component) =>
          (component.kind === 'Provider' || component.kind === 'RuntimeSupport') &&
          selectedIds.has(component.id),
      )
      .map((component) => [componentKey(component), component] as const),
  )
  const observed = new Map(
    candidate.components
      .filter(
        (component) =>
          (component.kind === 'Provider' || component.kind === 'RuntimeSupport') &&
          selectedIds.has(component.id),
      )
      .map((component) => [componentKey(component), component] as const),
  )
  const failures: IntegrityFailure[] = [...validateShape(candidate, 'Target')]
  for (const [key, component] of expected) {
    const found = observed.get(key)
    if (found === undefined)
      failures.push(
        failure('Target', { _tag: 'MissingComponent', kind: component.kind, id: component.id }),
      )
    else if (found.digest !== component.digest)
      failures.push(
        failure('Target', {
          _tag: 'DigestMismatch',
          kind: component.kind,
          id: component.id,
          expected: component.digest,
          observed: found.digest,
        }),
      )
  }
  if (failures.length > 0)
    return Object.freeze({ _tag: 'Invalid', failures: Object.freeze(failures) })
  return Object.freeze({
    _tag: 'Matched',
    providers: Object.freeze([...expected.values()].filter((entry) => entry.kind === 'Provider')),
    runtimeSupport: Object.freeze(
      [...expected.values()].filter((entry) => entry.kind === 'RuntimeSupport'),
    ),
  })
}

/** Stable human-readable detail for CLI and embedding diagnostics. */
export const formatFailure = (self: IntegrityFailure): string => {
  switch (self.reason._tag) {
    case 'MalformedGraph':
      return `malformed identity graph: ${self.reason.detail}`
    case 'MissingComponent':
      return `missing ${self.reason.kind} ${self.reason.id}`
    case 'UnexpectedComponent':
      return `unexpected ${self.reason.kind} ${self.reason.id}`
    case 'UnreadableComponent':
      return `cannot read ${self.reason.kind} ${self.reason.id}: ${self.reason.detail}`
    case 'DigestMismatch':
      return `${self.reason.kind} ${self.reason.id} digest mismatch: expected ${self.reason.expected}, observed ${self.reason.observed}`
  }
}
