import * as Option from 'effect/Option'
import * as SourceFile from './SourceFile.js'
import { modules } from './Stdlib.generated.js'
import type * as Token from './Token.js'

/** The reserved namespace prefix. User resolvers are never consulted for these identities. */
export const namespacePrefix = 'silk/'

/** Tests whether a module identity claims the reserved standard-library namespace. */
export const isReserved = (module: string): boolean =>
  module === 'silk' || module.startsWith(namespacePrefix)

/** One generated view of a canonical, compiler-shipped Silk source file. */
export interface Module {
  readonly module: string
  readonly path: string
  readonly namespace?: string
  readonly aliases?: ReadonlyArray<string>
  readonly sourceUrl: URL
  readonly bytes: Uint8Array
}

const encoder = new TextEncoder()

/** The deterministic standard-library manifest, ordered by canonical module identity. */
export const manifest: ReadonlyArray<Module> = Object.freeze(
  modules.map((entry) =>
    Object.freeze({
      module: entry.module,
      path: entry.path,
      ...('namespace' in entry ? { namespace: entry.namespace } : {}),
      ...('aliases' in entry ? { aliases: entry.aliases } : {}),
      sourceUrl: new URL(`../stdlib/${entry.path}`, import.meta.url),
      bytes: encoder.encode(entry.source),
    }),
  ),
)

const byModule = new Map(manifest.map((entry) => [entry.module, entry] as const))

const byNamespace = new Map(
  manifest.flatMap((entry) =>
    [entry.namespace, ...(entry.aliases ?? [])].flatMap((namespace) =>
      namespace === undefined ? [] : [[namespace, entry] as const],
    ),
  ),
)

/** Returns the generated manifest entry for one standard-library module identity. */
export const find = (module: string): Module | undefined => byModule.get(module)

/** Resolves one canonical source-backed prelude namespace. */
export const findNamespace = (namespace: string): Module | undefined => byNamespace.get(namespace)

/**
 * Token kinds whose bytes are inert prose rather than code: comment trivia and static-literal
 * bodies. A namespace spelled inside one of these never names a qualified actor.
 */
const inertKinds: ReadonlySet<Token.TokenKind> = new Set<Token.TokenKind>([
  'LineComment',
  'DocComment',
  'ModuleDocComment',
  'TextLiteral',
  'ByteStringLiteral',
  'InvalidStaticLiteral',
])

/**
 * Finds source-backed namespaces referenced as qualified actors in one lexed source file.
 *
 * The scan is lexical, never textual: a namespace counts only when one code token spells it
 * exactly and the very next token is a dot, so `Result.` written inside a comment or a text
 * literal never pulls `silk/result` into the closure. Trivia is not skipped between the two
 * tokens, which keeps `Namespace .member` outside the closure exactly as the byte scan did.
 * One linear pass over the token stream; the manifest is walked once to order the answer.
 */
export const requiredModules = (
  source: SourceFile.SourceFile,
  tokens: ReadonlyArray<Token.Token>,
): ReadonlyArray<string> => {
  const required = new Set<string>()
  for (let index = 0; index + 1 < tokens.length; index += 1) {
    const token = tokens[index]
    const next = tokens[index + 1]
    if (token === undefined || next === undefined) continue
    if (next.kind !== 'Dot' && next.kind !== 'DotDot') continue
    if (inertKinds.has(token.kind)) continue
    const spelling = Option.getOrUndefined(SourceFile.spelling(source, token.span))
    const entry = spelling === undefined ? undefined : byNamespace.get(spelling)
    if (entry !== undefined) required.add(entry.module)
  }
  return Object.freeze(
    manifest.flatMap((entry) => (required.has(entry.module) ? [entry.module] : [])),
  )
}

/** Every standard-library module's exact source bytes by canonical identity. */
export const sources: ReadonlyMap<string, Uint8Array> = new Map(
  manifest.map((entry) => [entry.module, Uint8Array.from(entry.bytes)] as const),
)
