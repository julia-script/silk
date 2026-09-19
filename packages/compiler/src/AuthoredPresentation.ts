import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'
import * as AuthoredIdentity from './AuthoredIdentity.js'

/** Current byte coordinates, deliberately absent from authored semantic artifacts. */
export interface Span {
  readonly start: number
  readonly end: number
}

/** Revision-specific display data for an owner-local authored anchor. */
export interface Entry {
  readonly anchor: AuthoredIdentity.Anchor
  readonly span: Span
  readonly spelling?: string
  readonly trivia?: string
  readonly documentation?: string
}

/** Rendering information for a local recovery cause; ordinal IDs are not semantic content. */
export interface Diagnostic {
  readonly anchor: AuthoredIdentity.Anchor
  readonly span: Span
  readonly code: string
  readonly message: string
}

export interface Presentation {
  readonly _tag: 'AuthoredPresentation'
  readonly sourceId: string
  readonly revision: string
  readonly entries: readonly Entry[]
  readonly diagnostics: readonly Diagnostic[]
}

export class AuthoredPresentationError extends Data.TaggedError('AuthoredPresentationError')<{
  readonly reason: { readonly _tag: 'InvalidSpan'; readonly span: Span }
}> {}

const span = Effect.fnUntraced(function* (
  value: Span,
): Effect.fn.Return<Span, AuthoredPresentationError> {
  if (
    !Number.isSafeInteger(value.start) ||
    !Number.isSafeInteger(value.end) ||
    value.start < 0 ||
    value.end < value.start
  ) {
    return yield* new AuthoredPresentationError({
      reason: { _tag: 'InvalidSpan', span: value },
    })
  }
  return Object.freeze({ start: value.start, end: value.end })
})

/** Copy display data independently; a new source revision never mutates previous artifacts. */
export const make = Effect.fn('AuthoredPresentation.make')(function* (
  sourceId: string,
  revision: string,
  entries: readonly Entry[],
  diagnostics: readonly Diagnostic[],
): Effect.fn.Return<
  Presentation,
  AuthoredPresentationError | AuthoredIdentity.AuthoredIdentityError
> {
  const copiedEntries: Entry[] = []
  const copiedDiagnostics: Diagnostic[] = []
  for (const entry of entries) {
    copiedEntries.push(
      Object.freeze({
        anchor: yield* AuthoredIdentity.anchor(entry.anchor.owner, entry.anchor.path),
        span: yield* span(entry.span),
        ...(entry.spelling === undefined ? {} : { spelling: entry.spelling }),
        ...(entry.trivia === undefined ? {} : { trivia: entry.trivia }),
        ...(entry.documentation === undefined ? {} : { documentation: entry.documentation }),
      }),
    )
  }
  for (const diagnostic of diagnostics) {
    copiedDiagnostics.push(
      Object.freeze({
        anchor: yield* AuthoredIdentity.anchor(diagnostic.anchor.owner, diagnostic.anchor.path),
        span: yield* span(diagnostic.span),
        code: diagnostic.code,
        message: diagnostic.message,
      }),
    )
  }
  return Object.freeze({
    _tag: 'AuthoredPresentation',
    sourceId,
    revision,
    entries: Object.freeze(copiedEntries),
    diagnostics: Object.freeze(copiedDiagnostics),
  })
})
