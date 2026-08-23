import * as Diagnostic from './Diagnostic.js'
import type * as SourceSpan from './SourceSpan.js'
import * as SuspensionMode from './SuspensionMode.js'

/** Static propagation facts published by one explicit Execution construction delimiter. */
export interface Delimiter {
  readonly _tag: 'ExecutionDelimiter'
  readonly body: SuspensionMode.Summary
  readonly owner: SuspensionMode.Summary
}

/** Entry admission remains separate from Effect requirement-row admission. */
export type EntryAdmission =
  | { readonly _tag: 'Accepted' }
  | { readonly _tag: 'MissingExplicitExecutionOwner'; readonly summary: SuspensionMode.Summary }

/** Retains the erased body's modes while stopping propagation into the owner-side drive caller. */
export const delimit = (body: SuspensionMode.Summary): Delimiter =>
  Object.freeze({
    _tag: 'ExecutionDelimiter',
    body,
    owner: SuspensionMode.direct,
  })

/** Checks only external-park ownership; requirement-row checks remain independent. */
export const admitEntry = (
  summary: SuspensionMode.Summary,
  explicitDelimiter: boolean,
): EntryAdmission =>
  SuspensionMode.has(summary, 'ExternalPark') && !explicitDelimiter
    ? Object.freeze({ _tag: 'MissingExplicitExecutionOwner', summary })
    : Object.freeze({ _tag: 'Accepted' })

/** Publishes the stable source diagnostic once a complete entry violates admission. */
export const entryDiagnostic = (
  summary: SuspensionMode.Summary,
  explicitDelimiter: boolean,
  span: SourceSpan.SourceSpan,
): Diagnostic.Diagnostic | undefined =>
  admitEntry(summary, explicitDelimiter)._tag === 'MissingExplicitExecutionOwner'
    ? Diagnostic.missingExplicitExecutionOwner(SuspensionMode.encode(summary), span)
    : undefined

export const encode = (self: Delimiter): string =>
  `ExecutionDelimiter<body=${SuspensionMode.encode(self.body)};owner=${SuspensionMode.encode(self.owner)}>`
