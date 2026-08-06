import type * as Diagnostic from '@silk-effect/compiler/Diagnostic'
import type * as Driver from '@silk-effect/compiler/Driver'
import * as SourceFile from '@silk-effect/compiler/SourceFile'
import type * as SourceResolver from '@silk-effect/compiler/SourceResolver'
import type * as Path from 'effect/Path'
import * as FileSourceResolver from './FileSourceResolver.js'

/**
 * Terminal rendering of driver outcomes: diagnostics, per-phase timings, and failure provenance.
 * Pure string building over the driver's closed outcome data — nothing here performs effects, so
 * the exact text a run prints is directly assertable in tests.
 */

/** Where a byte offset falls in a source, in the 1-based terms editors and humans use. */
export interface Position {
  readonly line: number
  readonly column: number
}

/** Physical display path and exact bytes for one loaded canonical module. */
export interface DisplaySource {
  readonly path: string
  readonly bytes: Uint8Array
}

/** Loaded sources keyed by the diagnostic source identities they own. */
export type SourceCatalog = ReadonlyMap<string, DisplaySource>

/** Builds display locations for the canonical modules loaded through one filesystem resolver. */
export const catalog = (
  resolver: FileSourceResolver.FileSourceResolver,
  sources: ReadonlyMap<string, SourceFile.SourceFile>,
  path: Path.Path,
): SourceCatalog =>
  new Map(
    [...sources].map(([module, source]) => [
      module,
      {
        path: FileSourceResolver.sourcePath(resolver, module, path),
        bytes: SourceFile.toUint8Array(source),
      },
    ]),
  )

const lineFeed = 0x0a

/**
 * Converts a byte offset into a 1-based line and column. Diagnostic spans are byte offsets into
 * the source, but a byte offset is not actionable in a terminal; a file:line:column prefix is.
 * Columns count bytes, matching the compiler's byte-oriented spans.
 */
export const positionAt = (bytes: Uint8Array, offset: number): Position => {
  const bounded = Math.max(0, Math.min(offset, bytes.length))
  let line = 1
  let lineStart = 0
  for (let index = 0; index < bounded; index += 1) {
    if (bytes[index] === lineFeed) {
      line += 1
      lineStart = index + 1
    }
  }
  return { line, column: bounded - lineStart + 1 }
}

/** Renders one diagnostic as a `path:line:column: severity[CODE] message` line with its notes. */
export const diagnostic = (self: Diagnostic.Diagnostic, sources: SourceCatalog): string => {
  const source = sources.get(self.span.sourceId)
  if (source === undefined) {
    return `${self.span.sourceId}:?:?: ${self.severity}[${self.code}] ${self.message}`
  }
  const at = positionAt(source.bytes, self.span.start)
  const head = `${source.path}:${at.line}:${at.column}: ${self.severity}[${self.code}] ${self.message}`
  const notes = (self.notes ?? []).map((note) => `  note: ${note}`)
  return [head, ...notes].join('\n')
}

/** Renders every diagnostic of one run, in the order the driver published them. */
export const diagnostics = (
  self: ReadonlyArray<Diagnostic.Diagnostic>,
  sources: SourceCatalog,
): string => self.map((entry) => diagnostic(entry, sources)).join('\n')

const milliseconds = (value: number): string => `${value.toFixed(1)}ms`

const megabytes = (bytes: number): string => `${(bytes / 1024 / 1024).toFixed(1)}MB`

/** Renders the per-phase timing table the driver records for every run. */
export const phases = (self: ReadonlyArray<Driver.PhaseReport>): string => {
  const rows = self.map((entry) => ({
    phase: entry.phase,
    elapsed: milliseconds(entry.elapsedMs),
    counts: `${entry.inputs} in / ${entry.outputs} out`,
    diagnostics: `${entry.diagnostics} diag`,
    heap: megabytes(entry.heapBytes),
  }))
  const width = rows.reduce((widest, row) => Math.max(widest, row.phase.length), 0)
  return rows
    .map(
      (row) =>
        `  ${row.phase.padEnd(width)}  ${row.elapsed.padStart(8)}  ${row.counts}  ${row.diagnostics}  heap ${row.heap}`,
    )
    .join('\n')
}

/**
 * Renders a native toolchain failure with the exact command that failed. The driver keeps full
 * command provenance precisely so a build failure is reproducible by hand; dropping it here would
 * waste the only information that makes a linker error debuggable.
 */
const toolchainFailure = (stage: string, failure: Driver.Failed['failure']): string => {
  const command = [failure.planned.command, ...failure.planned.arguments].join(' ')
  const reason =
    failure.reason._tag === 'MissingInput'
      ? `missing input ${failure.reason.path}`
      : failure.reason._tag === 'TargetMismatch'
        ? `target mismatch: expected ${failure.reason.expected}, got ${failure.reason.actual}`
        : `process exited with status ${failure.status ?? 'unknown'}`
  return [
    `${stage} stage failed: ${reason}`,
    `  command: ${command}`,
    ...(failure.output.trim().length > 0
      ? failure.output
          .trimEnd()
          .split('\n')
          .map((line) => `  | ${line}`)
      : []),
  ].join('\n')
}

/** Explains why the root module offered no usable `main`, in terms a caller can act on. */
const entryReason = (reason: Extract<Driver.NoEntry, { _tag: 'NoEntry' }>['reason']): string => {
  switch (reason) {
    case 'MissingEntry':
      return 'the root module declares no `main`'
    case 'AmbiguousEntry':
      return 'the root module declares more than one `main`'
    case 'ParameterizedEntry':
      return '`main` must take no parameters'
    case 'UntypedEntry':
      return '`main` must declare a resolved return type'
  }
}

/** The human-readable summary of one outcome, excluding the phase table. */
export const outcome = (
  self: Driver.Outcome,
  sources: SourceCatalog,
  entryPath: string,
): string => {
  switch (self._tag) {
    case 'Compiled': {
      const rendered = diagnostics(self.diagnostics, sources)
      return [
        ...(rendered.length > 0 ? [rendered] : []),
        `Compiled ${entryPath} -> ${self.executable} (${self.target.id}, ${self.symbols.length} symbols)`,
      ].join('\n')
    }
    case 'NoEntry': {
      const rendered = diagnostics(self.diagnostics, sources)
      return [
        ...(rendered.length > 0 ? [rendered] : []),
        `No entry point: ${entryReason(self.reason)}`,
      ].join('\n')
    }
    case 'TargetFailed': {
      const rendered = diagnostics(self.diagnostics, sources)
      return [
        ...(rendered.length > 0 ? [rendered] : []),
        `Target error: ${self.error.message}`,
      ].join('\n')
    }
    case 'BackendFailed': {
      const rendered = diagnostics(self.diagnostics, sources)
      return [
        ...(rendered.length > 0 ? [rendered] : []),
        `Backend error: ${self.error.message}`,
      ].join('\n')
    }
    case 'Failed':
      return toolchainFailure(self.stage, self.failure)
    case 'Rejected':
      return diagnostics(self.diagnostics, sources)
  }
}

/** Renders a typed source-resolution failure with every safe frontend diagnostic. */
export const sourceResolutionFailed = (
  self: Driver.SourceResolutionFailed,
  sources: SourceCatalog,
): string => {
  const renderedDiagnostics = diagnostics(self.diagnostics, sources)
  const failures = resolutionFailures(self.failures)
  return [...(renderedDiagnostics.length > 0 ? [renderedDiagnostics] : []), ...failures].join('\n')
}

/** Renders operational source resolver failures independently of any strict driver outcome. */
export const resolutionFailures = (
  self: ReadonlyArray<SourceResolver.SourceResolverError>,
): ReadonlyArray<string> =>
  self.map((failure) => `Source resolution error [${failure.module}]: ${failure.message}`)

/** Whether an outcome represents a successful compilation, deciding the process exit status. */
export const succeeded = (self: Driver.Outcome): self is Driver.Compiled => self._tag === 'Compiled'
