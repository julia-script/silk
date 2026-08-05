import { memoryUsage } from 'node:process'
import * as Effect from 'effect/Effect'
import * as Backend from './Backend.js'
import * as DeclarationIndex from './DeclarationIndex.js'
import * as Diagnostic from './Diagnostic.js'
import * as Elaboration from './Elaboration.js'
import * as Instances from './Instances.js'
import * as Layout from './Layout.js'
import * as Lower from './Lower.js'
import * as ModuleClosure from './ModuleClosure.js'
import * as NativeToolchain from './NativeToolchain.js'
import * as Ownership from './Ownership.js'
import * as Target from './Target.js'
import type * as ToolchainPlan from './ToolchainPlan.js'

/**
 * The end-to-end compiler driver: one orchestration path from a compilation request to a running
 * native executable. The driver invokes the backend and linker services itself — no external
 * harness performs a stage. Outcomes are closed data naming failing stages with provenance, and
 * every run carries a per-phase report: elapsed time, input and output counts, diagnostic
 * counts, and engine-heap memory totals (the bootstrap approximation of allocator totals).
 */

/** One phase's observability entry. Reports are data, not artifacts — exempt from byte-identity. */
export interface PhaseReport {
  readonly phase: string
  readonly elapsedMs: number
  readonly inputs: number
  readonly outputs: number
  readonly diagnostics: number
  readonly heapBytes: number
}

/** One driver request. */
export interface CompileRequest {
  readonly compilation: ModuleClosure.CompilationRequest
  readonly toolchain: NativeToolchain.Toolchain
  readonly profile: ToolchainPlan.OptimizationProfile
  readonly destination: string
  readonly backend?: Backend.Backend
  readonly scopeName?: string
  readonly saveTemps?: boolean
}

/** A completed compilation with its executable and report. */
export interface Compiled {
  readonly _tag: 'Compiled'
  readonly executable: string
  readonly target: Target.Target
  readonly symbols: ReadonlyArray<Backend.SymbolEntry>
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<PhaseReport>
}

/** The request's root module has no valid entry; the toolchain was never invoked. */
export interface NoEntry {
  readonly _tag: 'NoEntry'
  readonly reason: Extract<Instances.Entry, { readonly _tag: 'Unavailable' }>['reason']
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<PhaseReport>
}

/** A native stage failed; the outcome names it with full command provenance. */
export interface Failed {
  readonly _tag: 'Failed'
  readonly stage: 'object' | 'shim' | 'link'
  readonly failure: NativeToolchain.ToolchainFailure
  readonly report: ReadonlyArray<PhaseReport>
}

/** Target selection or native-kind validation stopped compilation before MIR lowering. */
export interface TargetFailed {
  readonly _tag: 'TargetFailed'
  readonly error: Target.TargetError
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<PhaseReport>
}

/** Shared MIR validation, compatibility, or backend construction stopped native emission. */
export interface BackendFailed {
  readonly _tag: 'BackendFailed'
  readonly error: Backend.BackendError
  readonly diagnostics: ReadonlyArray<Diagnostic.Diagnostic>
  readonly report: ReadonlyArray<PhaseReport>
}

/** The closed outcome of one driver run. */
export type Outcome = Compiled | NoEntry | TargetFailed | BackendFailed | Failed

/** Compiles one request end to end, writing the executable to the durable destination. */
export const compile = Effect.fn('Driver.compile')(function* (
  request: CompileRequest,
): Effect.fn.Return<Outcome> {
  const report: Array<PhaseReport> = []
  const phase = <A>(
    name: string,
    inputs: number,
    run: () => A,
    outputs: (result: A) => number,
    diagnostics: (result: A) => number = () => 0,
  ): A => {
    const startedAt = performance.now()
    const result = run()
    report.push(
      Object.freeze({
        phase: name,
        elapsedMs: performance.now() - startedAt,
        inputs,
        outputs: outputs(result),
        diagnostics: diagnostics(result),
        heapBytes: memoryUsage().heapUsed,
      }),
    )
    return result
  }

  const closure = phase(
    'closure',
    request.compilation.sources.size,
    () => ModuleClosure.load(request.compilation),
    (result) => result.modules.length,
    (result) => result.diagnostics.length,
  )
  const index = phase(
    'declaration-index',
    closure.modules.length,
    () => DeclarationIndex.collect(closure),
    (result) => result.modules.reduce((sum, module) => sum + module.declarations.length, 0),
    (result) => result.diagnostics.length,
  )
  const results = phase(
    'elaboration',
    index.modules.length,
    () =>
      new Map(
        closure.modules.map((module) => [module.name, Elaboration.elaborateModule(module.syntax)]),
      ),
    (result) => [...result.values()].reduce((sum, module) => sum + module.functions.length, 0),
    (result) => [...result.values()].reduce((sum, module) => sum + module.diagnostics.length, 0),
  )
  const ownership = phase(
    'ownership',
    results.size,
    () =>
      new Map(
        [...results.entries()].map(([name, result]) => [name, Ownership.checkModule(result)]),
      ),
    (result) => [...result.values()].reduce((sum, module) => sum + module.functions.length, 0),
    (result) => [...result.values()].reduce((sum, module) => sum + module.diagnostics.length, 0),
  )
  const discovery = phase(
    'instance-discovery',
    results.size,
    () => Instances.discover(request.compilation.rootModule, results),
    (result) => result.instances.length,
  )
  const diagnostics = Diagnostic.merge(
    ...closure.modules.map((module) => module.syntax.lexicalDiagnostics),
    ...closure.modules.map((module) => module.syntax.parserDiagnostics),
    closure.diagnostics,
    ...[...results.values()].map((result) => result.diagnostics),
    ...[...ownership.values()].map((facts) => facts.diagnostics),
  )

  if (discovery.entry._tag !== 'Resolved') {
    return Object.freeze({
      _tag: 'NoEntry',
      reason: discovery.entry.reason,
      diagnostics,
      report: Object.freeze([...report]),
    })
  }

  const targetAndLayout = phase(
    'target-layout',
    discovery.instances.length,
    () => {
      const selection = Target.select(request.compilation.target)
      if (selection._tag === 'Unavailable') return selection
      if (!Target.isNative(selection.target)) {
        return Object.freeze({
          _tag: 'Unavailable' as const,
          error: new Target.TargetError({
            operation: 'Target.requireNative',
            requested: selection.target.id,
            message: `Native compilation does not support target ${selection.target.id}`,
          }),
        })
      }
      return Object.freeze({
        _tag: 'Available' as const,
        target: selection.target,
        layout: Layout.plan(selection.target, discovery),
      })
    },
    (result) => (result._tag === 'Available' ? result.layout.entries.length : 0),
  )
  if (targetAndLayout._tag === 'Unavailable') {
    return Object.freeze({
      _tag: 'TargetFailed',
      error: targetAndLayout.error,
      diagnostics,
      report: Object.freeze([...report]),
    })
  }

  const program = phase(
    'mir-lowering',
    discovery.instances.length,
    () => Lower.lowerProgram(discovery, ownership, targetAndLayout.layout),
    (result) => result.functions.length,
  )
  const backend = request.backend ?? Backend.LlvmBackend
  const backendStartedAt = performance.now()
  const emitted = yield* Backend.emit(backend, program, {
    mode: request.profile === 'release' ? 'release' : 'debug',
    sources: new Map(
      closure.modules.map((module) => [module.name, Uint8Array.from(module.syntax.source.bytes)]),
    ),
  }).pipe(
    Effect.map((artifact) => Object.freeze({ _tag: 'Emitted' as const, artifact })),
    Effect.catchTag('BackendError', (error) =>
      Effect.succeed(Object.freeze({ _tag: 'Rejected' as const, error })),
    ),
  )
  report.push(
    Object.freeze({
      phase: 'backend',
      elapsedMs: performance.now() - backendStartedAt,
      inputs: program.functions.length,
      outputs: emitted._tag === 'Emitted' ? emitted.artifact.symbols.length : 0,
      diagnostics: 0,
      heapBytes: memoryUsage().heapUsed,
    }),
  )
  if (emitted._tag === 'Rejected') {
    return Object.freeze({
      _tag: 'BackendFailed',
      error: emitted.error,
      diagnostics,
      report: Object.freeze([...report]),
    })
  }
  const artifact = emitted.artifact

  return NativeToolchain.withBuildScope(
    request.scopeName ?? 'driver',
    (scope) => {
      const object = phase(
        'object',
        1,
        () =>
          NativeToolchain.emitObject(
            request.toolchain,
            scope,
            artifact,
            targetAndLayout.target,
            request.profile,
          ),
        (result) => (result._tag === 'ObjectArtifact' ? 1 : 0),
      )
      if (object._tag === 'ToolchainFailure') {
        return Object.freeze({
          _tag: 'Failed',
          stage: 'object',
          failure: object,
          report: Object.freeze([...report]),
        })
      }
      const shim = phase(
        'shim',
        1,
        () => NativeToolchain.compileShim(request.toolchain, scope, targetAndLayout.target),
        (result) => (result._tag === 'ObjectArtifact' ? 1 : 0),
      )
      if (shim._tag === 'ToolchainFailure') {
        return Object.freeze({
          _tag: 'Failed',
          stage: 'shim',
          failure: shim,
          report: Object.freeze([...report]),
        })
      }
      const linked = phase(
        'link',
        2,
        () =>
          NativeToolchain.ClangLinker.link(
            request.toolchain,
            targetAndLayout.target,
            [object.artifact, shim.artifact],
            [],
            request.destination,
          ),
        (result) => (result._tag === 'Executable' ? 1 : 0),
      )
      if (linked._tag === 'ToolchainFailure') {
        return Object.freeze({
          _tag: 'Failed',
          stage: 'link',
          failure: linked,
          report: Object.freeze([...report]),
        })
      }
      return Object.freeze({
        _tag: 'Compiled',
        executable: linked.path,
        target: linked.target,
        symbols: artifact.symbols,
        diagnostics,
        report: Object.freeze([...report]),
      })
    },
    { saveTemps: request.saveTemps ?? false },
  )
})
