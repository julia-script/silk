import * as Backend from './Backend.js'
import * as Target from './Target.js'
import * as WasmBackend from './WasmBackend.js'

/**
 * Which backend serves which target.
 *
 * Backends already declare the targets they support, so the choice is derivable rather than
 * something a caller should have to make: asking for a target *is* asking for its backend, and a
 * caller free to pair them by hand is a caller free to pair them wrongly — a native target sent
 * to the WebAssembly backend is a rejection that never needed to be possible.
 *
 * This is its own module because a backend implementation imports the service contract in
 * `Backend`, so listing the implementations there would close an import cycle. Both the driver
 * and the analysis facade dispatch through here.
 */

/** Every backend the bootstrap compiler can dispatch to, in deterministic order. */
export const backends: ReadonlyArray<Backend.Backend> = Object.freeze([
  Backend.LlvmBackend,
  WasmBackend.WasmBackend,
])

/**
 * The backend that serves a target, or `undefined` for a target no backend claims.
 *
 * A missing backend is a real state rather than an impossible one: `Target.Id` is a closed set,
 * but nothing forces every member of it to have an implementation yet.
 */
export const forTarget = (target: Target.Target): Backend.Backend | undefined =>
  backends.find((candidate) => candidate.targets.includes(target.id))
