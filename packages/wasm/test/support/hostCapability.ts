/**
 * Host WebAssembly features a test may need but cannot assume.
 *
 * Some assertions in this suite ask the host's own `WebAssembly.validate` to accept a module the
 * builder emitted. That is the right oracle when the host implements the proposal in question, and
 * the wrong one when it does not: the module is still valid, the host is simply older. Issue #161
 * measured that gap for one construct, and the repository's declared engine range spans both sides
 * of it, so the suite has to detect the difference rather than assume it away.
 *
 * A capability is probed with a module written out by hand rather than one produced by the builder.
 * If the probe went through the encoder under test, an encoder regression would look exactly like a
 * missing host feature, and the assertion it guards would turn itself off in the one situation it
 * exists to catch.
 */

/**
 * The package builds against the ES library, which does not describe `process`. Declaring the one
 * field used here keeps that build honest without pulling in Node's types, and `typeof` reaches for
 * it without assuming a runtime that has it.
 *
 * @internal
 */
declare const process: { readonly env: { readonly CI?: string | undefined } } | undefined

/** @internal */
const inContinuousIntegration = (): boolean =>
  typeof process !== 'undefined' && process.env.CI !== undefined

export interface Capability {
  /** Whether the running host accepts the construct. */
  readonly supported: boolean
  /** What a guarded assertion must report when it does not run. */
  readonly reason: string
}

/**
 * `memory.copy` between an i64-address memory and an i32-address memory.
 *
 * The memory64 proposal takes the destination index at the destination memory's address type, the
 * source index at the source memory's, and the count at the narrower of the two. V8 13.6 (Node 24)
 * implements that rule. V8 12.4 (Node 22) requires both memories to share an address type and
 * rejects the mixed form in either direction, while accepting i64 memories, `i64.load` at an offset
 * above 2^32, and `memory.size` on an i64 memory — so the gap is specific to this instruction, not
 * to memory64 as a whole.
 *
 * The probe below is that instruction and nothing else: two memories, one i64 and one i32, and a
 * function that copies between them.
 */
const mixedMemoryCopyProbe = Uint8Array.from([
  // magic and version
  0x00, 0x61, 0x73, 0x6d, 0x01, 0x00, 0x00, 0x00,
  // type section: one type `() -> ()`
  0x01, 0x04, 0x01, 0x60, 0x00, 0x00,
  // function section: one function of that type
  0x03, 0x02, 0x01, 0x00,
  // memory section: memory 0 is i64-addressed (limits flag 0x04), memory 1 is i32-addressed
  0x05, 0x05, 0x02, 0x04, 0x01, 0x00, 0x01,
  // code section: one body of twelve bytes, no locals
  0x0a, 0x0e, 0x01, 0x0c, 0x00,
  // destination index at memory 0's address type, source index at memory 1's, count at the narrower
  0x42, 0x00, 0x41, 0x00, 0x41, 0x00,
  // memory.copy, destination memory 0, source memory 1, then end
  0xfc, 0x0a, 0x00, 0x01, 0x0b,
])

/**
 * Whether the running host validates a mixed 64/32-address `memory.copy`, and what to report when
 * it does not.
 */
export const mixedMemoryCopy: Capability = (() => {
  const supported = WebAssembly.validate(mixedMemoryCopyProbe)
  return {
    supported,
    reason: supported
      ? 'host validates a mixed 64/32-address memory.copy'
      : "this host's WebAssembly rejects a mixed 64/32-address memory.copy, so it cannot serve as the validity oracle for a module containing one; the encoding is unchecked here. Node 24 accepts it (see issue #161)",
  }
})()

export interface Requirement {
  /** Whether the guarded test should be skipped rather than run. */
  readonly skip: boolean
  /** The test's name, carrying the reason when the capability is absent. */
  readonly name: string
  /** Present only when the test must run and fail, carrying what to fail with. */
  readonly failure: string | undefined
}

/**
 * Describes how a test that can only run on a capable host should be registered.
 *
 * The reason goes into the test's own name rather than into a `console.log`, because the default
 * reporter prints neither log output nor annotations: a skip explained only by a log is a skip
 * nobody reads, which is how #151's `opt` cross-check went its whole life without running. A named
 * skip still counts in the run summary, so the run reports that something did not happen, and every
 * reporter that lists a test at all shows why.
 *
 * A capability missing under CI is a failure rather than a skip. CI pins a runtime that has it, so
 * its absence there means the pinned runtime changed and the assertion has quietly stopped running
 * — the outcome this guard exists to prevent, not one for it to produce.
 */
export const requiring = (capability: Capability, name: string): Requirement => {
  if (capability.supported) return { skip: false, name, failure: undefined }
  const explanation = `${name}: ${capability.reason}`
  return inContinuousIntegration()
    ? {
        skip: false,
        name,
        failure: `${explanation}. CI pins a runtime that supports it, so this is a change in the pinned runtime rather than an environment to skip.`,
      }
    : { skip: true, name: `${name} — skipped: ${capability.reason}`, failure: undefined }
}
