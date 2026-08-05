import { describe, expect, it } from 'vitest'
import * as BackendRegistry from '../src/BackendRegistry.js'
import * as Target from '../src/Target.js'

describe('BackendRegistry.forTarget', () => {
  // The point of deriving the backend is that a caller cannot pair a target with a backend that
  // rejects it. That only holds if the mapping covers every target and never covers one twice.
  it('serves every canonical target', () => {
    for (const target of Target.all) {
      expect(BackendRegistry.forTarget(target), target.id).toBeDefined()
    }
  })

  it('serves each target with exactly one backend', () => {
    for (const target of Target.all) {
      const claiming = BackendRegistry.backends.filter((backend) =>
        backend.targets.includes(target.id),
      )
      expect(
        claiming.map((backend) => backend.name),
        target.id,
      ).toHaveLength(1)
    }
  })

  it('routes native targets to LLVM and WebAssembly to the wasm backend', () => {
    for (const target of Target.all) {
      expect(BackendRegistry.forTarget(target)?.name, target.id).toBe(
        target.kind === 'Native' ? 'LLVM' : 'WebAssembly',
      )
    }
  })

  it('accepts the target it was selected for', () => {
    // A backend that does not list the target it was chosen for would be rejected by
    // `Backend.emit` at the point of use, which is exactly the failure this removes.
    for (const target of Target.all) {
      expect(BackendRegistry.forTarget(target)?.targets, target.id).toContain(target.id)
    }
  })
})
