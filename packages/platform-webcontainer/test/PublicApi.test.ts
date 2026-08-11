import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Package from '../src/index.js'
import * as WebContainer from '../src/WebContainer.js'
import type * as WebContainerError from '../src/WebContainerError.js'

const mountEffect: Effect.Effect<
  void,
  WebContainerError.WebContainerError,
  WebContainer.WebContainer
> = WebContainer.mount({})

it('exports every public actor namespace and Effect-only mount operation', () => {
  assert.isFunction(Package.WebContainer.mount)
  assert.isFunction(Package.WebContainerError.wrappedFailure)
  assert.isFunction(Package.WebContainerEvent.stream)
  assert.isFunction(Package.WebContainerFileSystem.platformError)
  assert.isFunction(Package.WebContainerProcess.make)
  assert.isTrue(Effect.isEffect(mountEffect))
})
