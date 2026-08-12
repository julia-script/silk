import * as WebContainer from '@silk-effect/platform-webcontainer/WebContainer'
import * as WebContainerFileSystem from '@silk-effect/platform-webcontainer/WebContainerFileSystem'
import * as FileSystem from 'effect/FileSystem'
import * as Layer from 'effect/Layer'
import { Atom } from 'effect/unstable/reactivity'

export const makeLayer = <E, R>(
  runtimeLayer: Layer.Layer<WebContainer.WebContainer, E, R>,
): Layer.Layer<WebContainer.WebContainer | FileSystem.FileSystem, E, R> => {
  const fileSystemLayer = WebContainerFileSystem.layer.pipe(Layer.provide(runtimeLayer))
  return Layer.merge(runtimeLayer, fileSystemLayer)
}

/** The one live WebContainer layer value shared by every editor capability. */
export const webContainerLayer = WebContainer.layer({ workdirName: 'silk-editor' })

/** Provides both WebContainer and Effect's standard FileSystem from the same acquired runtime. */
export const layer = makeLayer(webContainerLayer)

/** The registry-scoped service runtime used by every editor atom. */
export const runtime = Atom.runtime(layer)

/** Creates the same editor environment around a deterministic WebContainer service for tests. */
export const testLayer = (service: WebContainer.Service) =>
  makeLayer(WebContainer.layerTest(service))
