import * as Effect from 'effect/Effect'
import * as Sink from 'effect/Sink'
import * as Stream from 'effect/Stream'
import * as WebContainer from '../../src/WebContainer.js'

const unavailable = (operation: string) =>
  Effect.die(new Error(`${operation} is unavailable in this test`))

export const make = (overrides: Partial<WebContainer.Service> = {}): WebContainer.Service =>
  WebContainer.make({
    path: '/usr/local/bin:/usr/bin',
    workdir: '/home/projects/test',
    fileSystem: {
      makeDirectory: () => unavailable('makeDirectory'),
      readDirectory: () => unavailable('readDirectory'),
      readFile: () => unavailable('readFile'),
      readFileString: () => unavailable('readFileString'),
      writeFile: () => unavailable('writeFile'),
      writeFileString: () => unavailable('writeFileString'),
      remove: () => unavailable('remove'),
      rename: () => unavailable('rename'),
      watch: () => Stream.unwrap(unavailable('watch')),
    },
    mount: () => unavailable('mount'),
    exportJson: () => unavailable('exportJson'),
    exportBinary: () => unavailable('exportBinary'),
    exportZip: () => unavailable('exportZip'),
    setPreviewScript: () => unavailable('setPreviewScript'),
    spawn: () => unavailable('spawn'),
    portEvents: Stream.empty,
    serverReadyEvents: Stream.empty,
    internalErrorEvents: Stream.empty,
    previewMessageEvents: Stream.empty,
    ...overrides,
  })

export const drainStringInput: Sink.Sink<void, string> = Sink.drain
