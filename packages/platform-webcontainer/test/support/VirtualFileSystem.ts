import * as Effect from 'effect/Effect'
import type * as WebContainer from '../../src/WebContainer.js'
import * as WebContainerError from '../../src/WebContainerError.js'
import * as WebContainerEvent from '../../src/WebContainerEvent.js'
import * as WebContainerService from './WebContainerService.js'

const failure = (code: string, message: string): Error & { readonly code: string } =>
  Object.assign(new Error(message), { code })

const normalize = (target: string): string => {
  const segments: Array<string> = []
  for (const segment of target.replace('/home/projects/test', '').split('/')) {
    if (segment === '' || segment === '.') {
      continue
    }
    if (segment === '..') {
      segments.pop()
    } else {
      segments.push(segment)
    }
  }
  return segments.join('/')
}

const parent = (target: string): string => {
  const parts = normalize(target).split('/')
  parts.pop()
  return parts.join('/')
}

const boundary = <A>(operation: string, evaluate: () => A) =>
  Effect.try({
    try: evaluate,
    catch: (cause) =>
      WebContainerError.wrappedFailure(
        `VirtualFileSystem.${operation}`,
        `Virtual ${operation} failed`,
        cause,
      ),
  })

interface Watcher {
  readonly path: string
  readonly recursive: boolean
  readonly listener: (notification: WebContainer.WatchNotification) => void
}

export interface VirtualFileSystem {
  readonly service: WebContainer.Service
  readonly files: Map<string, Uint8Array>
  readonly directories: Set<string>
  /** Delivers one raw watch notification to every watcher covering `target`. */
  readonly emit: (target: string, event: 'rename' | 'change') => void
  readonly watcherCount: () => number
  /** How many times file contents were read — proves stat/exists never touch contents. */
  readonly fileReads: () => number
}

export const make = (): VirtualFileSystem => {
  const files = new Map<string, Uint8Array>()
  const directories = new Set<string>([''])
  const watchers = new Set<Watcher>()
  let fileReads = 0

  const emit = (target: string, event: 'rename' | 'change'): void => {
    const normalized = normalize(target)
    for (const watcher of watchers) {
      if (watcher.path === normalized) {
        watcher.listener({ event, filename: normalized.split('/').pop() ?? '' })
        continue
      }
      const prefix = watcher.path === '' ? '' : `${watcher.path}/`
      if (!normalized.startsWith(prefix)) {
        continue
      }
      const relative = normalized.slice(prefix.length)
      if (!watcher.recursive && relative.includes('/')) {
        continue
      }
      watcher.listener({ event, filename: relative })
    }
  }

  const fileSystem: WebContainer.FileSystem = {
    makeDirectory: (target, options) =>
      boundary('makeDirectory', () => {
        const normalized = normalize(target)
        if (files.has(normalized) || directories.has(normalized)) {
          throw failure('EEXIST', `${target} already exists`)
        }
        if (options?.recursive === true) {
          let current = ''
          for (const segment of normalized.split('/')) {
            current = current === '' ? segment : `${current}/${segment}`
            directories.add(current)
          }
          return
        }
        if (!directories.has(parent(normalized))) {
          throw failure('ENOENT', `Parent of ${target} was not found`)
        }
        directories.add(normalized)
      }),
    readDirectory: (target) =>
      boundary('readDirectory', () => {
        const normalized = normalize(target)
        if (!directories.has(normalized)) {
          throw failure(
            files.has(normalized) ? 'ENOTDIR' : 'ENOENT',
            `${target} is not a directory`,
          )
        }
        const prefix = normalized === '' ? '' : `${normalized}/`
        const entries = new Map<string, WebContainer.DirectoryEntry>()
        for (const directory of directories) {
          if (directory === normalized || !directory.startsWith(prefix)) {
            continue
          }
          const relative = directory.slice(prefix.length)
          if (!relative.includes('/')) {
            entries.set(relative, { name: relative, type: 'Directory' })
          }
        }
        for (const filename of files.keys()) {
          if (!filename.startsWith(prefix)) {
            continue
          }
          const relative = filename.slice(prefix.length)
          if (!relative.includes('/')) {
            entries.set(relative, { name: relative, type: 'File' })
          }
        }
        return [...entries.values()].sort((left, right) => left.name.localeCompare(right.name))
      }),
    readFile: (target) =>
      boundary('readFile', () => {
        const normalized = normalize(target)
        const value = files.get(normalized)
        if (value === undefined) {
          throw failure(
            directories.has(normalized) ? 'EISDIR' : 'ENOENT',
            `${target} is not a file`,
          )
        }
        fileReads += 1
        return value.slice()
      }),
    readFileString: (target) =>
      boundary('readFileString', () => {
        const normalized = normalize(target)
        const value = files.get(normalized)
        if (value === undefined) {
          throw failure(
            directories.has(normalized) ? 'EISDIR' : 'ENOENT',
            `${target} is not a file`,
          )
        }
        fileReads += 1
        return new TextDecoder().decode(value)
      }),
    writeFile: (target, data) =>
      boundary('writeFile', () => {
        const normalized = normalize(target)
        if (!directories.has(parent(normalized))) {
          throw failure('ENOENT', `Parent of ${target} was not found`)
        }
        files.set(normalized, data.slice())
      }),
    writeFileString: (target, data) =>
      boundary('writeFileString', () => {
        const normalized = normalize(target)
        if (!directories.has(parent(normalized))) {
          throw failure('ENOENT', `Parent of ${target} was not found`)
        }
        files.set(normalized, new TextEncoder().encode(data))
      }),
    remove: (target, options) =>
      boundary('remove', () => {
        const normalized = normalize(target)
        if (files.delete(normalized)) {
          return
        }
        if (!directories.has(normalized)) {
          if (options?.force === true) {
            return
          }
          throw failure('ENOENT', `${target} was not found`)
        }
        const prefix = `${normalized}/`
        const hasChildren =
          [...directories].some((entry) => entry.startsWith(prefix)) ||
          [...files.keys()].some((entry) => entry.startsWith(prefix))
        if (hasChildren && options?.recursive !== true) {
          throw failure('EBUSY', `${target} is not empty`)
        }
        for (const filename of files.keys()) {
          if (filename.startsWith(prefix)) {
            files.delete(filename)
          }
        }
        for (const directory of directories) {
          if (directory === normalized || directory.startsWith(prefix)) {
            directories.delete(directory)
          }
        }
      }),
    rename: (oldPath, newPath) =>
      boundary('rename', () => {
        const oldNormalized = normalize(oldPath)
        const newNormalized = normalize(newPath)
        const file = files.get(oldNormalized)
        if (file !== undefined) {
          files.delete(oldNormalized)
          files.set(newNormalized, file)
          return
        }
        if (!directories.has(oldNormalized)) {
          throw failure('ENOENT', `${oldPath} was not found`)
        }
        directories.delete(oldNormalized)
        directories.add(newNormalized)
        const prefix = `${oldNormalized}/`
        for (const directory of Array.from(directories)) {
          if (directory.startsWith(prefix)) {
            directories.delete(directory)
            directories.add(`${newNormalized}/${directory.slice(prefix.length)}`)
          }
        }
        for (const [filename, contents] of Array.from(files)) {
          if (filename.startsWith(prefix)) {
            files.delete(filename)
            files.set(`${newNormalized}/${filename.slice(prefix.length)}`, contents)
          }
        }
      }),
    watch: (target, options) =>
      WebContainerEvent.stream((listener) =>
        Effect.sync(() => {
          const watcher: Watcher = {
            path: normalize(target),
            recursive: options?.recursive === true,
            listener,
          }
          watchers.add(watcher)
          return {
            unsubscribe: Effect.sync(() => {
              watchers.delete(watcher)
            }),
          }
        }),
      ),
  }

  return {
    files,
    directories,
    emit,
    watcherCount: () => watchers.size,
    fileReads: () => fileReads,
    service: WebContainerService.make({ fileSystem }),
  }
}
