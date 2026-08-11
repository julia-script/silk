/** The two process-output destinations exposed by Silk's smallest observable host service. */
export type Destination = 'Stdout' | 'Stderr'

/** One complete immutable write retained by deterministic and in-memory providers. */
export interface WriteEvent {
  readonly _tag: 'HostWrite'
  readonly destination: Destination
  readonly bytes: ReadonlyArray<number>
}

/** The typed provider result: a write commits completely or fails without a partial event. */
export type WriteResult =
  | { readonly _tag: 'Written' }
  | { readonly _tag: 'WriteFailure'; readonly message: string }

/** The explicit host boundary used by evaluation and WebAssembly hosting. */
export interface Provider {
  readonly writeAll: (destination: Destination, bytes: ReadonlyArray<number>) => WriteResult
}

/** A replaceable provider plus its immutable event snapshot. */
export interface Memory {
  readonly _tag: 'StandardStreamsMemory'
  readonly provider: Provider
  readonly events: () => ReadonlyArray<WriteEvent>
}

/** The private, versioned WebAssembly import contract. */
export const wasmModule = 'silk:runtime/standard-streams@v1'
export const wasmWriteAll = 'write_all'
export const wasmMemoryExport = '__silk_memory_v1'

const written: WriteResult = Object.freeze({ _tag: 'Written' })

/** Builds an in-memory all-or-failure provider; `failAt` is a zero-based attempted-write ordinal. */
export const memory = (options: { readonly failAt?: number } = {}): Memory => {
  const recorded: Array<WriteEvent> = []
  let attempted = 0
  const provider: Provider = Object.freeze({
    writeAll: (destination: Destination, bytes: ReadonlyArray<number>) => {
      const ordinal = attempted
      attempted += 1
      if (ordinal === options.failAt) {
        return Object.freeze({
          _tag: 'WriteFailure',
          message: `standard stream write ${ordinal} failed`,
        })
      }
      recorded.push(
        Object.freeze({
          _tag: 'HostWrite',
          destination,
          bytes: Object.freeze(Array.from(bytes)),
        }),
      )
      return written
    },
  })
  return Object.freeze({
    _tag: 'StandardStreamsMemory',
    provider,
    events: () => Object.freeze(recorded.map((event) => event)),
  })
}

/** Creates the exact import object for a module exporting the private runtime memory. */
export const wasmImports = (
  provider: Provider,
  memoryOf: () => { readonly buffer: ArrayBufferLike } | undefined,
): Readonly<Record<string, Readonly<Record<string, (...arguments_: Array<number>) => number>>>> =>
  Object.freeze({
    [wasmModule]: Object.freeze({
      [wasmWriteAll]: (destination: number, address: number, length: number): number => {
        const memory = memoryOf()
        if (memory === undefined || address < 0 || length < 0) return 1
        const end = address + length
        if (!Number.isSafeInteger(end) || end > memory.buffer.byteLength) return 1
        const bytes = Object.freeze(Array.from(new Uint8Array(memory.buffer, address, length)))
        try {
          return provider.writeAll(destination === 0 ? 'Stdout' : 'Stderr', bytes)._tag ===
            'Written'
            ? 0
            : 1
        } catch {
          return 1
        }
      },
    }),
  })
