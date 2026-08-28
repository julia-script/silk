import type { BlockedReason, TraceEvent } from './BootstrapTrace.js'
import type {
  AggregateValue,
  IntegerValue,
  SliceValue,
  UnionValue,
  Value,
} from './BootstrapValue.js'
import type * as ChildProcess from './ChildProcess.js'
import type * as HostInput from './HostInput.js'
import type * as Mir from './Mir.js'
import type * as MonotonicClock from './MonotonicClock.js'
import * as OsFileSystemHost from './OsFileSystemHost.js'
import type * as Scalar from './Scalar.js'
import type * as StandardInput from './StandardInput.js'
import type * as StandardStreams from './StandardStreams.js'
import * as SystemClock from './SystemClock.js'
import * as Type from './Type.js'

/** Preserves an arbitrary stream-provider throw as observable evaluator data. */
export const writeFailure = (cause: unknown): StandardStreams.WriteResult =>
  Object.freeze({
    _tag: 'WriteFailure',
    message: 'standard stream provider threw',
    cause,
  })

/** Preserves an arbitrary filesystem-provider throw in the typed host failure channel. */
export const osFailure = (cause: unknown): OsFileSystemHost.Failure =>
  Object.freeze({ _tag: 'Failure', reason: 'Other', cause })

/** Executes one standard-stream boundary call and preserves an arbitrary thrown cause. */
export const writeAll = (
  provider: StandardStreams.Provider,
  destination: StandardStreams.Destination,
  bytes: ReadonlyArray<number>,
): StandardStreams.WriteResult => {
  try {
    return provider.writeAll(destination, bytes)
  } catch (cause) {
    return writeFailure(cause)
  }
}

/** Executes one filesystem host operation and translates a JavaScript throw exactly once. */
export const invoke = <A>(run: () => A): A | OsFileSystemHost.Failure => {
  try {
    return run()
  } catch (cause) {
    return osFailure(cause)
  }
}

interface LocalState {
  readonly value: Value
  readonly fromCall: boolean
}

export type BoundaryStep =
  | { readonly _tag: 'Value'; readonly value: Value }
  | { readonly _tag: 'Blocked'; readonly reason: BlockedReason }

export interface State {
  readonly standardStreams?: StandardStreams.Provider
  readonly standardInput?: StandardInput.Provider
  readonly childProcess?: ChildProcess.Provider
  readonly processCaptures: Array<ReadonlyArray<number>>
  readonly hostInput?: HostInput.Provider
  readonly osFileSystem?: OsFileSystemHost.Provider
  readonly systemClock?: SystemClock.Provider
  readonly monotonicClock?: MonotonicClock.Provider
}

export interface ExecutionContext {
  readonly state: State
  readonly fn: Mir.MirFunction
  readonly trace: Array<TraceEvent>
  readonly read: (local: Mir.LocalId) => LocalState
  readonly write: (local: Mir.LocalId, state: LocalState) => void
  readonly cell: (slice: SliceValue) => LocalState
  readonly readInteger: (local: Mir.LocalId, expected?: Scalar.IntegerSpelling) => IntegerValue
  readonly replaceReferenced: (local: Mir.LocalId, replacement: Value) => void
  readonly byteView: (local: Mir.LocalId) => ReadonlyArray<number>
  readonly writeByteView: (local: Mir.LocalId, bytes: ReadonlyArray<number>) => void
  readonly optionValue: (element: Type.Type, payload?: Value) => UnionValue
  readonly handleValue: (handle: OsFileSystemHost.Handle) => AggregateValue
  readonly hostHandle: (local: Mir.LocalId) => OsFileSystemHost.Handle
}

const integerValue = (type: Scalar.IntegerSpelling, input: bigint | number): IntegerValue =>
  Object.freeze({ _tag: 'IntegerValue', type, value: BigInt(input) })

const blockedStep = (reason: BlockedReason): BoundaryStep =>
  Object.freeze({ _tag: 'Blocked', reason: Object.freeze(reason) })

/** Executes the host/OS boundary operations owned by the bootstrap OS actor. */
export const execute = (
  context: ExecutionContext,
  operation: Extract<Mir.Operation, { readonly _tag: 'HostWrite' | 'OsCall' }>,
): BoundaryStep | undefined => {
  const {
    state,
    fn,
    trace,
    read,
    write,
    cell,
    readInteger,
    replaceReferenced,
    byteView,
    writeByteView,
    optionValue,
    handleValue,
    hostHandle,
  } = context
  switch (operation._tag) {
    case 'HostWrite': {
      const stream = readInteger(operation.stream, 'i32')
      const viewed = read(operation.bytes).value
      const bytes = (() => {
        if (viewed._tag === 'StaticViewValue') return viewed.bytes
        if (viewed._tag !== 'SliceValue') return undefined
        const root = cell(viewed).value
        if (root._tag !== 'ArrayValue') return undefined
        const selected = root.elements.slice(viewed.base, viewed.base + viewed.length)
        if (selected.some((element) => element._tag !== 'IntegerValue' || element.type !== 'u8'))
          return undefined
        return Object.freeze(
          selected.flatMap((element) =>
            element._tag === 'IntegerValue' ? [Number(element.value)] : [],
          ),
        )
      })()
      if (bytes === undefined) {
        throw new RangeError('MIR verifier allowed a non-byte slice standard-stream write')
      }
      const destination: StandardStreams.Destination = stream.value === 0n ? 'Stdout' : 'Stderr'
      const result =
        state.standardStreams === undefined
          ? undefined
          : writeAll(state.standardStreams, destination, bytes)
      if (result === undefined) return blockedStep({ _tag: 'MissingStandardStreams' })
      trace.push(
        Object.freeze({
          _tag: 'HostWrite',
          function: fn.id,
          destination,
          bytes: Object.freeze(Array.from(bytes)),
          outcome: result._tag,
          ...(result._tag === 'WriteFailure' && result.cause !== undefined
            ? { cause: result.cause }
            : {}),
          span: operation.provenance.span,
        }),
      )
      if (result._tag === 'WriteFailure') {
        return {
          _tag: 'Value',
          value: Object.freeze({
            _tag: 'EffectOutcomeValue',
            type: operation.propagationType.type,
            tag: operation.failureTag,
            payload: Object.freeze({
              _tag: 'AggregateValue',
              type: operation.failure,
              fields: Object.freeze([]),
            }),
          }),
        }
      }
      write(operation.destination, {
        value: Object.freeze({
          _tag: 'AggregateValue',
          type: Type.unit,
          fields: Object.freeze([]),
        }),
        fromCall: false,
      })
      break
    }
    case 'OsCall': {
      const arguments_ = operation.arguments
      const commit = (result: Value): void =>
        write(operation.destination, { value: result, fromCall: false })
      const name = operation.operation.name
      const clockResult = (
        completed: boolean,
        cause?: unknown,
      ): Extract<BoundaryStep, { readonly _tag: 'Value' }> => {
        trace.push(
          Object.freeze({
            _tag: 'OsCall',
            function: fn.id,
            operation: operation.operation,
            outcome: completed ? 'Completed' : 'Failure',
            ...(cause === undefined ? {} : { cause }),
            span: operation.provenance.span,
          }),
        )
        return Object.freeze({ _tag: 'Value', value: integerValue('i32', completed ? 1 : 0) })
      }
      const invokeClock = <A>(run: () => A): { readonly result?: A; readonly cause?: unknown } => {
        try {
          return Object.freeze({ result: run() })
        } catch (cause) {
          return Object.freeze({ cause })
        }
      }
      if (name === 'osSystemClockNow' || name === 'osMonotonicClockNow') {
        const host = name === 'osSystemClockNow' ? state.systemClock : state.monotonicClock
        if (host === undefined)
          return blockedStep({
            _tag: name === 'osSystemClockNow' ? 'MissingSystemClock' : 'MissingMonotonicClock',
          })
        const seconds = arguments_.at(0)
        const nanoseconds = arguments_.at(1)
        if (seconds === undefined || nanoseconds === undefined)
          throw new RangeError('OS clock read omitted scalar outputs')
        const invoked = invokeClock(host.now)
        const result = invoked.result
        const completed =
          result !== undefined && result._tag === 'Read' && SystemClock.isInstant(result.instant)
        if (completed && result?._tag === 'Read') {
          replaceReferenced(seconds, integerValue('i64', result.instant.seconds))
          replaceReferenced(nanoseconds, integerValue('i64', result.instant.nanoseconds))
        }
        const boundary = clockResult(completed, invoked.cause)
        commit(boundary.value)
        break
      }
      if (name === 'osSystemClockResolution' || name === 'osMonotonicClockResolution') {
        const host = name === 'osSystemClockResolution' ? state.systemClock : state.monotonicClock
        if (host === undefined)
          return blockedStep({
            _tag:
              name === 'osSystemClockResolution' ? 'MissingSystemClock' : 'MissingMonotonicClock',
          })
        const output = arguments_.at(0)
        if (output === undefined) throw new RangeError('OS clock resolution omitted its output')
        const invoked = invokeClock(host.resolution)
        const result = invoked.result
        const completed =
          result !== undefined &&
          result._tag === 'Resolution' &&
          SystemClock.isResolution(result.nanoseconds)
        if (completed && result?._tag === 'Resolution') {
          replaceReferenced(output, integerValue('u64', result.nanoseconds))
        }
        const boundary = clockResult(completed, invoked.cause)
        commit(boundary.value)
        break
      }
      if (name === 'osMonotonicClockWaitUntil') {
        const host = state.monotonicClock
        if (host === undefined) return blockedStep({ _tag: 'MissingMonotonicClock' })
        const seconds = arguments_.at(0)
        const nanoseconds = arguments_.at(1)
        if (seconds === undefined || nanoseconds === undefined)
          throw new RangeError('OS monotonic wait omitted its deadline')
        const deadline = Object.freeze({
          seconds: readInteger(seconds, 'i64').value,
          nanoseconds: readInteger(nanoseconds, 'i64').value,
        })
        if (!SystemClock.isInstant(deadline)) {
          const boundary = clockResult(false)
          commit(boundary.value)
          break
        }
        const invoked = invokeClock(() => host.waitUntil(deadline))
        const completed = invoked.result?._tag === 'Waited'
        const boundary = clockResult(completed, invoked.cause)
        commit(boundary.value)
        break
      }
      const reasonOutput = arguments_.at(-2)
      const codeOutput = arguments_.at(-1)
      if (reasonOutput === undefined || codeOutput === undefined)
        throw new RangeError('OS intrinsic omitted status outputs')
      const status = (failure?: OsFileSystemHost.Failure): void => {
        replaceReferenced(
          reasonOutput,
          integerValue(
            'i32',
            failure === undefined ? 0 : OsFileSystemHost.reasonCode(failure.reason),
          ),
        )
        replaceReferenced(codeOutput, integerValue('u32', BigInt(failure?.nativeCode ?? 0)))
        trace.push(
          Object.freeze({
            _tag: 'OsCall',
            function: fn.id,
            operation: operation.operation,
            outcome: failure === undefined ? 'Completed' : 'Failure',
            ...(failure === undefined ? {} : { reason: failure.reason }),
            ...(failure?.nativeCode === undefined ? {} : { nativeCode: failure.nativeCode }),
            ...(failure?.cause === undefined ? {} : { cause: failure.cause }),
            span: operation.provenance.span,
          }),
        )
      }
      if (name === 'osStandardInputRead') {
        const input = state.standardInput
        if (input === undefined) return blockedStep({ _tag: 'MissingStandardInput' })
        const output = arguments_.at(0)
        if (output === undefined) throw new RangeError('OS read omitted its output buffer')
        const capacity = byteView(output).length
        const result = input.read(capacity)
        if (result._tag === 'ReadFailure') {
          status({ _tag: 'Failure', reason: 'Other' })
          commit(optionValue('usize'))
          break
        }
        if (result.bytes.length > capacity)
          throw new RangeError('standard-input provider overran the caller buffer')
        writeByteView(output, result.bytes)
        status()
        commit(optionValue('usize', integerValue('usize', BigInt(result.bytes.length))))
        break
      }
      if (name === 'osProcessExecute') {
        const child = state.childProcess
        if (child === undefined) return blockedStep({ _tag: 'MissingChildProcess' })
        const program = arguments_.at(0)
        const argumentBlock = arguments_.at(1)
        const environmentBlock = arguments_.at(2)
        const workingDirectory = arguments_.at(3)
        const processStatus = arguments_.at(4)
        const processCode = arguments_.at(5)
        const outputLength = arguments_.at(6)
        const errorLength = arguments_.at(7)
        if (
          program === undefined ||
          argumentBlock === undefined ||
          environmentBlock === undefined ||
          workingDirectory === undefined ||
          processStatus === undefined ||
          processCode === undefined ||
          outputLength === undefined ||
          errorLength === undefined
        )
          throw new RangeError('OS execute omitted arguments')
        const directory = byteView(workingDirectory)
        const entries = (
          block: ReadonlyArray<number>,
        ): ReadonlyArray<ReadonlyArray<number>> | null => {
          if (block.length === 0) return Object.freeze([])
          if (block.at(-1) !== 0) return null
          const collected: Array<ReadonlyArray<number>> = []
          let start = 0
          for (const [index, byte] of block.entries()) {
            if (byte !== 0) continue
            collected.push(Object.freeze(block.slice(start, index)))
            start = index + 1
          }
          return Object.freeze(collected)
        }
        const requestArguments = entries(byteView(argumentBlock))
        const requestEnvironment = entries(byteView(environmentBlock))
        const programBytes = byteView(program)
        // The block protocol is the intrinsic's precondition, so a malformed request is a
        // typed start failure rather than an execution the host never saw.
        if (
          requestArguments === null ||
          requestEnvironment === null ||
          programBytes.length === 0 ||
          programBytes.includes(0) ||
          directory.includes(0)
        ) {
          state.processCaptures[0] = Object.freeze([])
          state.processCaptures[1] = Object.freeze([])
          status({ _tag: 'Failure', reason: 'InvalidPath' })
          commit(integerValue('i32', 0))
          break
        }
        const result = child.execute(
          Object.freeze({
            program: programBytes,
            arguments: requestArguments,
            environment: requestEnvironment,
            ...(directory.length === 0 ? {} : { workingDirectory: directory }),
          }),
        )
        if (result._tag === 'ExecuteFailure') {
          state.processCaptures[0] = Object.freeze([])
          state.processCaptures[1] = Object.freeze([])
          status({
            _tag: 'Failure',
            reason: result.reason,
            ...(result.nativeCode === undefined ? {} : { nativeCode: result.nativeCode }),
          })
          commit(integerValue('i32', 0))
          break
        }
        state.processCaptures[0] = result.output
        state.processCaptures[1] = result.errors
        replaceReferenced(processStatus, integerValue('i32', result._tag === 'Exited' ? 0 : 1))
        replaceReferenced(
          processCode,
          integerValue('i32', result._tag === 'Exited' ? result.code : result.signal),
        )
        replaceReferenced(outputLength, integerValue('usize', BigInt(result.output.length)))
        replaceReferenced(errorLength, integerValue('usize', BigInt(result.errors.length)))
        status()
        commit(integerValue('i32', 1))
        break
      }
      if (name === 'osProcessCapture') {
        const stream = arguments_.at(0)
        const offset = arguments_.at(1)
        const output = arguments_.at(2)
        if (stream === undefined || offset === undefined || output === undefined)
          throw new RangeError('OS capture omitted arguments')
        const selector = readInteger(stream, 'i32').value
        const captured = state.processCaptures.at(Number(selector))
        const start = Number(readInteger(offset, 'usize').value)
        if (selector !== 0n && selector !== 1n) {
          status({ _tag: 'Failure', reason: 'WrongType' })
          commit(optionValue('usize'))
          break
        }
        if (captured === undefined || start > captured.length) {
          status({ _tag: 'Failure', reason: 'InvalidPath' })
          commit(optionValue('usize'))
          break
        }
        const transferred = captured.slice(start, start + byteView(output).length)
        writeByteView(output, transferred)
        status()
        commit(optionValue('usize', integerValue('usize', BigInt(transferred.length))))
        break
      }
      if (name.startsWith('osHost')) {
        const input = state.hostInput
        if (input === undefined) return blockedStep({ _tag: 'MissingHostInput' })
        if (name === 'osHostArgumentCount') {
          const count = arguments_.at(0)
          if (count === undefined) throw new RangeError('OS count omitted its output')
          const result = input.argumentCount()
          if (result._tag === 'LookupFailure') {
            status({ _tag: 'Failure', reason: 'Other' })
            commit(integerValue('i32', 0))
            break
          }
          replaceReferenced(count, integerValue('usize', BigInt(result.count)))
          status()
          commit(integerValue('i32', 1))
          break
        }
        const output = arguments_.at(name === 'osHostWorkingDirectory' ? 0 : 1)
        if (output === undefined) throw new RangeError('OS lookup omitted its output buffer')
        const selector = arguments_.at(0)
        if (selector === undefined) throw new RangeError('OS lookup omitted its subject')
        let result: HostInput.Lookup
        if (name === 'osHostArgument') {
          result = input.argument(Number(readInteger(selector, 'usize').value))
        } else if (name === 'osHostVariable') {
          result = input.variable(byteView(selector))
        } else {
          result = input.workingDirectory()
        }
        if (result._tag !== 'Present') {
          // Absence is the not-found reason, which the provider reads as an ordinary answer;
          // any other reason is a host that could not answer at all.
          status({
            _tag: 'Failure',
            reason: result._tag === 'Absent' ? 'NotFound' : 'Other',
          })
          commit(optionValue('usize'))
          break
        }
        // The complete byte length is the result even when only a prefix fit, so the caller
        // can size an exact buffer and ask again.
        const capacity = byteView(output).length
        writeByteView(output, result.bytes.slice(0, capacity))
        status()
        commit(optionValue('usize', integerValue('usize', BigInt(result.bytes.length))))
        break
      }
      const host = state.osFileSystem
      if (host === undefined) return blockedStep({ _tag: 'MissingOsFileSystemHost' })
      try {
        if (name === 'osFileOpen' || name === 'osDirectoryOpen') {
          const root = arguments_.at(0)
          const path = arguments_.at(1)
          if (root === undefined || path === undefined)
            throw new RangeError('OS open omitted paths')
          const result =
            name === 'osFileOpen'
              ? invoke(() =>
                  host.fileOpen(
                    byteView(root),
                    byteView(path),
                    Number(readInteger(arguments_.at(2) ?? root, 'i32').value),
                  ),
                )
              : invoke(() => host.directoryOpen(byteView(root), byteView(path)))
          if (result._tag === 'Failure') {
            status(result)
            commit(optionValue(Type.osHandle))
          } else {
            status()
            commit(optionValue(Type.osHandle, handleValue(result.handle)))
          }
          break
        }
        if (name === 'osFileRead') {
          const handle = arguments_.at(0)
          const output = arguments_.at(1)
          if (handle === undefined || output === undefined)
            throw new RangeError('OS read omitted arguments')
          const capacity = byteView(output).length
          const result = invoke(() => host.fileRead(hostHandle(handle), capacity))
          if (result._tag === 'Failure') {
            status(result)
            commit(optionValue('usize'))
          } else {
            writeByteView(output, result.bytes)
            status()
            commit(optionValue('usize', integerValue('usize', BigInt(result.bytes.length))))
          }
          break
        }
        if (name === 'osFileWrite') {
          const handle = arguments_.at(0)
          const input = arguments_.at(1)
          const offset = arguments_.at(2)
          if (handle === undefined || input === undefined || offset === undefined)
            throw new RangeError('OS write omitted arguments')
          const result = invoke(() =>
            host.fileWrite(
              hostHandle(handle),
              byteView(input).slice(Number(readInteger(offset, 'usize').value)),
            ),
          )
          if (result._tag === 'Failure') {
            status(result)
            commit(optionValue('usize'))
          } else {
            status()
            commit(optionValue('usize', integerValue('usize', BigInt(result.count))))
          }
          break
        }
        if (name === 'osDirectoryNext') {
          const handle = arguments_.at(0)
          const output = arguments_.at(1)
          const kind = arguments_.at(2)
          const required = arguments_.at(3)
          if (
            handle === undefined ||
            output === undefined ||
            kind === undefined ||
            required === undefined
          )
            throw new RangeError('OS directory next omitted arguments')
          const result = invoke(() =>
            host.directoryNext(hostHandle(handle), byteView(output).length),
          )
          if (result._tag === 'Failure' || result._tag === 'BufferTooSmall') {
            const failure: OsFileSystemHost.Failure =
              result._tag === 'Failure' ? result : { _tag: 'Failure', reason: 'BufferTooSmall' }
            status(failure)
            if (result._tag === 'BufferTooSmall')
              replaceReferenced(required, integerValue('usize', BigInt(result.requiredCapacity)))
            commit(optionValue('usize'))
          } else if (result._tag === 'End') {
            status()
            commit(optionValue('usize', integerValue('usize', 0n)))
          } else {
            writeByteView(output, result.name)
            replaceReferenced(kind, integerValue('i32', result.kind === 'File' ? 0 : 1))
            status()
            commit(optionValue('usize', integerValue('usize', BigInt(result.name.length))))
          }
          break
        }
        if (name === 'osDirectoryCreateUnique') {
          const root = arguments_.at(0)
          const parent = arguments_.at(1)
          const prefix = arguments_.at(2)
          const output = arguments_.at(3)
          const required = arguments_.at(4)
          if (
            root === undefined ||
            parent === undefined ||
            prefix === undefined ||
            output === undefined ||
            required === undefined
          )
            throw new RangeError('OS unique directory create omitted arguments')
          const result = invoke(() =>
            host.directoryCreateUnique(
              byteView(root),
              byteView(parent),
              byteView(prefix),
              byteView(output).length,
            ),
          )
          if (result._tag === 'Failure' || result._tag === 'BufferTooSmall') {
            status(
              result._tag === 'Failure' ? result : { _tag: 'Failure', reason: 'BufferTooSmall' },
            )
            if (result._tag === 'BufferTooSmall')
              replaceReferenced(required, integerValue('usize', BigInt(result.requiredCapacity)))
            commit(optionValue('usize'))
          } else {
            writeByteView(output, result.name)
            status()
            commit(optionValue('usize', integerValue('usize', BigInt(result.name.length))))
          }
          break
        }
        if (name === 'osPathInspect') {
          const root = arguments_.at(0)
          const path = arguments_.at(1)
          const kind = arguments_.at(2)
          const length = arguments_.at(3)
          if (
            root === undefined ||
            path === undefined ||
            kind === undefined ||
            length === undefined
          )
            throw new RangeError('OS inspect omitted arguments')
          const result = invoke(() => host.pathInspect(byteView(root), byteView(path)))
          if (result._tag === 'Failure') {
            status(result)
            commit(integerValue('i32', 0))
          } else {
            replaceReferenced(kind, integerValue('i32', result.kind === 'File' ? 0 : 1))
            replaceReferenced(length, integerValue('usize', BigInt(result.byteLength)))
            status()
            commit(integerValue('i32', 1))
          }
          break
        }
        let command:
          | ReturnType<typeof host.directoryCreate>
          | ReturnType<typeof host.fileRemove>
          | ReturnType<typeof host.directoryRemove>
          | ReturnType<typeof host.handleClose>
          | OsFileSystemHost.Failure
          | undefined
        if (
          name === 'osDirectoryCreate' ||
          name === 'osFileRemove' ||
          name === 'osDirectoryRemove'
        ) {
          const root = arguments_.at(0)
          const path = arguments_.at(1)
          if (root === undefined || path === undefined)
            throw new RangeError('OS command omitted paths')
          if (name === 'osDirectoryCreate') {
            command = invoke(() => host.directoryCreate(byteView(root), byteView(path)))
          } else if (name === 'osFileRemove') {
            command = invoke(() => host.fileRemove(byteView(root), byteView(path)))
          } else {
            command = invoke(() => host.directoryRemove(byteView(root), byteView(path)))
          }
        } else if (name === 'osHandleClose') {
          command = invoke(() => host.handleClose(hostHandle(arguments_.at(0) ?? reasonOutput)))
        }
        if (command === undefined) throw new RangeError(`Unknown OS intrinsic ${name}`)
        if (command._tag === 'Failure') {
          status(command)
          commit(integerValue('i32', 0))
        } else {
          status()
          commit(integerValue('i32', 1))
        }
      } catch (cause) {
        const failure = osFailure(cause)
        status(failure)
        commit(
          operation.type._tag === 'Union'
            ? optionValue(
                operation.type.type.members.some((member) =>
                  Type.equals(member, Type.some(Type.osHandle)),
                )
                  ? Type.osHandle
                  : 'usize',
              )
            : integerValue('i32', 0),
        )
      }
      break
    }
  }
}
