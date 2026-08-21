import * as Data from 'effect/Data'
import * as Option from 'effect/Option'
import * as Result from 'effect/Result'
import * as Schema from 'effect/Schema'
import * as DocumentVersion from './DocumentVersion.js'
import * as EditorQuery from './EditorQuery.js'
import * as IncidentId from './IncidentId.js'
import * as ProjectGeneration from './ProjectGeneration.js'
import * as RequestId from './RequestId.js'
import * as WorkerEpoch from './WorkerEpoch.js'

export const version = 1 as const

const envelope = {
  protocolVersion: Schema.Literal(version),
  epoch: WorkerEpoch.schema,
}

const source = Schema.Struct({
  uri: Schema.String,
  version: DocumentVersion.schema,
  workspace: Schema.String,
  sourceRoot: Schema.String,
  module: Schema.String,
  bytes: Schema.Uint8Array,
})

const invalidation = Schema.Struct({
  priorityUri: Schema.optionalKey(Schema.String),
  dirtyPaths: Schema.Array(Schema.String),
  rediscover: Schema.Boolean,
})

/** Messages accepted by a project worker. */
export const hostSchema = Schema.Union([
  Schema.Struct({
    ...envelope,
    _tag: Schema.Literal('Initialize'),
    workspace: Schema.String,
    sourceRoot: Schema.String,
  }),
  Schema.Struct({
    ...envelope,
    _tag: Schema.Literal('Analyze'),
    generation: ProjectGeneration.schema,
    sources: Schema.Array(source),
    invalidation,
  }),
  Schema.Struct({
    ...envelope,
    _tag: Schema.Literal('SupersedeAnalysis'),
    generation: ProjectGeneration.schema,
  }),
  Schema.Struct({
    ...envelope,
    _tag: Schema.Literal('Query'),
    generation: ProjectGeneration.schema,
    requestId: RequestId.schema,
    query: Schema.Unknown,
  }),
  Schema.Struct({
    ...envelope,
    _tag: Schema.Literal('CancelQuery'),
    requestId: RequestId.schema,
  }),
  Schema.Struct({ ...envelope, _tag: Schema.Literal('Shutdown') }),
])

type DecodedHostMessage = typeof hostSchema.Type

export type HostMessage =
  | Exclude<DecodedHostMessage, { readonly _tag: 'Query' }>
  | {
      readonly protocolVersion: 1
      readonly epoch: WorkerEpoch.WorkerEpoch
      readonly _tag: 'Query'
      readonly generation: ProjectGeneration.ProjectGeneration
      readonly requestId: RequestId.RequestId
      readonly query: EditorQuery.EditorQuery
    }

/** Messages emitted by a project worker. */
export const workerSchema = Schema.Union([
  Schema.Struct({ ...envelope, _tag: Schema.Literal('Ready') }),
  Schema.Struct({
    ...envelope,
    _tag: Schema.Literal('Progress'),
    generation: ProjectGeneration.schema,
    phase: Schema.String,
  }),
  Schema.Struct({
    ...envelope,
    _tag: Schema.Literal('Superseded'),
    generation: ProjectGeneration.schema,
  }),
  Schema.Struct({
    ...envelope,
    _tag: Schema.Literal('Commit'),
    generation: ProjectGeneration.schema,
    uris: Schema.Array(Schema.String),
  }),
  Schema.Struct({
    ...envelope,
    _tag: Schema.Literal('Failure'),
    generation: ProjectGeneration.schema,
    incident: IncidentId.schema,
    message: Schema.String,
  }),
  Schema.Struct({
    ...envelope,
    _tag: Schema.Literal('Result'),
    generation: ProjectGeneration.schema,
    requestId: RequestId.schema,
    result: Schema.Unknown,
  }),
  Schema.Struct({ ...envelope, _tag: Schema.Literal('Stopped') }),
])

export type WorkerMessage = typeof workerSchema.Type

/** Typed protocol-boundary rejection of an unknown structured-clone payload. */
export class ProtocolFailure extends Data.TaggedError('ProtocolFailure')<{
  readonly direction: 'HostToWorker' | 'WorkerToHost'
  readonly message: string
}> {}

const decode = <A>(
  direction: ProtocolFailure['direction'],
  parser: (input: unknown) => Option.Option<A>,
  input: unknown,
): Result.Result<A, ProtocolFailure> => {
  const parsed = parser(input)
  return Option.isSome(parsed)
    ? Result.succeed(parsed.value)
    : Result.fail(new ProtocolFailure({ direction, message: 'Malformed worker protocol message' }))
}

const decodeHostOption = Schema.decodeUnknownOption(hostSchema)
const decodeWorkerOption = Schema.decodeUnknownOption(workerSchema)

export const decodeHost = (input: unknown): Result.Result<HostMessage, ProtocolFailure> => {
  const decoded = decode('HostToWorker', decodeHostOption, input)
  if (Result.isFailure(decoded)) return Result.fail(decoded.failure)
  const message = decoded.success
  if (message._tag !== 'Query') return Result.succeed(message)
  const query = EditorQuery.decode(message.query)
  return Result.isFailure(query)
    ? Result.fail(
        new ProtocolFailure({ direction: 'HostToWorker', message: query.failure.message }),
      )
    : Result.succeed({ ...message, query: query.success })
}

export const decodeWorker = (input: unknown): Result.Result<WorkerMessage, ProtocolFailure> =>
  decode('WorkerToHost', decodeWorkerOption, input)
