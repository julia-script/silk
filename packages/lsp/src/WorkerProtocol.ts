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
  configuration: Schema.optionalKey(Schema.Unknown),
})

const invalidation = Schema.Struct({
  priorityUri: Schema.optionalKey(Schema.String),
  dirtyPaths: Schema.Array(Schema.String),
  rediscover: Schema.Boolean,
})

/** Messages accepted by a project worker. */
export const hostSchema = Schema.Union([
  Schema.TaggedStruct('Initialize', {
    ...envelope,
    workspace: Schema.String,
    sourceRoot: Schema.String,
  }),
  Schema.TaggedStruct('Analyze', {
    ...envelope,
    generation: ProjectGeneration.schema,
    sources: Schema.Array(source),
    invalidation,
  }),
  Schema.TaggedStruct('SupersedeAnalysis', { ...envelope, generation: ProjectGeneration.schema }),
  Schema.TaggedStruct('Query', {
    ...envelope,
    generation: ProjectGeneration.schema,
    requestId: RequestId.schema,
    query: Schema.Unknown,
  }),
  Schema.TaggedStruct('CancelQuery', { ...envelope, requestId: RequestId.schema }),
  Schema.TaggedStruct('Shutdown', { ...envelope }),
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
  Schema.TaggedStruct('Ready', { ...envelope }),
  Schema.TaggedStruct('Progress', {
    ...envelope,
    generation: ProjectGeneration.schema,
    phase: Schema.String,
  }),
  Schema.TaggedStruct('Superseded', { ...envelope, generation: ProjectGeneration.schema }),
  Schema.TaggedStruct('Commit', {
    ...envelope,
    generation: ProjectGeneration.schema,
    uris: Schema.Array(Schema.String),
  }),
  Schema.TaggedStruct('Failure', {
    ...envelope,
    generation: ProjectGeneration.schema,
    incident: IncidentId.schema,
    message: Schema.String,
  }),
  Schema.TaggedStruct('Result', {
    ...envelope,
    generation: ProjectGeneration.schema,
    requestId: RequestId.schema,
    result: Schema.Unknown,
  }),
  Schema.TaggedStruct('Stopped', { ...envelope }),
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
