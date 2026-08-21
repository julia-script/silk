import { assert, it } from '@effect/vitest'
import * as Result from 'effect/Result'
import * as DocumentVersion from '../src/DocumentVersion.js'
import type * as EditorQuery from '../src/EditorQuery.js'
import * as IncidentId from '../src/IncidentId.js'
import * as ProjectGeneration from '../src/ProjectGeneration.js'
import * as QueryOutcome from '../src/QueryOutcome.js'
import * as RequestId from '../src/RequestId.js'
import * as WorkerEpoch from '../src/WorkerEpoch.js'
import * as WorkerProtocol from '../src/WorkerProtocol.js'

const outcomeTag = <A>(outcome: QueryOutcome.QueryOutcome<A>): string => {
  switch (outcome._tag) {
    case 'Ready':
    case 'Superseded':
    case 'Canceled':
    case 'DeadlineExceeded':
    case 'AnalysisFailed':
    case 'Unavailable':
    case 'Closed':
      return outcome._tag
  }
}

const queryTag = (query: EditorQuery.EditorQuery): EditorQuery.Tag => query._tag

it('keeps query outcomes and the editor query catalog exhaustive', () => {
  const incident = IncidentId.next(IncidentId.initial)
  const outcomes: ReadonlyArray<QueryOutcome.QueryOutcome<number>> = [
    QueryOutcome.ready(1),
    QueryOutcome.superseded,
    QueryOutcome.canceled,
    QueryOutcome.deadlineExceeded,
    QueryOutcome.analysisFailed(incident),
    QueryOutcome.unavailable,
    QueryOutcome.closed,
  ]
  assert.deepEqual(outcomes.map(outcomeTag), [
    'Ready',
    'Superseded',
    'Canceled',
    'DeadlineExceeded',
    'AnalysisFailed',
    'Unavailable',
    'Closed',
  ])

  const hover: EditorQuery.EditorQuery<'Hover'> = {
    _tag: 'Hover',
    uri: 'file:///workspace/Main.silk',
    parameters: { line: 0, character: 0 },
  }
  assert.strictEqual(queryTag(hover), 'Hover')
})

it('validates every host/worker envelope and rejects malformed messages', () => {
  const epoch = WorkerEpoch.initial
  const generation = ProjectGeneration.initial
  const requestId = RequestId.initial
  const initialized: WorkerProtocol.HostMessage = {
    protocolVersion: WorkerProtocol.version,
    _tag: 'Initialize',
    epoch,
    workspace: 'project:/workspace/silk.toml',
    sourceRoot: '/workspace',
  }
  const analyze: WorkerProtocol.HostMessage = {
    protocolVersion: WorkerProtocol.version,
    _tag: 'Analyze',
    epoch,
    generation,
    sources: [
      {
        uri: 'file:///workspace/Main.silk',
        version: DocumentVersion.make(1),
        workspace: 'project:/workspace/silk.toml',
        sourceRoot: '/workspace',
        module: 'Main',
        bytes: new TextEncoder().encode('pub fn main() -> i32 { return 42 }'),
      },
    ],
    invalidation: { dirtyPaths: [], rediscover: false },
  }
  const query: WorkerProtocol.HostMessage = {
    protocolVersion: WorkerProtocol.version,
    _tag: 'Query',
    epoch,
    generation,
    requestId,
    query: {
      _tag: 'Hover',
      uri: 'file:///workspace/Main.silk',
      parameters: { line: 0, character: 0 },
    },
  }
  const committed: WorkerProtocol.WorkerMessage = {
    protocolVersion: WorkerProtocol.version,
    _tag: 'Commit',
    epoch,
    generation,
    uris: ['file:///workspace/Main.silk'],
  }

  assert.isTrue(Result.isSuccess(WorkerProtocol.decodeHost(initialized)))
  assert.isTrue(Result.isSuccess(WorkerProtocol.decodeHost(analyze)))
  assert.isTrue(Result.isSuccess(WorkerProtocol.decodeHost(query)))
  assert.isTrue(Result.isSuccess(WorkerProtocol.decodeWorker(committed)))
  assert.isTrue(
    Result.isFailure(
      WorkerProtocol.decodeHost({
        ...initialized,
        protocolVersion: 2,
      }),
    ),
  )
  assert.isTrue(
    Result.isFailure(
      WorkerProtocol.decodeWorker({
        ...committed,
        generation: { _tag: 'ProjectGeneration', value: -1 },
      }),
    ),
  )
  assert.isTrue(Result.isFailure(WorkerProtocol.decodeWorker({ _tag: 'Commit' })))
})
