import { readFileSync } from 'node:fs'
import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as MirVerification from '../src/MirVerification.js'

const renamedProvider = readFileSync(
  new URL('./fixtures/scheduler-fiber/renamed-provider.silk', import.meta.url),
)
const observation = readFileSync(
  new URL('./fixtures/scheduler-fiber/observation.silk', import.meta.url),
)
const forkChild = readFileSync(
  new URL('./fixtures/scheduler-fiber/fork-child.silk', import.meta.url),
)
const localScheduler = readFileSync(
  new URL('./fixtures/scheduler-fiber/local-scheduler.silk', import.meta.url),
)
const localSchedulerRoot = readFileSync(
  new URL('./fixtures/scheduler-fiber/local-scheduler-root.silk', import.meta.url),
)
const localSchedulerShutdown = readFileSync(
  new URL('./fixtures/scheduler-fiber/local-scheduler-shutdown.silk', import.meta.url),
)
const localSchedulerNestedCancellation = readFileSync(
  new URL('./fixtures/scheduler-fiber/local-scheduler-nested-cancellation.silk', import.meta.url),
)
const localSchedulerStaleReuse = readFileSync(
  new URL('./fixtures/scheduler-fiber/local-scheduler-stale-reuse.silk', import.meta.url),
)
const localSchedulerSemantics = readFileSync(
  new URL('./fixtures/scheduler-fiber/local-scheduler-semantics.silk', import.meta.url),
)
const localSchedulerTimers = readFileSync(
  new URL('./fixtures/scheduler-fiber/local-scheduler-timers.silk', import.meta.url),
)
const localSchedulerTimerBasic = readFileSync(
  new URL('./fixtures/scheduler-fiber/local-scheduler-timer-basic.silk', import.meta.url),
)
const localSchedulerTimerBasicText = localSchedulerTimerBasic.toString('utf8')
const invalidTimerChildRequirementSource = localSchedulerTimerBasicText
  .replace('struct ParentClock {', 'service Extra {}\n\nstruct ParentClock {')
  .replace(
    '? &mut Scheduler.Scheduler | &mut MonotonicClock.MonotonicClock {\n  run MonotonicClock.waitFor(1)',
    '? &mut Scheduler.Scheduler | &mut MonotonicClock.MonotonicClock | &mut Extra {\n  run MonotonicClock.waitFor(1)',
  )
const missingParentClockSource = localSchedulerTimerBasicText.replace(
  `  let mut clock = ParentClock { mark: SystemClock.make(0, 0) }
  let program = LocalScheduler.execute(&mut scheduler, root())
    |> Effect.provideMut<MonotonicClock.MonotonicClock>(&mut clock)
  return run move program`,
  '  return run LocalScheduler.execute(&mut scheduler, root())',
)
const localSchedulerImplementation = readFileSync(
  new URL('../stdlib/silk/local_scheduler.silk', import.meta.url),
  'utf8',
)

const taskIdBoundarySource = `${localSchedulerImplementation}

fn requireReserved(selected: ReservedIdentity | Refused) -> u64 {
  return match move selected {
    ReservedIdentity { identity } => identity.value
    Refused {} => u64.MIN
  }
}

fn verifyTaskIdRefusal(selected: ReservedIdentity | Refused, fresh: u64) -> i32 {
  return match move selected {
    ReservedIdentity { identity } => -3
    Refused {} => verifyFreshTaskId(fresh)
  }
}

fn verifyFreshTaskId(fresh: u64) -> i32 {
  if fresh != 0 { return -4 }
  return 42
}

fn taskIdBoundary() -> i32 {
  let mut nearLimit = TaskIdSource {
    next: 18446744073709551614,
    exhausted: false,
  }
  let first = requireReserved(reserveIdentityStep(&mut nearLimit))
  let second = requireReserved(reserveIdentityStep(&mut nearLimit))
  let refused = reserveIdentityStep(&mut nearLimit)
  let mut freshSource = TaskIdSource {
    next: 0,
    exhausted: false,
  }
  let fresh = requireReserved(reserveIdentityStep(&mut freshSource))
  if first != 18446744073709551614 { return -1 }
  if second != u64.MAX { return -2 }
  return verifyTaskIdRefusal(move refused, fresh)
}

pub fn main() -> i32 { return taskIdBoundary() }`

const describe = (value: unknown): string =>
  JSON.stringify(value, (_, current) =>
    typeof current === 'bigint' ? current.toString() : current,
  )

const ascii = (value: string): Uint8Array =>
  Uint8Array.from(value, (character) => character.charCodeAt(0))

const ordinalAllocator = `
import silk.allocator { Allocator as AllocationService }
import silk.hash as Hash
import silk.hash_map as HashMap
import silk.layout as Layout
import silk.option as Option
import silk.usize as usize

fn claimAllocation(self: &mut Audit) -> bool {
  self.requests = self.requests + 1
  if self.remaining == 0 {
    self.refusals = self.refusals + 1
    return false
  }
  self.remaining = self.remaining - 1
  return true
}

struct QuotaAllocator { control: Shared.Shared<Audit> }

effect fn allocate(
  self: &mut QuotaAllocator,
  layout: Layout.Layout,
) -> Allocation ! Allocator.OutOfMemoryError {
  let admitted = Shared.withMut(&self.control, claimAllocation)
  if !admitted { fail Allocator.OutOfMemoryError {} }
  let mut system = Allocator.systemAllocatorProvider()
  let pending = AllocationService.allocate(move layout)
    |> Effect.provideMut<AllocationService>(&mut system)
  return run pending
}

impl AllocationService for QuotaAllocator { allocate: QuotaAllocator.allocate }

fn allocationRefusalObserved() -> () { return () }
fn taskIdentityRefusalObserved() -> () { return () }
fn publicationInsertionRefused() -> () { return () }
fn publicationInsertionAccepted() -> () { return () }
fn publicationStoreEmpty() -> () { return () }
fn publicationStoreLeaked() -> () { return () }

struct OrdinalReadyNode { identity: u64 }
struct OrdinalReadyEndpoint { node: Shared.Shared<OrdinalReadyNode> }
struct OrdinalRootCompleted {}
struct OrdinalRootNeverFailed {}

fn ordinalReady(endpoint: &OrdinalReadyEndpoint) -> () {
  return ()
}
`

const originalRejectPrepared = `fn rejectPrepared(
  mailbox: &Shared.Shared<Scheduler.TaskMailbox>,
  submission: &Shared.Shared<Scheduler.SubmissionSlot>,
  audit: &Shared.Shared<Audit>,
) -> () {
  let response = Shared.with<
    Scheduler.TaskMailbox,
    Shared.Shared<Scheduler.PublicationResponse>
  >(
    mailbox,
    responseHandle,
  )
  let request = Shared.withMut(mailbox, takeRequest)
  let prepared = Shared.withMut(submission, takeSubmission)
  let selected = selectPrepared(move request, move prepared)
  finishRejected(move selected, move response, Shared.clone<Audit>(audit))
  return ()
}`

const ordinalRejectPrepared = `fn finishPublicationRejection(
  response: Shared.Shared<Scheduler.PublicationResponse>,
  audit: Shared.Shared<Audit>,
  wake: Intrinsic.Wake,
) -> () {
  let rejected = Shared.withMut(&response, rejectResponse)
  let recorded = Shared.withMut(&audit, recordRejection)
  drop response
  drop audit
  Intrinsic.wake(move wake)
  return ()
}

fn finishPublicationInsertion(
  store: &mut HashMap.HashMap<Scheduler.TaskId, Scheduler.PreparedTask>,
  identity: Scheduler.TaskId,
  response: Shared.Shared<Scheduler.PublicationResponse>,
  audit: Shared.Shared<Audit>,
  wake: Intrinsic.Wake,
  outcome: Result.Result<
    Option.Option<Scheduler.PreparedTask>,
    Allocator.OutOfMemoryError
  >,
) -> () {
  return match move outcome {
    Result.Result<
      Option.Option<Scheduler.PreparedTask>,
      Allocator.OutOfMemoryError
    >.Success { value: previous } =>
      finishAcceptedPublicationInsertion(
        move store,
        identity,
        move response,
        move audit,
        move wake,
        move previous,
      )
    Result.Result<
      Option.Option<Scheduler.PreparedTask>,
      Allocator.OutOfMemoryError
    >.Failure { error } =>
      finishRefusedPublicationInsertion(
        move store,
        move response,
        move audit,
        move wake,
        move error,
      )
  }
}

fn finishAcceptedPublicationInsertion(
  store: &mut HashMap.HashMap<Scheduler.TaskId, Scheduler.PreparedTask>,
  identity: Scheduler.TaskId,
  response: Shared.Shared<Scheduler.PublicationResponse>,
  audit: Shared.Shared<Audit>,
  wake: Intrinsic.Wake,
  previous: Option.Option<Scheduler.PreparedTask>,
) -> () {
  drop previous
  publicationInsertionAccepted()
  let removed = HashMap.remove<Scheduler.TaskId, Scheduler.PreparedTask>(move store, identity)
  drop removed
  publicationStoreEmpty()
  finishPublicationRejection(move response, move audit, move wake)
  return ()
}

fn finishRefusedPublicationInsertion(
  store: &HashMap.HashMap<Scheduler.TaskId, Scheduler.PreparedTask>,
  response: Shared.Shared<Scheduler.PublicationResponse>,
  audit: Shared.Shared<Audit>,
  wake: Intrinsic.Wake,
  error: Allocator.OutOfMemoryError,
) -> () {
  drop error
  publicationInsertionRefused()
  if HashMap.length<Scheduler.TaskId, Scheduler.PreparedTask>(store) == usize.ZERO {
    publicationStoreEmpty()
  } else {
    publicationStoreLeaked()
  }
  finishPublicationRejection(move response, move audit, move wake)
  return ()
}

effect fn rejectPrepared(
  mailbox: &Shared.Shared<Scheduler.TaskMailbox>,
  submission: &Shared.Shared<Scheduler.SubmissionSlot>,
  audit: &Shared.Shared<Audit>,
  control: &Shared.Shared<Audit>,
) -> () {
  let response = Shared.with<
    Scheduler.TaskMailbox,
    Shared.Shared<Scheduler.PublicationResponse>
  >(
    mailbox,
    responseHandle,
  )
  let request = Shared.withMut(mailbox, takeRequest)
  let prepared = Shared.withMut(submission, takeSubmission)
  let PreparedSelection { task, wake } = move selectPrepared(move request, move prepared)
  let identity = task.identity
  let mut store = HashMap.make<Scheduler.TaskId, Scheduler.PreparedTask>(Hash.seed(19))
  let mut allocator = QuotaAllocator { control: Shared.clone<Audit>(control) }
  let insertion = HashMap.insert<Scheduler.TaskId, Scheduler.PreparedTask>(
    &mut store,
    identity,
    move task,
  ) |> Effect.provideMut<Allocator.Allocator>(&mut allocator)
  let outcome = run Effect.result(move insertion)
  finishPublicationInsertion(
    &mut store,
    identity,
    move response,
    Shared.clone<Audit>(audit),
    move wake,
    move outcome,
  )
  drop allocator
  drop store
  return ()
}`

const allocationOrdinalSource = (quota: number): string => {
  const base = forkChild.toString()
  return base
    .replace(
      'struct Audit {',
      `${ordinalAllocator}\nstruct Audit {
  remaining: i32
  requests: i32
  refusals: i32`,
    )
    .replace('if self.identity.value == 3 {', 'if self.identity.value == 1 {')
    .replace(
      '  let mut allocator = Allocator.systemAllocatorProvider()\n  let preparedFiber',
      `  let mut allocator = QuotaAllocator {
    control: Shared.clone<Audit>(&self.audit),
  }
  let preparedFiber`,
    )
    .replace(
      `  let execution = run Execution.make(
    runChild<A, E>(
      move child,
      move childProvider,
      TaskClock {},
      move producer,
      move lifetime,
    ),
    (),
    ready,
  ) |> Effect.provideMut<Allocator.Allocator>(&mut allocator)`,
      `  let node = run Shared.make<OrdinalReadyNode>(OrdinalReadyNode {
    identity: identity.value,
  }) |> Effect.provideMut<Allocator.Allocator>(&mut allocator)
  let endpoint = OrdinalReadyEndpoint { node: move node }
  let execution = run Execution.make(
    runChild<A, E>(
      move child,
      move childProvider,
      TaskClock {},
      move producer,
      move lifetime,
    ),
    move endpoint,
    ordinalReady,
  ) |> Effect.provideMut<Allocator.Allocator>(&mut allocator)`,
    )
    .replace(
      '  providerPreparationFinished()\n  return move pending',
      '  drop allocator\n  providerPreparationFinished()\n  return move pending',
    )
    .replace(originalRejectPrepared, ordinalRejectPrepared)
    .replace(
      `effect fn rootWork(audit: Shared.Shared<Audit>) -> ()
! Allocator.OutOfMemoryError | Scheduler.TaskIdExhaustedError
? &mut Scheduler.Scheduler | &mut MonotonicClock.MonotonicClock {
  let child = run Fiber.forkChild<
    i32,
    Allocator.OutOfMemoryError | Scheduler.TaskIdExhaustedError,
  >(nestedWork())
  rootForkReturned()
  let outcome = run Fiber.await<
    i32,
    Allocator.OutOfMemoryError | Scheduler.TaskIdExhaustedError,
  >(move child)
  let nested = match move outcome {
    Fiber.Outcome<
      i32,
      Allocator.OutOfMemoryError | Scheduler.TaskIdExhaustedError,
    > { value } => match move value {
      Fiber.Success<i32> { value: success } => success
      Fiber.Failure<Allocator.OutOfMemoryError | Scheduler.TaskIdExhaustedError> { error } => -200
      Fiber.Cancelled {} => -300
    }
  }
  let rejected = run Effect.catchAll(forkRejected(), recoverRejected)
  let stored = Shared.withMut(&audit, recordResult(nested + rejected))
  drop audit
  return ()
}`,
      `effect fn rootWork(audit: Shared.Shared<Audit>) -> ()
! Allocator.OutOfMemoryError | Scheduler.TaskIdExhaustedError
? &mut Scheduler.Scheduler | &mut MonotonicClock.MonotonicClock {
  let rejected = run Effect.catchAll(forkRejected(), recoverRejected)
  let stored = Shared.withMut(&audit, recordResult(41 + rejected))
  drop audit
  return ()
}`,
    )
    .replace(
      `fn auditResult(self: &Audit) -> i32 {
  if self.rootPreparations != 2 { return -1 }
  if self.nestedPreparations != 1 { return -2 }
  if self.publications != 2 { return -3 }
  if self.rejections != 1 { return -4 }
  if self.rejectedReleases != 1 { return -5 }
  return self.result
}`,
      `fn auditResult(self: &Audit) -> i32 { return self.result }`,
    )
    .replace(
      `effect fn scenario() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let audit =`,
      `effect fn scenario() -> i32 ! Allocator.OutOfMemoryError {
  let mut bootstrap = Allocator.systemAllocatorProvider()
  let audit =`,
    )
    .replace(
      `  let audit = run Shared.make<Audit>(Audit {
    rootPreparations: 0,`,
      `  let audit = run Shared.make<Audit>(Audit {
    remaining: ${quota},
    requests: 0,
    refusals: 0,
    rootPreparations: 0,`,
    )
    .replace(
      `  }) |> Effect.provideMut<Allocator.Allocator>(&mut allocator)
  let identities`,
      `  }) |> Effect.provideMut<Allocator.Allocator>(&mut bootstrap)
  let mut allocator = QuotaAllocator { control: Shared.clone<Audit>(&audit) }
  let identities`,
    )
    .replace(
      `  let identities = run Shared.make<TaskIdentitySource>(TaskIdentitySource { next: 1 })
    |> Effect.provideMut<Allocator.Allocator>(&mut allocator)
  let rootMailbox`,
      `  let identities = run Shared.make<TaskIdentitySource>(TaskIdentitySource { next: 1 })
    |> Effect.provideMut<Allocator.Allocator>(&mut allocator)
  let rootCompletion = run Fiber.prepare<OrdinalRootCompleted, OrdinalRootNeverFailed>()
    |> Effect.provideMut<Allocator.Allocator>(&mut allocator)
  let Fiber.PreparedFiber<OrdinalRootCompleted, OrdinalRootNeverFailed> {
    producer: rootProducer,
    canceller: rootCanceller,
    fiber: rootFiber,
  } = move rootCompletion
  let mut rootStore = HashMap.make<Scheduler.TaskId, bool>(Hash.seed(11))
  let rootInsertion = HashMap.insert<Scheduler.TaskId, bool>(
    &mut rootStore,
    Scheduler.TaskId { value: 0 },
    true,
  ) |> Effect.provideMut<Allocator.Allocator>(&mut allocator)
  let rootPrevious = run rootInsertion
  drop rootPrevious
  let rootMailbox`,
    )
    .replace(
      `  let mut rootState = TaskRunState {
    phase: ExecutionEmpty {},
    completed: false,
  }
  let rootProgram`,
      `  let mut rootState = TaskRunState {
    phase: ExecutionEmpty {},
    completed: false,
  }
  let rootNode = run Shared.make<OrdinalReadyNode>(OrdinalReadyNode { identity: 0 })
    |> Effect.provideMut<Allocator.Allocator>(&mut allocator)
  let rootEndpoint = OrdinalReadyEndpoint { node: move rootNode }
  let rootProgram`,
    )
    .replace(
      `  let mut rootExecution = run Execution.make(move root, (), ready)
    |> Effect.provideMut<Allocator.Allocator>(&mut allocator)`,
      `  let mut rootExecution = run Execution.make(
    move root,
    move rootEndpoint,
    ordinalReady,
  ) |> Effect.provideMut<Allocator.Allocator>(&mut allocator)`,
    )
    .replace(
      / {2}run driveExecution\(move rootExecution, &mut rootState\)[\s\S]*? {2}drop nested/,
      `  run driveExecution(move rootExecution, &mut rootState)
  if !rootState.completed {
    run rejectPrepared(&rootDriverMailbox, &rootDriverSubmission, &audit, &audit)
    run driveStored(&mut rootState)
  }`,
    )
    .replace(
      `  drop rootDriverSubmission
  let result =`,
      `  drop rootDriverSubmission
  let rootRemoved = HashMap.remove<Scheduler.TaskId, bool>(
    &mut rootStore,
    Scheduler.TaskId { value: 0 },
  )
  drop rootRemoved
  drop rootStore
  drop rootProducer
  drop rootCanceller
  drop rootFiber
  let result =`,
    )
    .replace(
      '  rejectPrepared(&rootDriverMailbox, &rootDriverSubmission, &audit)',
      '  run rejectPrepared(&rootDriverMailbox, &rootDriverSubmission, &audit, &audit)',
    )
    .replace('  drop audit\n  return result', '  drop allocator\n  drop audit\n  return result')
    .replace(
      `effect fn recover(error: Allocator.OutOfMemoryError) -> i32 {
  drop error
  return -99
}`,
      `effect fn recover(error: Allocator.OutOfMemoryError) -> i32 {
  drop error
  allocationRefusalObserved()
  return 7
}`,
    )
}

it.effect('dispatches Scheduler.prepare to a renamed ordinary-source provider', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'scheduler-fiber/renamed-provider',
      new Uint8Array(renamedProvider),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [],
      describe(Analysis.diagnostics(snapshot)),
    )
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)
    assert.strictEqual(
      evaluated.trace.filter(
        (event) => event._tag === 'Call' && event.target.name === 'renamedPrepareCalled',
      ).length,
      1,
    )
    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
  }),
)

it.effect('reserves task identities through MAX, refuses exhaustion, and resets fresh state', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'scheduler-fiber/task-id-boundary',
      ascii(taskIdBoundarySource),
      'aarch64-apple-darwin',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [],
      describe(Analysis.diagnostics(snapshot)),
    )
    const outcome = Analysis.evaluate(snapshot)
    assert.strictEqual(outcome._tag, 'Completed', describe(outcome))
    if (outcome._tag === 'Completed') assert.strictEqual(outcome.result.value, 42n)
  }),
)

it.effect('observes immediate and pending Fibers with monotonic terminal signals', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'scheduler-fiber/observation',
      new Uint8Array(observation),
      'wasm32-unknown-unknown',
    )
    assert.deepEqual(
      Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
      [],
      describe(Analysis.diagnostics(snapshot)),
    )
    const evaluated = Analysis.evaluate(snapshot)
    assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
    if (evaluated._tag !== 'Completed') return
    assert.strictEqual(evaluated.result.value, 42n)

    const construction = evaluated.trace.findIndex(
      (event) => event._tag === 'Call' && event.target.name === 'constructionComplete',
    )
    assert.isAtLeast(construction, 0)
    assert.isFalse(
      evaluated.trace.slice(construction + 1).some((event) => event._tag === 'AllocationAcquire'),
      'terminal observation, pending registration, notification, and resumption must not allocate',
    )

    const transitions = evaluated.trace
      .filter((event) => event._tag === 'ExecutionTransition')
      .map((event) => event.event)
    assert.deepEqual(transitions, [
      'Initialize',
      'Initialize',
      'Initialize',
      'Initialize',
      'Initialize',
      'Initialize',
      'Initialize',
      'Initialize',
      'Drive',
      'Complete',
      'Drive',
      'Complete',
      'Drive',
      'Complete',
      'Drive',
      'Complete',
      'Drive',
      'Complete',
      'Drive',
      'Complete',
      'Drive',
      'Register',
      'RetainGuard',
      'Relinquish',
      'Drive',
      'Register',
      'RetainGuard',
      'Relinquish',
      'Notify',
      'Eligible',
      'Resume',
      'Drive',
      'Complete',
      'Notify',
      'Eligible',
      'Resume',
      'Drive',
      'Complete',
    ])

    const allocations = evaluated.trace.filter(
      (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
    )
    assert.strictEqual(
      allocations.filter((event) => event._tag === 'AllocationAcquire').length,
      allocations.filter((event) => event._tag === 'AllocationRelease').length,
      describe(allocations),
    )

    assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

    const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
    const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
    assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
  }),
)

it.effect(
  'publishes child Fibers atomically through a renamed Scheduler provider',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'scheduler-fiber/fork-child',
        new Uint8Array(forkChild),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        [],
        describe(Analysis.diagnostics(snapshot)),
      )
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)

      const calls = evaluated.trace.filter((event) => event._tag === 'Call')
      const callIndex = (name: string): number =>
        calls.findIndex((event) => event.target.name === name)
      const callCount = (name: string): number =>
        calls.filter((event) => event.target.name === name).length
      for (const name of [
        'rootForkReturned',
        'nestedBodyActivated',
        'nestedForkReturned',
        'closedBodyActivated',
        'rejectedTaskReleased',
        'rejectionObserved',
      ])
        assert.strictEqual(callCount(name), 1, name)
      assert.isBelow(callIndex('rootForkReturned'), callIndex('nestedBodyActivated'))
      assert.isBelow(callIndex('nestedForkReturned'), callIndex('closedBodyActivated'))
      assert.strictEqual(callIndex('rejectedBodyActivated'), -1)
      assert.strictEqual(callIndex('wrongRejectionFailure'), -1)
      assert.isBelow(callIndex('rejectedTaskReleased'), callIndex('rejectionObserved'))

      const preparationFinished = evaluated.trace.flatMap((event, index) =>
        event._tag === 'Call' && event.target.name === 'providerPreparationFinished' ? [index] : [],
      )
      assert.lengthOf(preparationFinished, 3)
      for (const finished of preparationFinished) {
        const nextRegister = evaluated.trace.findIndex(
          (event, index) =>
            index > finished && event._tag === 'ExecutionTransition' && event.event === 'Register',
        )
        assert.isAbove(nextRegister, finished)
      }

      const transitions = evaluated.trace.filter((event) => event._tag === 'ExecutionTransition')
      assert.strictEqual(
        transitions.filter((transition) => transition.event === 'Register').length,
        5,
        describe(transitions),
      )
      assert.strictEqual(
        transitions.filter((transition) => transition.event === 'Notify').length,
        5,
        describe(transitions),
      )

      const acquiredTickets = evaluated.trace.flatMap((event) =>
        event._tag === 'AllocationAcquire' ? [event.ticket] : [],
      )
      const releasedTickets = evaluated.trace.flatMap((event) =>
        event._tag === 'AllocationRelease' ? [event.ticket] : [],
      )
      assert.deepEqual(
        releasedTickets.toSorted((left, right) => left - right),
        acquiredTickets.toSorted((left, right) => left - right),
        describe({ acquiredTickets, releasedTickets }),
      )
      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
    }),
  { timeout: 120_000 },
)

it.effect(
  'executes a scheduler-owned root with generic success and failure outcomes',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'scheduler-fiber/local-scheduler',
        new Uint8Array(localScheduler),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        [],
        describe(Analysis.diagnostics(snapshot)),
      )
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)

      const calls = evaluated.trace.filter((event) => event._tag === 'Call')
      const callIndex = (name: string): number =>
        calls.findIndex((event) => event.target.name === name)
      assert.isAtLeast(callIndex('rootStarted'), 0)
      assert.isBelow(callIndex('rootStarted'), callIndex('forkReturned'))
      assert.isBelow(callIndex('forkReturned'), callIndex('childStarted'))
      assert.isBelow(callIndex('childStarted'), callIndex('rootResumed'))

      const allocations = evaluated.trace.filter(
        (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
      )
      assert.strictEqual(
        allocations.filter((event) => event._tag === 'AllocationAcquire').length,
        allocations.filter((event) => event._tag === 'AllocationRelease').length,
        describe(allocations),
      )

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
  { timeout: 120_000 },
)

it.effect(
  'suspends timer Fibers through a scripted parent clock without blocking siblings',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'scheduler-fiber/local-scheduler-timers',
        new Uint8Array(localSchedulerTimers),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        [],
        describe(Analysis.diagnostics(snapshot)),
      )
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)

      const calls = evaluated.trace.filter((event) => event._tag === 'Call')
      for (const name of [
        'recordFirstTimer',
        'recordSecondTimer',
        'recordEarlyTimer',
        'recordMiddleTimer',
        'recordLateTimer',
        'recordCancellationEarlyTimer',
        'recordCancellationLateTimer',
        'recordInnerTimer',
        'recordFairnessTimer',
      ]) {
        assert.strictEqual(
          calls.filter((event) => event.target.name === name).length,
          1,
          `${name} must run exactly once`,
        )
      }

      const onlyTraceCall = (name: string): number => {
        const indices = evaluated.trace.flatMap((event, index) =>
          event._tag === 'Call' && event.target.name === name ? [index] : [],
        )
        assert.lengthOf(indices, 1, name)
        return indices[0] ?? -1
      }
      const beforeCachedReads = onlyTraceCall('beforeCachedReads')
      const afterCachedReads = onlyTraceCall('afterCachedReads')
      const cachedReadTrace = evaluated.trace.slice(beforeCachedReads + 1, afterCachedReads)
      assert.isFalse(
        cachedReadTrace.some(
          (event) => event._tag === 'ExecutionTransition' && event.event === 'Register',
        ),
        'task-local clock reads must not park',
      )
      assert.isFalse(
        cachedReadTrace.some(
          (event) => event._tag === 'Call' && /^parentNow(?:\$|$)/.test(event.target.name),
        ),
        'task-local clock reads must not dispatch to the parent provider',
      )
      assert.isFalse(
        cachedReadTrace.some(
          (event) => event._tag === 'Call' && /^parentResolution(?:\$|$)/.test(event.target.name),
        ),
        'task-local resolution reads must use the cached parent resolution',
      )

      const beforeImmediateWaits = onlyTraceCall('beforeImmediateWaits')
      const afterImmediateWaits = onlyTraceCall('afterImmediateWaits')
      const immediateWaitTrace = evaluated.trace.slice(
        beforeImmediateWaits + 1,
        afterImmediateWaits,
      )
      assert.isFalse(
        immediateWaitTrace.some(
          (event) => event._tag === 'ExecutionTransition' && event.event === 'Register',
        ),
        'zero and past-deadline waits must not park',
      )
      assert.isFalse(
        immediateWaitTrace.some(
          (event) => event._tag === 'Call' && /^parentNow(?:\$|$)/.test(event.target.name),
        ),
        'zero and past-deadline waits must not dispatch to the parent provider',
      )
      assert.isFalse(
        immediateWaitTrace.some(
          (event) =>
            event._tag === 'Call' && /^parentWait(?:Until|For)(?:\$|$)/.test(event.target.name),
        ),
        'zero and past-deadline waits must not block through the parent provider',
      )

      const beforeRelativeWait = onlyTraceCall('beforeRelativeWait')
      const afterRelativeWait = onlyTraceCall('afterRelativeWait')
      const registration = evaluated.trace.findIndex(
        (event, index) =>
          index > beforeRelativeWait &&
          event._tag === 'ExecutionTransition' &&
          event.event === 'Register',
      )
      const nextDrive = evaluated.trace.findIndex(
        (event, index) =>
          index > registration &&
          event._tag === 'Call' &&
          /^driveIdentity(?:\$|$)/.test(event.target.name),
      )
      assert.isAbove(registration, beforeRelativeWait)
      assert.isAbove(nextDrive, registration)
      assert.isAbove(afterRelativeWait, nextDrive)
      const armedTimer = evaluated.trace.findIndex(
        (event, index) =>
          index > registration &&
          event._tag === 'Call' &&
          /^armTimer(?:\$|$)/.test(event.target.name),
      )
      assert.isAbove(armedTimer, registration)
      assert.strictEqual(
        evaluated.trace
          .slice(beforeRelativeWait + 1, nextDrive)
          .filter((event) => event._tag === 'ExecutionTransition' && event.event === 'Register')
          .length,
        1,
        'one positive relative wait must park exactly once before control returns to the driver',
      )
      assert.strictEqual(
        evaluated.trace
          .slice(registration + 1, armedTimer)
          .filter((event) => event._tag === 'Call' && /^parentNow(?:\$|$)/.test(event.target.name))
          .length,
        1,
        'the relative wait must sample the parent clock exactly once before arming its timer',
      )
      assert.isFalse(
        evaluated.trace
          .slice(registration + 1, armedTimer + 1)
          .some((event) => event._tag === 'AllocationAcquire'),
        'timer installation and arming must use prefunded storage',
      )

      const cancelledTimerActivated = onlyTraceCall('cancelledMiddleTimerActivated')
      const cancelledTimerReleased = onlyTraceCall('cancelledMiddleTimerReleased')
      const cancellationEarlyTimer = onlyTraceCall('recordCancellationEarlyTimer')
      const cancellationLateTimer = onlyTraceCall('recordCancellationLateTimer')
      const cancellationDisarm = evaluated.trace.findLastIndex(
        (event, index) =>
          index < cancelledTimerReleased &&
          event._tag === 'Call' &&
          event.target.name === 'disarmRegistration',
      )
      assert.isAbove(cancellationDisarm, cancelledTimerActivated)
      assert.isBelow(cancelledTimerActivated, cancelledTimerReleased)
      assert.isBelow(cancelledTimerReleased, cancellationEarlyTimer)
      assert.isBelow(cancellationEarlyTimer, cancellationLateTimer)
      assert.isFalse(
        evaluated.trace
          .slice(cancellationDisarm, cancelledTimerReleased)
          .some((event) => event._tag === 'AllocationAcquire'),
        'timer cancellation and arbitrary-index removal must not allocate',
      )
      assert.isFalse(
        evaluated.trace.some(
          (event) => event._tag === 'Call' && event.target.name === 'cancelledMiddleTimerResumed',
        ),
        'the cancelled middle timer must never resume',
      )

      const acquiredTickets = evaluated.trace.flatMap((event) =>
        event._tag === 'AllocationAcquire' ? [event.ticket] : [],
      )
      const releasedTickets = evaluated.trace.flatMap((event) =>
        event._tag === 'AllocationRelease' ? [event.ticket] : [],
      )
      assert.deepEqual(
        releasedTickets.toSorted((left, right) => left - right),
        acquiredTickets.toSorted((left, right) => left - right),
        describe({ acquiredTickets, releasedTickets }),
      )
    }),
  { timeout: 300_000 },
)

it.effect(
  'agrees across evaluator and direct Wasm for one ready sibling and one timer',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'scheduler-fiber/local-scheduler-timer-basic',
        new Uint8Array(localSchedulerTimerBasic),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        [],
        describe(Analysis.diagnostics(snapshot)),
      )
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
  { timeout: 120_000 },
)

it.effect(
  'enforces child and parent clock requirement-row boundaries',
  () =>
    Effect.gen(function* () {
      const invalidChild = yield* Analysis.ofSource(
        'scheduler-fiber/invalid-timer-child-requirement',
        ascii(invalidTimerChildRequirementSource),
      )
      assert.deepEqual(
        Analysis.diagnostics(invalidChild).map((diagnostic) => diagnostic.code),
        ['SEM0012'],
      )

      const missingParentClock = yield* Analysis.ofSource(
        'scheduler-fiber/missing-parent-clock',
        ascii(missingParentClockSource),
      )
      assert.deepEqual(
        Analysis.diagnostics(missingParentClock).map((diagnostic) => diagnostic.code),
        ['SEM0071'],
      )
    }),
  { timeout: 120_000 },
)

it.effect(
  'extracts generic root outcomes without externally parking execute',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'scheduler-fiber/local-scheduler-root',
        new Uint8Array(localSchedulerRoot),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        [],
        describe(Analysis.diagnostics(snapshot)),
      )
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)
      assert.strictEqual(
        evaluated.trace.filter(
          (event) =>
            event._tag === 'Call' &&
            (event.target.name === 'successStarted' || event.target.name === 'failureStarted'),
        ).length,
        2,
      )

      const allocations = evaluated.trace.filter(
        (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
      )
      assert.strictEqual(
        allocations.filter((event) => event._tag === 'AllocationAcquire').length,
        allocations.filter((event) => event._tag === 'AllocationRelease').length,
        describe(allocations),
      )

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
  { timeout: 120_000 },
)

it.effect(
  'shuts down typed LocalScheduler runs and reuses one provider without stale state',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'scheduler-fiber/local-scheduler-shutdown',
        new Uint8Array(localSchedulerShutdown),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        [],
        describe(Analysis.diagnostics(snapshot)),
      )
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)

      const callIndices = (name: string): ReadonlyArray<number> =>
        evaluated.trace.flatMap((event, index) =>
          event._tag === 'Call' && event.target.name === name ? [index] : [],
        )
      const onlyCall = (name: string): number => {
        const indices = callIndices(name)
        assert.lengthOf(indices, 1, name)
        return indices[0] ?? -1
      }

      const completedSiblingDropped = onlyCall('completedSiblingHandleDropped')
      const completedSiblingActivated = onlyCall('completedSiblingActivated')
      const completedSiblingReleased = onlyCall('completedSiblingReleased')
      const successDropped = onlyCall('successHandleDropped')
      const successActivated = onlyCall('successChildActivated')
      const successLeafActivated = onlyCall('successLeafActivated')
      const successTerminal = onlyCall('successTerminating')
      const successReleased = onlyCall('successChildReleased')
      const successLeafReleased = onlyCall('successLeafReleased')
      const successReturned = onlyCall('successReturned')
      assert.isBelow(successDropped, successActivated)
      assert.isBelow(successActivated, completedSiblingDropped)
      assert.isBelow(completedSiblingDropped, completedSiblingActivated)
      assert.isBelow(completedSiblingActivated, completedSiblingReleased)
      assert.isBelow(completedSiblingReleased, successLeafActivated)
      assert.isBelow(successLeafActivated, successTerminal)
      assert.isBelow(successActivated, successTerminal)
      assert.isBelow(successTerminal, successReleased)
      assert.isBelow(successTerminal, successLeafReleased)
      assert.isBelow(successReleased, successLeafReleased)
      assert.isBelow(successReleased, successReturned)
      assert.isBelow(successLeafReleased, successReturned)

      const failureDropped = onlyCall('failureHandleDropped')
      const failureActivated = onlyCall('failureChildActivated')
      const failureTerminal = onlyCall('failureTerminating')
      const failureReleased = onlyCall('failureChildReleased')
      const failureRecovered = onlyCall('failureRecovered')
      assert.isBelow(failureDropped, failureActivated)
      assert.isBelow(failureActivated, failureTerminal)
      assert.isBelow(failureTerminal, failureReleased)
      assert.isBelow(failureReleased, failureRecovered)

      assert.lengthOf(callIndices('escapedChildActivated'), 0)
      const escapeTerminal = onlyCall('escapeTerminating')
      const escapeReturned = onlyCall('escapedReturned')
      const escapedCancelled = onlyCall('escapedCancelled')
      assert.isBelow(escapeTerminal, escapeReturned)
      assert.isBelow(escapeReturned, escapedCancelled)
      assert.isFalse(
        evaluated.trace
          .slice(escapeReturned + 1, escapedCancelled)
          .some((event) => event._tag === 'ExecutionTransition' && event.event === 'Register'),
        'a handle cancelled by the previous run must be immediately observable',
      )

      const stalledRegistered = onlyCall('stalledRegistered')
      const stalledReleased = onlyCall('stalledRootReleased')
      const stalledRecovered = onlyCall('stalledRecovered')
      assert.isBelow(stalledRegistered, stalledReleased)
      assert.isBelow(stalledReleased, stalledRecovered)
      assert.isBelow(stalledRecovered, onlyCall('freshStarted'))
      assert.isBelow(onlyCall('freshStarted'), onlyCall('freshReturned'))
      assert.lengthOf(callIndices('resumedUnexpectedly'), 0)

      const shutdownIntervals: ReadonlyArray<readonly [number, number]> = [
        [successTerminal, successReturned],
        [failureTerminal, failureRecovered],
        [escapeTerminal, escapeReturned],
        [stalledRegistered, stalledRecovered],
      ]
      for (const [terminal, returned] of shutdownIntervals) {
        assert.isFalse(
          evaluated.trace
            .slice(terminal + 1, returned)
            .some((event) => event._tag === 'AllocationAcquire'),
          'typed shutdown must use only preallocated task links',
        )
      }

      const transitions = evaluated.trace.filter((event) => event._tag === 'ExecutionTransition')
      const transitionCount = (event: (typeof transitions)[number]['event']): number =>
        transitions.filter((transition) => transition.event === event).length
      assert.strictEqual(transitionCount('Initialize'), 11, describe(transitions))
      assert.strictEqual(transitionCount('NotifyInitial'), 11, describe(transitions))
      assert.strictEqual(transitionCount('Complete'), 6, describe(transitions))
      assert.strictEqual(transitionCount('Cancel'), 5, describe(transitions))
      assert.strictEqual(transitionCount('Cleanup'), 4, describe(transitions))
      assert.strictEqual(transitionCount('Release'), 5, describe(transitions))
      assert.strictEqual(transitionCount('Register'), 11, describe(transitions))
      assert.strictEqual(transitionCount('Latch'), 2, describe(transitions))
      assert.strictEqual(transitionCount('Relinquish'), 9, describe(transitions))

      const allocations = evaluated.trace.filter(
        (event) => event._tag === 'AllocationAcquire' || event._tag === 'AllocationRelease',
      )
      const acquiredTickets = allocations.flatMap((event) =>
        event._tag === 'AllocationAcquire' ? [event.ticket] : [],
      )
      const releasedTickets = allocations.flatMap((event) =>
        event._tag === 'AllocationRelease' ? [event.ticket] : [],
      )
      assert.deepEqual(
        releasedTickets.toSorted((left, right) => left - right),
        acquiredTickets.toSorted((left, right) => left - right),
        describe({ acquiredTickets, releasedTickets }),
      )

      const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
      const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
      assert.strictEqual((instance.exports.silk_main as () => number)(), 42)
    }),
  // This is the suite's largest scheduler program and can exceed two minutes while the compiler's
  // parallel acceptance workers contend for CPU; focused runs remain substantially faster.
  { timeout: 300_000 },
)

it.effect(
  'cancels a parked nested LocalScheduler and every inner timer Fiber',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'scheduler-fiber/local-scheduler-nested-cancellation',
        new Uint8Array(localSchedulerNestedCancellation),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        [],
        describe(Analysis.diagnostics(snapshot)),
      )
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])
      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)

      const callIndices = (name: string): ReadonlyArray<number> =>
        evaluated.trace.flatMap((event, index) =>
          event._tag === 'Call' && event.target.name === name ? [index] : [],
        )
      const onlyCall = (name: string): number => {
        const indices = callIndices(name)
        assert.lengthOf(indices, 1, name)
        return indices[0] ?? -1
      }
      const published = onlyCall('nestedFiberPublished')
      const activated = onlyCall('nestedTimerActivated')
      const terminal = onlyCall('nestedOuterTerminating')
      const released = onlyCall('nestedTimerReleased')
      const returned = onlyCall('nestedOuterReturned')
      const cancelled = onlyCall('nestedCancellationObserved')
      assert.isBelow(published, activated)
      assert.isBelow(activated, terminal)
      assert.isBelow(terminal, released)
      assert.isBelow(released, returned)
      assert.isBelow(returned, cancelled)
      assert.lengthOf(callIndices('resumedUnexpectedly'), 0)

      const acquiredTickets = evaluated.trace.flatMap((event) =>
        event._tag === 'AllocationAcquire' ? [event.ticket] : [],
      )
      const releasedTickets = evaluated.trace.flatMap((event) =>
        event._tag === 'AllocationRelease' ? [event.ticket] : [],
      )
      assert.deepEqual(
        releasedTickets.toSorted((left, right) => left - right),
        acquiredTickets.toSorted((left, right) => left - right),
        describe({ acquiredTickets, releasedTickets }),
      )
    }),
  { timeout: 180_000 },
)

it.effect(
  'preserves FIFO Fiber semantics across yields, joins, nesting, and typed failure',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'scheduler-fiber/local-scheduler-semantics',
        new Uint8Array(localSchedulerSemantics),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        [],
        describe(Analysis.diagnostics(snapshot)),
      )
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)

      const callIndex = (name: string): number => {
        const indices = evaluated.trace.flatMap((event, index) =>
          event._tag === 'Call' && event.target.name === name ? [index] : [],
        )
        assert.lengthOf(indices, 1, name)
        return indices[0] ?? -1
      }
      const ordered = (names: ReadonlyArray<string>): void => {
        for (let index = 1; index < names.length; index += 1) {
          const previous = names[index - 1] ?? ''
          const current = names[index] ?? ''
          assert.isBelow(callIndex(previous), callIndex(current), `${previous} before ${current}`)
        }
      }

      ordered([
        'firstForkReturned',
        'firstSiblingActivated',
        'secondForkReturned',
        'secondSiblingActivated',
        'thirdForkReturned',
        'thirdSiblingActivated',
        'fifoRootTerminating',
      ])
      ordered([
        'rootBeforeYield',
        'yieldingChildStarted',
        'rootBetweenYields',
        'yieldingChildResumedOnce',
        'rootAfterYields',
        'yieldingChildResumedTwice',
        'yieldingChildJoined',
      ])
      ordered([
        'nestedStarted',
        'nestedForkReturned',
        'nestedLeafStarted',
        'nestedLeafJoined',
        'nestedFailureObserved',
      ])
      assert.isFalse(
        evaluated.trace.some(
          (event) => event._tag === 'Call' && event.target.name === 'cancelledChildActivated',
        ),
        'root termination must cancel its initially ready child before the child activates',
      )
      assert.isAtLeast(callIndex('cancelledOutcomeObserved'), 0)

      const childCompleted = callIndex('completedChildStarted')
      const beforeCompletedJoin = callIndex('beforeCompletedJoin')
      const afterCompletedJoin = callIndex('afterCompletedJoin')
      assert.isBelow(childCompleted, beforeCompletedJoin)
      assert.isBelow(beforeCompletedJoin, afterCompletedJoin)
      assert.isFalse(
        evaluated.trace
          .slice(beforeCompletedJoin + 1, afterCompletedJoin)
          .some((event) => event._tag === 'ExecutionTransition' && event.event === 'Register'),
        'joining an already completed child must not park',
      )
    }),
  { timeout: 120_000 },
)

it.effect(
  'keeps cancelled Wakes and stale ready nodes inert across scheduler reuse',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* Analysis.ofSourceRealized(
        'scheduler-fiber/local-scheduler-stale-reuse',
        new Uint8Array(localSchedulerStaleReuse),
        'wasm32-unknown-unknown',
      )
      assert.deepEqual(
        Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
        [],
        describe(Analysis.diagnostics(snapshot)),
      )
      assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

      const evaluated = Analysis.evaluate(snapshot)
      assert.strictEqual(evaluated._tag, 'Completed', describe(evaluated))
      if (evaluated._tag !== 'Completed') return
      assert.strictEqual(evaluated.result.value, 42n)

      const callIndices = (name: string): ReadonlyArray<number> =>
        evaluated.trace.flatMap((event, index) =>
          event._tag === 'Call' && event.target.name === name ? [index] : [],
        )
      const onlyCall = (name: string): number => {
        const indices = callIndices(name)
        assert.lengthOf(indices, 1, name)
        return indices[0] ?? -1
      }

      const retainedInstalled = onlyCall('retainedWakeInstalled')
      const retainedReleased = onlyCall('retainedGuardReleased')
      const retainedRecovered = onlyCall('retainedRunRecovered')
      const staleTerminal = onlyCall('staleRootTerminating')
      const staleReturned = onlyCall('staleRunReturned')
      const freshStarted = onlyCall('freshStarted')
      const wakeConsumed = onlyCall('cancelledWakeConsumed')
      const currentChildActivated = onlyCall('currentChildActivated')
      const freshReturned = onlyCall('freshReturned')
      assert.isBelow(retainedInstalled, retainedReleased)
      assert.isBelow(retainedReleased, retainedRecovered)
      assert.isBelow(retainedRecovered, staleTerminal)
      assert.isBelow(staleTerminal, staleReturned)
      assert.isBelow(staleReturned, freshStarted)
      assert.isBelow(freshStarted, wakeConsumed)
      assert.isBelow(wakeConsumed, currentChildActivated)
      assert.isBelow(currentChildActivated, freshReturned)
      assert.lengthOf(callIndices('staleChildActivated'), 0)
      assert.lengthOf(callIndices('resumedUnexpectedly'), 0)

      const freshEntry = evaluated.trace.slice(staleReturned + 1, freshStarted)
      assert.strictEqual(
        freshEntry.filter(
          (event) => event._tag === 'Call' && /^driveIdentity(?:\$|$)/.test(event.target.name),
        ).length,
        1,
        'the later execute must dispatch only its fresh root before that root starts',
      )

      const lateWake = evaluated.trace.slice(freshStarted + 1, wakeConsumed)
      assert.isFalse(
        lateWake.some(
          (event) =>
            event._tag === 'ExecutionTransition' &&
            (event.event === 'Notify' || event.event === 'Eligible'),
        ),
        'consuming a Wake retained from a cancelled run must not publish readiness',
      )
      assert.isTrue(
        lateWake.some((event) => event._tag === 'ExecutionTransition' && event.event === 'Release'),
        'the retained cancelled Wake must discharge its old package authority',
      )
      assert.isFalse(
        lateWake.some((event) => event._tag === 'Call' && event.target.name === 'notifyReady'),
        'the retained cancelled Wake must not call its old ready endpoint',
      )
    }),
  { timeout: 120_000 },
)

it.effect(
  'sweeps canonical Fiber preparation and publication allocation ordinals',
  () =>
    Effect.gen(function* () {
      const terminalQuota = 19
      for (let quota = 0; quota <= terminalQuota; quota++) {
        const label = `q${quota}`
        const source = allocationOrdinalSource(quota)
        const snapshot = yield* Analysis.ofSourceRealized(
          `scheduler-fiber/allocation-ordinals/${label}`,
          ascii(source),
          'wasm32-unknown-unknown',
        )
        assert.deepEqual(
          Analysis.diagnostics(snapshot).map((diagnostic) => diagnostic.code),
          [],
          `${label}: ${describe(Analysis.diagnostics(snapshot))}`,
        )

        const evaluated = Analysis.evaluate(snapshot)
        assert.strictEqual(evaluated._tag, 'Completed', `${label}: ${describe(evaluated)}`)
        if (evaluated._tag !== 'Completed') continue

        const publicationReached = 17 <= quota
        const rejectedLifetimeReached = 15 <= quota
        const childPreparationReached = 10 <= quota
        const expected = childPreparationReached ? 42 : 7
        assert.strictEqual(evaluated.result.value, BigInt(expected), label)

        const callCount = (name: string): number =>
          evaluated.trace.filter((event) => event._tag === 'Call' && event.target.name === name)
            .length
        assert.strictEqual(callCount('taskIdentityRefusalObserved'), 0, label)
        assert.strictEqual(callCount('wrongRejectionFailure'), 0, label)
        assert.strictEqual(callCount('rejectedBodyActivated'), 0, label)
        assert.strictEqual(callCount('allocationRefusalObserved'), quota < 10 ? 1 : 0, label)
        assert.strictEqual(
          callCount('publicationInsertionRefused'),
          quota === 17 || quota === 18 ? 1 : 0,
          label,
        )
        assert.strictEqual(
          callCount('publicationInsertionAccepted'),
          quota === terminalQuota ? 1 : 0,
          label,
        )
        assert.strictEqual(callCount('publicationStoreLeaked'), 0, label)
        assert.strictEqual(callCount('publicationStoreEmpty'), publicationReached ? 1 : 0, label)
        assert.strictEqual(
          callCount('rejectedTaskReleased'),
          rejectedLifetimeReached ? 1 : 0,
          label,
        )
        assert.strictEqual(callCount('rejectionObserved'), childPreparationReached ? 1 : 0, label)
        if (rejectedLifetimeReached) {
          const releasedTask = evaluated.trace.findIndex(
            (event) => event._tag === 'Call' && event.target.name === 'rejectedTaskReleased',
          )
          const observed = evaluated.trace.findIndex(
            (event) => event._tag === 'Call' && event.target.name === 'rejectionObserved',
          )
          assert.isBelow(releasedTask, observed, label)
        }
        if (publicationReached) {
          const callIndex = (name: string): number =>
            evaluated.trace.findIndex(
              (event) => event._tag === 'Call' && event.target.name === name,
            )
          const releasedTask = callIndex('rejectedTaskReleased')
          if (quota < terminalQuota)
            assert.isBelow(releasedTask, callIndex('publicationInsertionRefused'), label)
          else assert.isBelow(callIndex('publicationInsertionAccepted'), releasedTask, label)
        }

        const acquired = evaluated.trace.flatMap((event) =>
          event._tag === 'AllocationAcquire' ? [event.ticket] : [],
        )
        const released = evaluated.trace.flatMap((event) =>
          event._tag === 'AllocationRelease' ? [event.ticket] : [],
        )
        assert.strictEqual(acquired.length, quota < terminalQuota ? quota + 1 : 20, label)
        assert.deepEqual(
          released.toSorted((left, right) => left - right),
          acquired.toSorted((left, right) => left - right),
          `${label}: ${describe({ acquired, released })}`,
        )

        if (quota === terminalQuota)
          assert.deepEqual(MirVerification.verify(Analysis.loweredMir(snapshot)), [])

        const wasm = yield* Analysis.codegenWasm(snapshot, { mode: 'release' })
        const instance = new WebAssembly.Instance(new WebAssembly.Module(wasm.bytes.slice()), {})
        assert.strictEqual((instance.exports.silk_main as () => number)(), expected, label)
      }
    }),
  600_000,
)
