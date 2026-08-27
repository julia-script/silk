/**
 * The shared evaluation corpus: programs with pinned expected outcomes. The MIR interpreter's
 * tests consume it today; the native acceptance differential reuses it to compare interpreter
 * results against compiled output. Expected results were pinned against the fact-based
 * evaluator before the MIR retarget.
 */
import { readFileSync } from 'node:fs'
import * as Transcendental from '../../src/Transcendental.js'
import { floatMathPrograms } from './floatMath.js'
import { renameIndependentPolicy } from './independentPolicyRename.js'
import {
  auditAllocatorSuspension,
  mixedServiceProviderSuspension,
  ownedAllocatorSuspensionFailure,
  ownedAllocatorSuspensionSuccess,
  ownedProviderSuspendedFailure,
  ownedProviderSuspendedSuccess,
} from './ownedAllocatorSuspension.js'
import { storedCatchSuspension } from './storedCatchSuspension.js'

// folded from Transcendental.test.ts: the canonical-bits program is generated from the pinned
// high-precision vectors plus the fixed edge cases, so the expected bits can never drift from the
// reference implementation. Transcendental.test.ts imports this source for its IR assertions.
interface TranscendentalVector {
  readonly width: 32 | 64
  readonly inputBits: string
  readonly operation: 'Sin' | 'Cos'
}

const transcendentalFixture = JSON.parse(
  readFileSync(new URL('../fixtures/transcendental-vectors.json', import.meta.url), 'utf8'),
) as { readonly vectors: ReadonlyArray<TranscendentalVector> }

const transcendentalVectors: ReadonlyArray<TranscendentalVector> = [
  ...transcendentalFixture.vectors,
  { width: 32, inputBits: '0x00000000', operation: 'Sin' },
  { width: 32, inputBits: '0x80000000', operation: 'Sin' },
  { width: 32, inputBits: '0x00000000', operation: 'Cos' },
  { width: 32, inputBits: '0x7f800000', operation: 'Sin' },
  { width: 32, inputBits: '0xff800000', operation: 'Cos' },
  { width: 32, inputBits: '0x7fc12345', operation: 'Sin' },
  { width: 64, inputBits: '0x0000000000000000', operation: 'Sin' },
  { width: 64, inputBits: '0x8000000000000000', operation: 'Sin' },
  { width: 64, inputBits: '0x0000000000000000', operation: 'Cos' },
  { width: 64, inputBits: '0x7ff0000000000000', operation: 'Sin' },
  { width: 64, inputBits: '0xfff0000000000000', operation: 'Cos' },
  { width: 64, inputBits: '0x7ff8123456789abc', operation: 'Sin' },
]

/** Canonical-bits transcendental program: bit-exact sin/cos parity across every engine. */
export const transcendentalCanonicalBits = `import silk.f32 as f32
import silk.f64 as f64
pub fn main() -> i32 {
${transcendentalVectors
  .map((vector, index) => {
    const inputBits = BigInt(vector.inputBits)
    const expectedBits = Transcendental.evaluate(vector.operation, {
      width: vector.width,
      bits: inputBits,
    }).bits
    return `  if f${vector.width}.toBits(f${vector.width}.${vector.operation.toLowerCase()}(f${vector.width}.fromBits(${inputBits.toString()}))) != ${expectedBits.toString()} { return ${index + 1} }`
  })
  .join('\n')}
  return 42
}`

export interface CorpusProgram {
  readonly name: string
  readonly source: string
  readonly nativeSource?: string
  readonly nativeEnvironment?: Readonly<Record<string, string>>
  readonly expected:
    | { readonly _tag: 'Completes'; readonly result: number }
    | { readonly _tag: 'Trap' }
    | { readonly _tag: 'UnavailableEntry'; readonly reason: string }
}

export interface InvalidCorpusProgram {
  readonly name: string
  readonly source: string
  readonly codes: ReadonlyArray<string>
}

export const scalarEnumSignedAcceptance = `import silk.i8 as i8
enum(i8) Status {
  Unknown = -1,
  Ready = 7,
}
fn copyStatus(value: Status) -> Status {
  let copied = value
  if copied != Status.Ready { return copied }
  return Status.Unknown
}
pub fn main() -> i32 {
  let status = copyStatus(Status.Unknown)
  let raw = Status.value(status)
  if status == Status.Ready { return 1 }
  return match status {
    Status.Unknown => i8.toI32(raw) + 43
    Status.Ready => 2
  }
}`

export const scalarEnumLaneAcceptance = `enum(i8) SignedFlag {
  Negative = -1,
  Positive = 1,
}
enum(u8) ByteFlag {
  Low = 1,
  High = 255,
}
enum(i64) WideCode {
  Selected = 4294967297,
  Other = 9,
}
struct StoredFlags {
  signed: SignedFlag
  unsigned: ByteFlag
}
fn wideIdentity(value: WideCode) -> WideCode { return value }
fn inspect(flags: &StoredFlags) -> i32 {
  if flags.signed != SignedFlag.Negative { return 1 }
  if flags.unsigned != ByteFlag.High { return 2 }
  let selected = wideIdentity(WideCode.Selected)
  if WideCode.value(selected) != 4294967297 { return 3 }
  return match selected {
    WideCode.Selected => 42
    WideCode.Other => 4
  }
}
pub fn main() -> i32 {
  let flags = StoredFlags { signed: SignedFlag.Negative, unsigned: ByteFlag.High }
  return inspect(&flags)
}`

/** Two independent roots resume in reverse suspension order without sharing a frame stack. */
export const independentExecutionNonLifo = `import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
struct Empty {}
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored result: i32 }
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard { Intrinsic.wake(move wake) return Guard {} }
effect fn body(value: i32) -> i32 { run Execution.park(register) return value }
fn complete(owner: &mut Owner, result: i32) -> () { owner.result = result return () }
fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
fn ready(state: &()) -> () { return () }
effect fn driveOnce(execution: Intrinsic.Execution<i32>, owner: &mut Owner) -> () {
  return run Execution.drive(move execution, move owner, complete, suspend)
}
effect fn finish(selected: Empty | Stored, owner: &mut Owner) -> () {
  return match move selected {
    Empty {} => ()
    Stored { execution } => run finishStored(move execution, move owner)
  }
}
effect fn finishStored(execution: Intrinsic.Execution<i32>, owner: &mut Owner) -> () {
  return run Execution.drive(move execution, move owner, complete, suspend)
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut firstOwner = Owner { slot: Empty {}, result: 0 }
  let mut secondOwner = Owner { slot: Empty {}, result: 0 }
  let first = run Execution.make(body(20), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let second = run Execution.make(body(22), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  run driveOnce(move first, &mut firstOwner)
  run driveOnce(move second, &mut secondOwner)
  let selectedSecond = Intrinsic.replace(secondOwner.slot, Empty {})
  run finish(move selectedSecond, &mut secondOwner)
  let selectedFirst = Intrinsic.replace(firstOwner.slot, Empty {})
  run finish(move selectedFirst, &mut firstOwner)
  return secondOwner.result * 10 + firstOwner.result
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionIllegalDormantDrive = `import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
struct Empty {}
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored }
struct Guard { wake: Intrinsic.Wake }
fn register(wake: Intrinsic.Wake) -> Guard { return Guard { wake: move wake } }
effect fn body() -> i32 { run Execution.park(register) return 42 }
fn complete(owner: &mut Owner, result: i32) -> () { return () }
fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
fn ready(state: &()) -> () { return () }
effect fn driveOnce(execution: Intrinsic.Execution<i32>, owner: &mut Owner) -> () {
  return run Execution.drive(move execution, move owner, complete, suspend)
}
effect fn driveStored(selected: Empty | Stored, owner: &mut Owner) -> () {
  return match move selected {
    Empty {} => ()
    Stored { execution } => run driveOnce(move execution, move owner)
  }
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut owner = Owner { slot: Empty {} }
  let execution = run Execution.make(body(), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  run driveOnce(move execution, &mut owner)
  let selected = Intrinsic.replace(owner.slot, Empty {})
  run driveStored(move selected, &mut owner)
  return 0
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

/** Attempts to drive a Dormant execution reentrantly while its fixed endpoint is being notified. */
export const independentExecutionIllegalNotifyingDrive = `import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.shared as Shared
struct Empty {}
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored }
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard {
  Intrinsic.wake(move wake)
  return Guard {}
}
effect fn body() -> i32 {
  run Execution.park(register)
  return 42
}
fn install(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
fn take(owner: &mut Owner) -> Empty | Stored {
  return Intrinsic.replace(owner.slot, Empty {})
}
fn reentrantComplete(state: &mut (), result: i32) -> () { return () }
fn reentrantSuspend(state: &mut (), execution: Intrinsic.Execution<i32>) -> () {
  drop execution
  return ()
}
effect fn reenter(selected: Empty | Stored, state: &mut ()) -> () {
  return match move selected {
    Empty {} => ()
    Stored { execution } => run Execution.drive(
      move execution,
      move state,
      reentrantComplete,
      reentrantSuspend
    )
  }
}
fn ready(owner: &Shared.Shared<Owner>) -> () {
  let selected = Shared.withMut(owner, take)
  let mut state = ()
  run reenter(move selected, &mut state)
  return ()
}
fn complete(state: &mut (), result: i32) -> () { return () }
fn suspend(
  state: &mut (),
  execution: Intrinsic.Execution<i32>,
  owner: Shared.Shared<Owner>
) -> () {
  let installing = install(move execution)
  Shared.withMut(&owner, move installing)
  drop owner
  return ()
}
effect fn driveOnce<
  S: once fn(&mut (), Intrinsic.Execution<i32>) -> () + Intrinsic.NonParking
>(
  execution: Intrinsic.Execution<i32>,
  state: &mut (),
  onSuspend: S
) -> () {
  return run Execution.drive(move execution, move state, complete, move onSuspend)
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let owner = run Shared.make<Owner>(Owner { slot: Empty {} })
    |> Effect.provideMut<Allocator>(&mut allocator)
  let endpoint = Shared.clone(&owner)
  let suspensionOwner = Shared.clone(&owner)
  let onSuspend = suspend(move suspensionOwner)
  let execution = run Execution.make(body(), move endpoint, ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut state = ()
  run driveOnce(move execution, &mut state, move onSuspend)
  drop owner
  return 0
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

const fatalCallbackSentinel = `fn callbackSentinel(value: i32) -> () {
  let zero = value - value
  let impossible = 1 / zero
  drop impossible
  return ()
}`

export const independentExecutionIllegalDormantDriveObservable =
  independentExecutionIllegalDormantDrive
    .replace(
      'struct Owner { slot: Empty | Stored }',
      'struct Owner { slot: Empty | Stored callbacks: i32 }',
    )
    .replace('Owner { slot: Empty {} }', 'Owner { slot: Empty {}, callbacks: 0 }')
    .replace(
      'fn complete(owner: &mut Owner, result: i32) -> () { return () }',
      `${fatalCallbackSentinel}
fn complete(owner: &mut Owner, result: i32) -> () { return callbackSentinel(result) }`,
    )
    .replace(
      'fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {',
      `fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  if owner.callbacks != 0 { callbackSentinel(1) }
  owner.callbacks = owner.callbacks + 1`,
    )

export const independentExecutionIllegalNotifyingDriveObservable =
  independentExecutionIllegalNotifyingDrive
    .replace(
      'fn reentrantComplete(state: &mut (), result: i32) -> () { return () }',
      `${fatalCallbackSentinel}
fn reentrantComplete(state: &mut (), result: i32) -> () { return callbackSentinel(result) }`,
    )
    .replace(
      'fn reentrantSuspend(state: &mut (), execution: Intrinsic.Execution<i32>) -> () {',
      `fn reentrantSuspend(state: &mut (), execution: Intrinsic.Execution<i32>) -> () {
  callbackSentinel(1)`,
    )

/** Builds a logical continuation frame so a configured private-stack limit can reject its push. */
export const independentExecutionStackExhaustion = `import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
struct State { completed: i32 }
effect fn count(value: i32) -> i32 {
  if value == 0 { return 42 }
  let next = run Effect.suspend(effect { return value - 1 })
  return run count(next)
}
effect fn body() -> i32 { return run count(10000) }
fn ready(state: &()) -> () { return () }
fn complete(state: &mut State, value: i32) -> () { state.completed = 1 return () }
fn suspend(state: &mut State, execution: Intrinsic.Execution<i32>) -> () {
  drop execution
  return ()
}
effect fn driveOnce(execution: Intrinsic.Execution<i32>, state: &mut State) -> () {
  return run Execution.drive(move execution, move state, complete, suspend)
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut state = State { completed: 0 }
  let execution = run Execution.make(body(), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  run driveOnce(move execution, &mut state)
  return 0
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionStackExhaustionObservable =
  independentExecutionStackExhaustion.replace(
    'fn complete(state: &mut State, value: i32) -> () { state.completed = 1 return () }',
    `fn complete(state: &mut State, value: i32) -> () {
  let zero = value - value
  let impossible = 1 / zero
  drop impossible
  state.completed = 1
  return ()
}`,
  )

export const independentExecutionMultiplePackages = `import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
struct Empty {}
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored result: i32 }
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard { Intrinsic.wake(move wake) return Guard {} }
effect fn firstBody() -> i32 { run Execution.park(register) return 20 }
effect fn secondBody() -> i32 { run Execution.park(register) return 22 }
fn complete(owner: &mut Owner, result: i32) -> () { owner.result = result return () }
fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
fn ready(state: &()) -> () { return () }
effect fn driveOnce(execution: Intrinsic.Execution<i32>, owner: &mut Owner) -> () {
  return run Execution.drive(move execution, move owner, complete, suspend)
}
effect fn finish(selected: Empty | Stored, owner: &mut Owner) -> () {
  return match move selected {
    Empty {} => ()
    Stored { execution } => run driveOnce(move execution, move owner)
  }
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut firstOwner = Owner { slot: Empty {}, result: 0 }
  let mut secondOwner = Owner { slot: Empty {}, result: 0 }
  let first = run Execution.make(firstBody(), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let second = run Execution.make(secondBody(), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  run driveOnce(move first, &mut firstOwner)
  run driveOnce(move second, &mut secondOwner)
  let selectedSecond = Intrinsic.replace(secondOwner.slot, Empty {})
  run finish(move selectedSecond, &mut secondOwner)
  let selectedFirst = Intrinsic.replace(firstOwner.slot, Empty {})
  run finish(move selectedFirst, &mut firstOwner)
  return firstOwner.result + secondOwner.result
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionLateCancelledWake = `import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.shared as Shared
struct Empty {}
struct Waiting { wake: Intrinsic.Wake }
struct Mailbox { slot: Empty | Waiting }
struct Guard { mailbox: Shared.Shared<Mailbox> }
struct ReadyState { called: i32 }
fn install(mailbox: &mut Mailbox, wake: Intrinsic.Wake) -> () {
  let previous = Intrinsic.replace(mailbox.slot, Waiting { wake: move wake })
  drop previous
  return ()
}
fn register(wake: Intrinsic.Wake, mailbox: Shared.Shared<Mailbox>) -> Guard {
  let installing = install(move wake)
  Shared.withMut(&mailbox, move installing)
  return Guard { mailbox: move mailbox }
}
fn extract(mailbox: &mut Mailbox) -> Empty | Waiting {
  return Intrinsic.replace(mailbox.slot, Empty {})
}
effect fn body(mailbox: Shared.Shared<Mailbox>) -> i32 {
  let registration = register(move mailbox)
  run Execution.park(move registration)
  return 1
}
fn complete(state: &mut (), result: i32) -> () { return () }
fn cancel(state: &mut (), execution: Intrinsic.Execution<i32>) -> () {
  drop execution
  return ()
}
fn markReady(state: &mut ReadyState) -> () {
  state.called = 1
  return ()
}
fn ready(state: &Shared.Shared<ReadyState>) -> () {
  Shared.withMut(state, markReady)
  return ()
}
fn readReady(state: &mut ReadyState) -> i32 { return state.called }
effect fn driveOnce(execution: Intrinsic.Execution<i32>, state: &mut ()) -> () {
  return run Execution.drive(move execution, move state, complete, cancel)
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mailbox = run Shared.make<Mailbox>(Mailbox { slot: Empty {} })
    |> Effect.provideMut<Allocator>(&mut allocator)
  let registrationMailbox = Shared.clone(&mailbox)
  let readyState = run Shared.make<ReadyState>(ReadyState { called: 0 })
    |> Effect.provideMut<Allocator>(&mut allocator)
  let endpoint = Shared.clone(&readyState)
  let execution = run Execution.make(body(move registrationMailbox), move endpoint, ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut state = ()
  run driveOnce(move execution, &mut state)
  let selected = Shared.withMut(&mailbox, extract)
  drop mailbox
  let result = match move selected {
    Empty {} => 0
    Waiting { wake } => signalLate(move wake)
  }
  let called = Shared.withMut(&readyState, readReady)
  drop readyState
  return result + called * 1000
}
fn signalLate(wake: Intrinsic.Wake) -> i32 {
  Intrinsic.wake(move wake)
  return 42
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionReentrantDestroy = `import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.shared as Shared
struct Empty {}
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored }
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard {
  Intrinsic.wake(move wake)
  return Guard {}
}
effect fn body() -> i32 {
  run Execution.park(register)
  return 1
}
fn install(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
fn take(owner: &mut Owner) -> Empty | Stored {
  return Intrinsic.replace(owner.slot, Empty {})
}
fn ready(owner: &Shared.Shared<Owner>) -> () {
  let selected = Shared.withMut(owner, take)
  drop selected
  return ()
}
fn complete(state: &mut (), result: i32) -> () { return () }
fn suspend(
  state: &mut (),
  execution: Intrinsic.Execution<i32>,
  owner: Shared.Shared<Owner>
) -> () {
  let installing = install(move execution)
  Shared.withMut(&owner, move installing)
  drop owner
  return ()
}
effect fn driveOnce<
  S: once fn(&mut (), Intrinsic.Execution<i32>) -> () + Intrinsic.NonParking
>(
  execution: Intrinsic.Execution<i32>,
  state: &mut (),
  onSuspend: S
) -> () {
  return run Execution.drive(move execution, move state, complete, move onSuspend)
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let owner = run Shared.make<Owner>(Owner { slot: Empty {} })
    |> Effect.provideMut<Allocator>(&mut allocator)
  let endpoint = Shared.clone(&owner)
  let suspensionOwner = Shared.clone(&owner)
  let onSuspend = suspend(move suspensionOwner)
  let execution = run Execution.make(body(), move endpoint, ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut state = ()
  run driveOnce(move execution, &mut state, move onSuspend)
  drop owner
  return 42
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionLocalReactor = `import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.shared as Shared
struct Empty {}
struct Armed { wake: Intrinsic.Wake }
struct Reactor { slot: Empty | Armed }
struct Guard { reactor: Shared.Shared<Reactor> }
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored result: i32 }
fn install(reactor: &mut Reactor, wake: Intrinsic.Wake) -> () {
  let previous = Intrinsic.replace(reactor.slot, Armed { wake: move wake })
  drop previous
  return ()
}
fn register(wake: Intrinsic.Wake, reactor: Shared.Shared<Reactor>) -> Guard {
  let installing = install(move wake)
  Shared.withMut(&reactor, move installing)
  return Guard { reactor: move reactor }
}
fn extract(reactor: &mut Reactor) -> Empty | Armed {
  return Intrinsic.replace(reactor.slot, Empty {})
}
fn poll(reactor: &Shared.Shared<Reactor>) -> () {
  let selected = Shared.withMut(reactor, extract)
  return match move selected {
    Empty {} => ()
    Armed { wake } => Intrinsic.wake(move wake)
  }
}
effect fn body(reactor: Shared.Shared<Reactor>) -> i32 {
  let registration = register(move reactor)
  run Execution.park(move registration)
  return 42
}
fn complete(owner: &mut Owner, result: i32) -> () { owner.result = result return () }
fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
fn ready(state: &()) -> () { return () }
effect fn driveOnce(execution: Intrinsic.Execution<i32>, owner: &mut Owner) -> () {
  return run Execution.drive(move execution, move owner, complete, suspend)
}
effect fn finish(selected: Empty | Stored, owner: &mut Owner) -> () {
  return match move selected {
    Empty {} => ()
    Stored { execution } => run driveOnce(move execution, move owner)
  }
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let reactor = run Shared.make<Reactor>(Reactor { slot: Empty {} })
    |> Effect.provideMut<Allocator>(&mut allocator)
  let bodyReactor = Shared.clone(&reactor)
  let execution = run Execution.make(body(move bodyReactor), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let mut owner = Owner { slot: Empty {}, result: 0 }
  run driveOnce(move execution, &mut owner)
  poll(&reactor)
  let selected = Intrinsic.replace(owner.slot, Empty {})
  run finish(move selected, &mut owner)
  drop reactor
  return owner.result
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionRepeatedGenerations = `import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
struct Empty {}
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored result: i32 }
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard { Intrinsic.wake(move wake) return Guard {} }
effect fn body() -> i32 {
  run Execution.park(register)
  run Execution.park(register)
  return 42
}
fn ready(state: &()) -> () { return () }
fn complete(owner: &mut Owner, result: i32) -> () { owner.result = result return () }
fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
effect fn drive(execution: Intrinsic.Execution<i32>, owner: &mut Owner) -> () {
  return run Execution.drive(move execution, move owner, complete, suspend)
}
effect fn driveStored(selected: Empty | Stored, owner: &mut Owner) -> () {
  return match move selected {
    Empty {} => ()
    Stored { execution } => run drive(move execution, move owner)
  }
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut owner = Owner { slot: Empty {}, result: 0 }
  let execution = run Execution.make(body(), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  run drive(move execution, &mut owner)
  let first = Intrinsic.replace(owner.slot, Empty {})
  run driveStored(move first, &mut owner)
  let second = Intrinsic.replace(owner.slot, Empty {})
  run driveStored(move second, &mut owner)
  return owner.result
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionEligibleDrop = `import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
struct Empty {}
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored }
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard { Intrinsic.wake(move wake) return Guard {} }
effect fn body() -> i32 { run Execution.park(register) return 1 }
fn ready(state: &()) -> () { return () }
fn complete(owner: &mut Owner, result: i32) -> () { return () }
fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
effect fn drive(execution: Intrinsic.Execution<i32>, owner: &mut Owner) -> () {
  return run Execution.drive(move execution, move owner, complete, suspend)
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut owner = Owner { slot: Empty {} }
  let execution = run Execution.make(body(), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  run drive(move execution, &mut owner)
  let selected = Intrinsic.replace(owner.slot, Empty {})
  drop selected
  return 42
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionParkedTypedFailure = `import silk.allocator { Allocator }
import silk.allocator { Allocator }
import silk.effect as Effect
import silk.execution as Execution
import silk.result { Result, Success, Failure }
struct Failed { code: i32 }
struct Empty {}
struct Stored { execution: Intrinsic.Execution<Result<i32, Failed>> }
struct Owner { slot: Empty | Stored result: i32 }
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard { Intrinsic.wake(move wake) return Guard {} }
effect fn failed() -> i32 ! Failed { fail Failed { code: 42 } }
effect fn body() -> Result<i32, Failed> {
  run Execution.park(register)
  return run Effect.result(failed())
}
fn ready(state: &()) -> () { return () }
fn observe(result: Result<i32, Failed>) -> i32 {
  return match move result {
    Result<i32, Failed> { value: outcome } => match move outcome {
      Success<i32> { value } => value
      Failure<Failed> { error } => match move error { Failed { code } => code }
    }
  }
}
fn complete(owner: &mut Owner, result: Result<i32, Failed>) -> () {
  owner.result = observe(move result)
  return ()
}
fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<Result<i32, Failed>>) -> () {
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
effect fn drive(execution: Intrinsic.Execution<Result<i32, Failed>>, owner: &mut Owner) -> () {
  return run Execution.drive(move execution, move owner, complete, suspend)
}
effect fn driveStored(selected: Empty | Stored, owner: &mut Owner) -> () {
  return match move selected {
    Empty {} => ()
    Stored { execution } => run drive(move execution, move owner)
  }
}
effect fn program() -> i32 ! Allocator.OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut owner = Owner { slot: Empty {}, result: 0 }
  let execution = run Execution.make(body(), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  run drive(move execution, &mut owner)
  let selected = Intrinsic.replace(owner.slot, Empty {})
  run driveStored(move selected, &mut owner)
  return owner.result
}
effect fn recover(error: Allocator.OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const constrainedCallableForwarding = `import silk.effect as Effect
service Counter {
  effect fn get() -> i32 ? &Counter
}
struct Fixed { value: i32 }
effect fn get(self: &Fixed) -> i32 { return self.value }
impl Counter for Fixed { get: Fixed.get }
effect fn read() -> i32 ? &Counter { return run Counter.get() }
fn forward<F>(value: F) -> F { return move value }
fn forwardAgain<F>(value: F) -> F {
  let forwarded = forward(move value)
  return move forwarded
}
pub fn main() -> i32 {
  let fixed = Fixed { value: 42 }
  let bind = forwardAgain(Effect.provide<Counter>(&fixed))
  return run bind(read())
}`

/** Fixed-seed xoshiro256** known answers shared by evaluator, Wasm, and native execution. */
export const seededRandomFingerprint = `import silk.effect as Effect
import silk.random as Random
import silk.u64 as u64
import silk.usize as usize

fn matches(seed: u64, expected: &[u64]) -> bool {
  let mut provider = Random.seeded(seed)
  let mut index = usize.ZERO
  while index < expected.length {
    let actual = run Random.nextU64()
      |> Effect.provideMut<Random.Random>(&mut provider)
    if actual != expected[index] { return false }
    index = index + usize.ONE
  }
  return true
}

pub fn main() -> i32 {
  let expected = [
    u64.toU64(0x15780b2e0c2ec716),
    u64.toU64(0x6104d9866d113a7e),
    u64.toU64(0xae17533239e499a1),
    u64.toU64(0xecb8ad4703b360a1),
    u64.toU64(0xfde6dc7fe2ec5e64),
    u64.toU64(0xc50da53101795238),
    u64.toU64(0xb82154855a65ddb2),
    u64.toU64(0xd99a2743ebe60087)
  ]
  if !matches(42, &expected) { return 1 }
  return 42
}`

/** Failure payloads retain member bits while rows change their widest physical carrier lane. */
export const heterogeneousFailurePayload = `import silk.effect as Effect
struct Selected { code: i32 }
struct Small { code: i32 }
struct Wide { code: f64 }
effect fn risky() -> i32 ! Selected | Small { fail Small { code: 41 } }
effect fn recoverSelected(problem: Selected) -> i32 ! Wide { return problem.code }
effect fn recoverAny(problem: Small | Wide) -> i32 {
  return match move problem {
    Small { code } => code + 1
    Wide { code } => 0
  }
}
effect fn widenedResidual() -> i32 ! Small | Wide {
  return run Effect.catch<Selected>(risky(), recoverSelected)
}
effect fn completeResidual() -> i32 {
  return run Effect.catchAll(widenedResidual(), recoverAny)
}
pub fn main() -> i32 { return run completeResidual() }
`

/** Detached address payloads survive a row carrier widened by a floating handler failure. */
export const heterogeneousOwnedFailurePayload = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.layout { Layout }
import silk.effect as Effect
struct Selected { code: i32 }
struct Owned { storage: Allocation }
struct Wide { code: f64 }
effect fn risky() -> i32 ! Selected | Owned | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<i32>()
  let pending = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let storage = run pending
  fail Owned { storage: move storage }
}
effect fn recoverSelected(problem: Selected) -> i32 ! Wide { return problem.code }
fn release(storage: Allocation) -> i32 {
  drop storage
  return 42
}
effect fn recoverAny(problem: Owned | OutOfMemoryError | Wide) -> i32 {
  return match move problem {
    Owned { storage } => release(move storage)
    OutOfMemoryError {} => 0
    Wide { code } => 0
  }
}
effect fn widenedResidual() -> i32 ! Owned | OutOfMemoryError | Wide {
  return run Effect.catch<Selected>(risky(), recoverSelected)
}
effect fn completeResidual() -> i32 {
  return run Effect.catchAll(widenedResidual(), recoverAny)
}
pub fn main() -> i32 { return run completeResidual() }
`

/** Reified residual unions release owned address payloads from a floating carrier when dropped. */
export const heterogeneousOwnedFailureResultDrop = `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.layout { Layout }
import silk.effect as Effect
struct Selected { code: i32 }
struct Owned { storage: Allocation }
struct Wide { code: f64 }
effect fn risky() -> i32 ! Selected | Owned | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let pending = Allocator.allocate(Layout.of<i32>()) |> Effect.provideMut(&mut allocator)
  let storage = run pending
  fail Owned { storage: move storage }
}
effect fn recoverSelected(problem: Selected) -> i32 ! Wide { return problem.code }
pub fn main() -> i32 {
  let selected = Effect.catch<Selected>(risky(), recoverSelected)
  let completed = run Effect.result(move selected)
  drop completed
  return 42
}
`

const staticCompositionFixture = readFileSync(
  new URL('../fixtures/static-composition/static-composition-acceptance.silk', import.meta.url),
  'utf8',
)

const staticCompositionScenarios: ReadonlyArray<{
  readonly name: string
  readonly selection: string
  readonly result: number
  readonly cleanupWitness: boolean
}> = [
  { name: 'success', selection: 'RunRequest { value: 40 }', result: 42, cleanupWitness: false },
  { name: 'help', selection: 'HelpRequest {}', result: 10, cleanupWitness: false },
  {
    name: 'selection-failure',
    selection: 'SelectionFailureRequest {}',
    result: 20,
    cleanupWitness: false,
  },
  {
    name: 'decode-failure',
    selection: 'DecodeFailureRequest {}',
    result: 30,
    cleanupWitness: false,
  },
  {
    name: 'uncalled-cleanup',
    selection: 'UncalledCleanupRequest { value: 40 }',
    result: 40,
    cleanupWitness: true,
  },
  {
    name: 'called-cleanup',
    selection: 'CalledCleanupRequest { value: 40 }',
    result: 40,
    cleanupWitness: true,
  },
  {
    name: 'suspension',
    selection: 'SuspensionRequest { value: 42 }',
    result: 42,
    cleanupWitness: true,
  },
]

const selectStaticCompositionScenario = (selection: string): string =>
  staticCompositionFixture.replace('RunRequest {value: 40}', selection)

const withTrappingStaticCompositionDrop = (source: string): string =>
  source.replace(
    `    if self.value == 0 {
      let boom = 1 / 0
      return ()
    }
    self.value = 0
    return ()`,
    `    let values = [self.value]
    self.value = values[i32.toUsize(self.value)]
    return ()`,
  )

const staticCompositionCorpus: ReadonlyArray<CorpusProgram> = [
  ...staticCompositionScenarios.map(
    (scenario): CorpusProgram => ({
      name: `static-composition-${scenario.name}`,
      source: selectStaticCompositionScenario(scenario.selection),
      expected: { _tag: 'Completes', result: scenario.result },
    }),
  ),
  ...staticCompositionScenarios
    .filter((scenario) => scenario.cleanupWitness)
    .map(
      (scenario): CorpusProgram => ({
        name: `static-composition-${scenario.name}-cleanup-witness`,
        source: withTrappingStaticCompositionDrop(
          selectStaticCompositionScenario(scenario.selection),
        ),
        expected: { _tag: 'Trap' },
      }),
    ),
]

export const corpus: ReadonlyArray<CorpusProgram> = [
  {
    name: 'literal',
    source: 'pub fn main() -> i32 { return 42 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'scalar-enum-signed',
    source: scalarEnumSignedAcceptance,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'scalar-enum-lanes',
    source: scalarEnumLaneAcceptance,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'seeded-random-fingerprint',
    source: seededRandomFingerprint,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'staged-callable-section',
    source: `fn combine(a: i32, b: i32, c: i32) -> i32 {
  return a * 100 + b * 10 + c
}
pub fn main() -> i32 { return combine(3)(2)(1) }`,
    expected: { _tag: 'Completes', result: 123 },
  },
  {
    name: 'runtime-indexed-subplace-borrow',
    source: `fn edit(values: &mut [i32]) -> () { values[0] = 40 }
fn change(index: usize) -> i32 {
  let mut matrix = [[1, 2], [3, 4]]
  edit(&mut matrix[index])
  return matrix[index][0] + matrix[index][1]
}
pub fn main() -> i32 { return change(0) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'finite-effect-join',
    source: `struct First {}
struct Second {}
fn choose(input: First | Second) -> Effect<i32> {
  return match move input {
    First {} => effect { return 41 }
    Second {} => effect { return 42 }
  }
}
pub fn main() -> i32 { return run choose(First {}) }`,
    expected: { _tag: 'Completes', result: 41 },
  },
  // Alternatives with different capture arities exercise the composite's unified payload lanes:
  // every executor must place and read alternative captures through the registered calling shape.
  {
    name: 'finite-effect-join-capture-arity',
    source: `struct First {}
struct Second {}
fn choose(input: First | Second, a: i32, b: i32, c: i32) -> Effect<i32> {
  return match move input {
    First {} => effect { return a + b + c }
    Second {} => effect { return c }
  }
}
pub fn main() -> i32 {
  let wide = run choose(First {}, 11, 13, 16)
  let narrow = run choose(Second {}, 11, 13, 2)
  return wide + narrow
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'finite-effect-join-selected-cleanup',
    source: `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
struct First {}
struct Second {}
struct Guard { storage: Allocation }
impl Drop for Guard {
  fn drop(self: &mut Guard) -> () { return () }
}
fn choose(input: First | Second, guard: Guard) -> once Effect<i32> {
  return match move input {
    First {} => effect { drop move guard return 41 }
    Second {} => effect { drop move guard return 42 }
  }
}
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<i32>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let storage = run recipe
  let selected = choose(Second {}, Guard { storage: move storage })
  drop selected
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'constrained-callable-forwarding',
    source: constrainedCallableForwarding,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'identity',
    source: `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(42) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'second-parameter',
    source: `pub fn second(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { return second(10, 42) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'nested',
    source: `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'nested-siblings',
    source: `pub fn identity(value: i32) -> i32 { return value }
pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { return choose(identity(1), identity(2)) }`,
    expected: { _tag: 'Completes', result: 2 },
  },
  {
    name: 'generic-specializations',
    source: `struct Pair { left: i32 right: i32 }
struct Box<T> { value: T }
fn identity<T>(value: T) -> T { return move value }
pub fn main() -> i32 {
  let scalar = Box<i32> { value: identity(0) }
  let pair = Box<Pair> { value: identity<Pair>(Pair { left: 40, right: 2 }) }
  return scalar.value + pair.value.left + pair.value.right
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'generic-partial-type-arguments',
    source: `struct Box<T> { value: T }
fn pick<A, B>(left: A, right: B) -> A { return move left }
fn phantom<A, B>(value: A) -> A { return move value }
pub fn main() -> i32 {
  let picked = pick<i32>(40, true)
  let boxed = pick<Box<i32>>(Box<i32> { value: 1 }, picked)
  return picked + boxed.value + phantom<i32, bool>(1)
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'same-specialization-recursion',
    source: `fn recurse<T>(value: T, remaining: i32) -> i32 {
  if remaining > 0 { return recurse<T>(move value, remaining - 1) }
  return 42
}
pub fn main() -> i32 { return recurse<i32>(1, 4) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'recursive-aggregate-return',
    source: `struct Pair { left: i32 right: i32 }
fn build(remaining: i32) -> Pair {
  if remaining == 0 { return Pair { left: 40, right: 2 } }
  return build(remaining - 1)
}
pub fn main() -> i32 {
  let pair = build(4)
  return pair.left + pair.right
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'recursive-mutable-slice',
    source: `import silk.usize as usize
fn fill(values: &mut [i32], index: usize) -> i32 {
  if index == 4 { return values[0] + values[1] + values[2] + values[3] }
  values[index] = usize.toI32(index) + 9
  return fill(&mut values, index + 1)
}
pub fn main() -> i32 {
  let mut values = [0, 0, 0, 0]
  return fill(&mut values, 0)
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'forward-call',
    source: `pub fn main() -> i32 { return answer() }
pub fn answer() -> i32 { return 42 }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'direct-recursion',
    source: `fn countdown(value: i32) -> i32 {
  if value == 0 { return 42 }
  return countdown(value - 1)
}
pub fn main() -> i32 { return countdown(4) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'mutual-recursion',
    source: `fn even(value: i32) -> i32 {
  if value == 0 { return 42 }
  return odd(value - 1)
}
fn odd(value: i32) -> i32 { return even(value - 1) }
pub fn main() -> i32 { return odd(5) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'unknown-call-trap',
    source: 'pub fn main() -> i32 { return missing() }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'inner-blocked-trap',
    source: `pub fn identity(value: i32) -> i32 { return value }
pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { return choose(identity(1), missing(2)) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'binding',
    source: `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { let value = identity(42) return value }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'binding-chain',
    source: `pub fn main() -> i32 { let first = 40 let second = 2 return first }`,
    expected: { _tag: 'Completes', result: 40 },
  },
  {
    name: 'moved-binding',
    source: `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { let value = 42 return identity(move value) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'use-after-move-trap',
    source: `pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { let value = 42 return choose(move value, value) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'arithmetic',
    source:
      'import silk.i32 as i32\npub fn main() -> i32 { return i32.subtract(i32.multiply(6, 7), 0) }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'operator-precedence',
    source: 'pub fn main() -> i32 { return 2 + 5 * 8 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'operator-pipeline',
    source: 'import silk.i32 as i32\npub fn main() -> i32 { return 2 |> i32.add(40) }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'operator-interface-contract',
    source: `struct Vector { value: i32 }
fn scale(left: Vector, right: i32) -> Vector {
  return Vector { value: left.value * right }
}
fn dot(left: Vector, right: Vector) -> i32 {
  return left.value * right.value
}
interface Multiply<Right, Output> {
  operator * fn multiply(left: Self, right: Right) -> Output
}
impl Multiply<i32, Vector> for Vector { multiply: Vector.scale }
impl Multiply<Vector, i32> for Vector { multiply: Vector.dot }
fn doubled<T: Multiply<i32, T>>(value: T) -> T { return move value * 2 }
pub fn main() -> i32 {
  let scaled = doubled(Vector { value: 21 })
  let unit = Vector { value: 1 }
  return move scaled * move unit
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'closed-operator-surface',
    source: `pub fn main() -> i32 {
if 6 * 7 != 42 { return 0 }
if 84 / 2 != 42 { return 0 }
if 85 % 43 != 42 { return 0 }
if 44 - 2 != 42 { return 0 }
if 40 + 2 != 42 { return 0 }
if !(1 < 2) { return 0 }
if !(2 <= 2) { return 0 }
if !(3 > 2) { return 0 }
if !(3 >= 3) { return 0 }
if true != true { return 0 }
if false == true { return 0 }
return (40 + 2) * 1
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'unary-bool-pipeline',
    source:
      'import silk.bool as bool\npub fn main() -> i32 { if true |> bool.not { return 0 } return 42 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'signed-truncation',
    source:
      'import silk.i32 as i32\npub fn main() -> i32 { return i32.add(i32.divide(-7, 2), 45) }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'remainder-sign',
    source:
      'import silk.i32 as i32\npub fn main() -> i32 { return i32.add(i32.remainder(-7, 2), 43) }',
    expected: { _tag: 'Completes', result: 42 },
  },
  // MIN % -1 traps identically everywhere: the quotient overflows even though the mathematical
  // remainder is 0, matching ordinary arithmetic's invalid-remainder trap rule.
  {
    name: 'arith-convergence-remainder-min-trap',
    source: `import silk.i32 as i32
pub fn main() -> i32 { return i32.remainder(i32.subtract(-2147483647, 1), -1) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'arith-convergence-remainder-min-trap-i64',
    source: `import silk.i64 as i64
pub fn main() -> i32 {
  let minimum = i64.subtract(-9223372036854775807, 1)
  if i64.remainder(minimum, -1) != 0 { return 1 }
  return 2
}`,
    expected: { _tag: 'Trap' },
  },
  // The checked variant answers None exactly where the ordinary operation traps.
  {
    name: 'arith-convergence-checked-remainder-min-none',
    source: `import silk.i32 as i32
import silk.option { Option }
pub fn main() -> i32 {
  let minimum = i32.subtract(-2147483647, 1)
  if Option.unwrapOr<i32>(i32.checkedRemainder(minimum, -1), 42) != 42 { return 1 }
  if Option.unwrapOr<i32>(i32.checkedRemainder(7, -1), -1) != 0 { return 2 }
  if Option.unwrapOr<i32>(i32.checkedRemainder(minimum, 2), -1) != 0 { return 3 }
  if Option.unwrapOr<i32>(i32.checkedRemainder(7, 0), 42) != 42 { return 4 }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // Rotate counts wrap unsigned modulo the lane width, so negative counts rotate the other way
  // instead of degenerating into a plain shift.
  {
    name: 'arith-convergence-rotate-negative-count',
    source: `import silk.i32 as i32
import silk.i64 as i64
pub fn main() -> i32 {
  if i32.rotateLeft(5, -1) != i32.rotateLeft(5, 31) { return 1 }
  if i32.rotateLeft(5, -1) != -2147483646 { return 2 }
  if i32.rotateRight(5, -1) != i32.rotateRight(5, 31) { return 3 }
  if i32.rotateLeft(5, 33) != i32.rotateLeft(5, 1) { return 4 }
  if i64.rotateLeft(5, -1) != i64.rotateLeft(5, 63) { return 5 }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // Float remainder is exact IEEE fmod on every executor: no intermediate overflow for extreme
  // exponent differences and bit-exact results for ordinary operands.
  {
    name: 'arith-convergence-float-remainder-exact',
    source: `import silk.f32 as f32
import silk.f64 as f64
fn infinity() -> f64 { return 1e308 * 10.0 }
pub fn main() -> i32 {
  if f64.toBits(f64.remainder(10.5, 3.25)) != 4604930618986332160 { return 1 }
  if f64.toBits(f64.remainder(1e308, 1e-308)) != 708093261633040 { return 2 }
  if f64.remainder(5.0, infinity()) != 5.0 { return 3 }
  if !f64.isNaN(f64.remainder(infinity(), 3.0)) { return 4 }
  if !f64.isNaN(f64.remainder(5.0, 0.0)) { return 5 }
  if !f64.isNaN(f64.remainder(f64.fromBits(9221120237041090560), 3.0)) { return 6 }
  if !f64.isSignNegative(f64.remainder(-5.0, 1.0)) { return 7 }
  if f64.remainder(-5.0, 1.0) != 0.0 { return 8 }
  if f32.toBits(f32.remainder(3.4e38, 1.2e-38)) != 3993146 { return 9 }
  if f32.toBits(f32.remainder(10.5, 3.25)) != 1061158912 { return 10 }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // A binding referenced only as an enum-value argument still becomes an effect capture.
  {
    name: 'arith-convergence-effect-enum-value-capture',
    source: `import silk.i8 as i8
enum(i8) Status {
  Unknown = -1,
  Ready = 41,
}
pub fn main() -> i32 {
  let status = Status.Ready
  let deferred = effect { return i8.toI32(Status.value(status)) + 1 }
  return run deferred
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'overflow-trap',
    source: 'import silk.i32 as i32\npub fn main() -> i32 { return i32.add(2147483647, 1) }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'operator-negation-overflow-trap',
    source: 'pub fn main() -> i32 { return -(-2147483648) }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'divide-by-zero-trap',
    source: 'import silk.i32 as i32\npub fn main() -> i32 { return i32.divide(1, 0) }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'minimum-division-trap',
    source: 'import silk.i32 as i32\npub fn main() -> i32 { return i32.divide(-2147483648, -1) }',
    expected: { _tag: 'Trap' },
  },
  {
    name: 'branch-taken',
    source:
      'import silk.i32 as i32\npub fn main() -> i32 { if i32.equals(1, 1) { return 42 } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'branch-otherwise',
    source:
      'import silk.i32 as i32\npub fn main() -> i32 { if i32.equals(1, 2) { return 0 } return 42 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'branch-else',
    source:
      'import silk.i32 as i32\npub fn main() -> i32 { if i32.lessThan(2, 1) { return 1 } else { return 42 } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'bool-not',
    source:
      'import silk.bool as bool\nimport silk.i32 as i32\npub fn main() -> i32 { if bool.not(i32.equals(1, 2)) { return 42 } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'operator-bool-not',
    source: 'pub fn main() -> i32 { if !(1 == 2) { return 42 } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'bool-through-function',
    source: `import silk.i32 as i32
pub fn check(flag: bool) -> i32 { if flag { return 42 } return 0 }
pub fn main() -> i32 { return check(i32.greaterOrEqual(3, 3)) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'arm-binding',
    source:
      'import silk.i32 as i32\npub fn main() -> i32 { let base = 40 if i32.equals(base, 40) { let bonus = 2 return i32.add(base, bonus) } return 0 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-inferred',
    source: 'pub fn main() -> i32 { let values = [10, 42] return values[1] }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-contextual-empty',
    source: `fn empty() -> [i32; 0] { return [] }
fn consume(values: [i32; 0]) -> i32 { return 42 }
pub fn main() -> i32 { return consume(empty()) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-nested',
    source: `fn choose(values: [[i32; 2]; 2], outer: usize, inner: usize) -> i32 { return values[outer][inner] }
pub fn main() -> i32 { return choose([[10, 11], [42, 43]], 1, 0) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-indexed-struct-field',
    source: `struct Pair { left: i32 right: i32 }
fn choose(values: [Pair; 2], index: usize) -> i32 { return values[index].left }
pub fn main() -> i32 { return choose([Pair { left: 10, right: 11 }, Pair { left: 42, right: 43 }], 1) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-whole-move',
    source: `struct Token { value: i32 }
pub fn main() -> i32 {
  let tokens = [Token { value: 10 }, Token { value: 42 }]
  let moved = move tokens
  return moved[1].value
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'array-negative-index-trap',
    source: `fn choose(values: [i32; 2], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([10, 42], -1) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'array-upper-index-trap',
    source: `fn choose(values: [i32; 2], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([10, 42], 2) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'array-zero-index-trap',
    source: `fn choose(values: [i32; 0], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([], 0) }`,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'mutable-scalar-loop',
    source: `pub fn main() -> i32 {
  let mut count = 0
  while count < 42 { count = count + 1 }
  return count
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'mutable-array-loop',
    source: `import silk.usize as usize
pub fn main() -> i32 {
  let mut values = [40, 0]
  let mut index = usize.add(0, 0)
  while index < 2 {
    values[index] = values[index] + 1
    index = index + 1
  }
  return values[0] + values[1]
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'loop-continue-break',
    source: `import silk.usize as usize
pub fn main() -> i32 {
  let mut index = usize.add(0, 0)
  while index < 50 {
    index = index + 1
    if index == 2 { continue }
    if index == 42 { break }
  }
  return usize.toI32(index)
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'mutable-struct-loop',
    source: `struct Pair { left: i32 right: i32 }
pub fn main() -> i32 {
  let mut pair = Pair { left: 0, right: 40 }
  while pair.left < 2 { pair.left = pair.left + 1 }
  return pair.left + pair.right
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'match-guarded-union-shared',
    source: `struct Left { value: i32 }
struct Right { value: i32 }
fn inspect(input: Left | Right) -> i32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => answer + 1
    Right { value } => value
  }
}
pub fn main() -> i32 { return inspect(Left { value: 41 }) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'match-move-nested-cleanup',
    source: `struct Token { value: i32 }
struct Box { token: Token discarded: Token }
pub fn main() -> i32 {
  let box = Box { token: Token { value: 42 }, discarded: Token { value: 0 } }
  return match move box {
    Box { token: Token { value }, .. } => value
  }
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'match-universal-fallback',
    source: `struct Left { value: i32 }
struct Right { value: i32 }
fn inspect(input: Left | Right) -> i32 {
  return match &input { Left { value } => value _ => 42 }
}
pub fn main() -> i32 { return inspect(Right { value: 0 }) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'match-exclusive-mutable',
    source: `struct Token { value: i32 }
pub fn main() -> i32 {
  let mut token = Token { value: 42 }
  return match &mut token { Token { value } => value }
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'nested-loops',
    source: `pub fn main() -> i32 {
  let mut outer = 0
  let mut total = 0
  while outer < 6 {
    let mut inner = 0
    while inner < 7 {
      total = total + 1
      inner = inner + 1
    }
    outer = outer + 1
  }
  return total
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'missing-entry',
    source: 'pub fn answer() -> i32 { return 42 }',
    expected: { _tag: 'UnavailableEntry', reason: 'MissingEntry' },
  },
  {
    name: 'generic-entry',
    source: 'pub fn main<T>() -> i32 { return 42 }',
    expected: { _tag: 'UnavailableEntry', reason: 'GenericEntry' },
  },
  {
    name: 'parameterized-entry',
    source: 'pub fn main(value: i32) -> i32 { return value }',
    expected: { _tag: 'UnavailableEntry', reason: 'ParameterizedEntry' },
  },
  // folded from StringAcceptance.test.ts: literals, owned copy/view/append, exact equality, and
  // scalar traversal.
  {
    name: 'string-owned-scalars',
    source: `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.u32 as u32
import silk.string {
  ScalarCursor,
  ScalarStep,
  copy,
  append,
  view,
  scalarCursor,
  nextScalar,
  scalarValue,
  nextCursor
}
import silk.option { Some, None }
import silk.char { toU32 as charToU32 }

fn scalarSum(value: string, cursor: ScalarCursor) -> u32 {
  return match move nextScalar(value, move cursor) {
    Some<ScalarStep> { value: step } => continueSum(value, move step)
    None nothing => u32.toU32(0)
  }
}

fn continueSum(value: string, step: ScalarStep) -> u32 {
  let scalar = charToU32(scalarValue(&step))
  let cursor = nextCursor(move step)
  return scalar + scalarSum(value, move cursor)
}

effect fn build() -> i32 ! OutOfMemoryError {
  let literal = "A\\u{a2}"
  if literal == "A\\u{a2}" {} else { return 1 }
  if literal != "A\\u{a3}" {} else { return 2 }

  let mut allocator = Allocator.systemAllocatorProvider()
  let copying = copy(literal) |> Effect.provideMut(&mut allocator)
  let mut owned = run copying
  let appending = append(&mut owned, "\\u{20ac}\\u{10348}")
    |> Effect.provideMut(&mut allocator)
  let appended = run appending
  let borrowed = view(&owned)
  if borrowed == "A\\u{a2}\\u{20ac}\\u{10348}" {} else { return 3 }
  if scalarSum(borrowed, scalarCursor()) == u32.toU32(74967) {} else { return 4 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from UnicodeNormalization.test.ts: the two normalized owners compared directly, which
  // the evaluator and native answer correctly while direct WebAssembly still cannot.
  {
    name: 'unicode-compared-directly',
    source: `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.string { String, view }
import silk.unicode { normalizeNfc }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let left = run normalizeNfc("\\u{e9}") |> Effect.provideMut(&mut allocator)
  let right = run normalizeNfc("e\\u{301}") |> Effect.provideMut(&mut allocator)
  if view(&left) == view(&right) {} else { return 1 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from CharacterLiteral.test.ts: every accepted escape, multi-byte scalars, and the six
  // comparisons. The source deliberately carries non-ASCII literals.
  {
    name: 'character-literal-acceptance',
    source: `import silk.char { equals, notEquals, lessThan, lessOrEqual, greaterThan, greaterOrEqual }

const asciiSpace: char = ' '
const asciiTab: char = '\\t'
const snowman: char = '\\u{2603}'

fn eq(left: char, right: char) -> bool { return left == right }
fn ne(left: char, right: char) -> bool { return left != right }
fn lt(left: char, right: char) -> bool { return left < right }
fn le(left: char, right: char) -> bool { return left <= right }
fn gt(left: char, right: char) -> bool { return left > right }
fn ge(left: char, right: char) -> bool { return left >= right }

pub fn main() -> i32 {
  if eq('a', 'a') {} else { return 1 }
  if ne('a', 'b') {} else { return 2 }
  if lt('a', 'b') {} else { return 3 }
  if le('a', 'a') {} else { return 4 }
  if gt('b', 'a') {} else { return 5 }
  if ge('a', 'a') {} else { return 6 }
  if eq('\\n', '\\u{a}') {} else { return 7 }
  if eq('\\r', '\\u{d}') {} else { return 8 }
  if eq('\\t', asciiTab) {} else { return 9 }
  if eq('\\0', '\\u{0}') {} else { return 10 }
  if eq('\\\\', '\\u{5c}') {} else { return 11 }
  if eq('\\'', '\\u{27}') {} else { return 12 }
  if eq('\\"', '"') {} else { return 13 }
  if eq('\\x41', 'A') {} else { return 14 }
  if eq(' ', asciiSpace) {} else { return 15 }
  if eq('é', '\\u{e9}') {} else { return 16 }
  if eq('☃', snowman) {} else { return 17 }
  if lt('é', snowman) {} else { return 18 }
  if gt('😀', snowman) {} else { return 19 }
  if equals('a', 'a') {} else { return 20 }
  if notEquals('a', 'b') {} else { return 21 }
  if lessThan('a', 'b') {} else { return 22 }
  if lessOrEqual('a', 'b') {} else { return 23 }
  if greaterThan('b', 'a') {} else { return 24 }
  if greaterOrEqual('b', 'a') {} else { return 25 }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from ShortCircuitOperatorAcceptance.test.ts: the counter proves the right operand of
  // `&&`/`||` runs exactly when short-circuiting says it must.
  {
    name: 'short-circuit-counting',
    source: `fn bump(counter: &mut [i32], answer: bool) -> bool {
  counter[0] = counter[0] + 1
  return answer
}

fn conjunction(gate: bool) -> i32 {
  let mut counter = [0]
  if gate && bump(&mut counter, true) { return counter[0] }
  return counter[0]
}

fn disjunction(gate: bool) -> i32 {
  let mut counter = [0]
  if gate || bump(&mut counter, true) { return counter[0] }
  return counter[0]
}

pub fn main() -> i32 {
  if conjunction(false) == 0 {} else { return 1 }
  if conjunction(true) == 1 {} else { return 2 }
  if disjunction(true) == 0 {} else { return 3 }
  if disjunction(false) == 1 {} else { return 4 }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from Transcendental.test.ts: bit-exact sin/cos results across every engine.
  {
    name: 'transcendental-canonical-bits',
    source: transcendentalCanonicalBits,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from HashedCollections.test.ts: seeded map growth with checked reads.
  {
    name: 'hashed-map-growth',
    source: `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.i32 as i32
import silk.hash as Hash
import silk.hash { HashKey, HashSeed, Word }
import silk.hash_map { HashMap, bucketCount, contains, get, insert, length, make, remove }
import silk.option { Option, Some, None }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut map = make<Word, i32>(Hash.seed(4242))
  let mut key = 0
  while key < 40 {
    let previous = run insert<Word, i32>(&mut map, Hash.word(i32.toU64(key)), key * 3)
      |> Effect.provideMut(&mut allocator)
    drop previous
    key = key + 1
  }
  if length<Word, i32>(&map) != 40 { return 1 }
  if bucketCount<Word, i32>(&map) <= 40 { return 2 }
  let mut probe = 0
  let mut total = 0
  while probe < 40 {
    let found = Option.unwrapOr<i32>(get<Word, i32>(&map, Hash.word(i32.toU64(probe))), -1)
    if found != probe * 3 { return 3 }
    total = total + found
    probe = probe + 1
  }
  if total != 2340 { return 4 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from VectorAcceptance.test.ts: growth past the initial capacity with boundary reads.
  {
    name: 'vector-growth-reads',
    source: `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.vector { Vector, make, append, get, length, capacity }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = make<i32>()
  let pending0 = append<i32>(&mut values, 10) |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let pending1 = append<i32>(&mut values, 11) |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let pending2 = append<i32>(&mut values, 12) |> Effect.provideMut(&mut allocator)
  let appended2 = run pending2
  let pending3 = append<i32>(&mut values, 13) |> Effect.provideMut(&mut allocator)
  let appended3 = run pending3
  let pending4 = append<i32>(&mut values, 14) |> Effect.provideMut(&mut allocator)
  let appended4 = run pending4
  let pending5 = append<i32>(&mut values, 15) |> Effect.provideMut(&mut allocator)
  let appended5 = run pending5
  if length<i32>(&values) == 6 {} else { return 0 }
  if capacity<i32>(&values) == 8 {} else { return 1 }
  let first = get<i32>(&values, 0)
  let last = get<i32>(&values, 5)
  return first + last + 17
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from OwnedAllocationDispatch.test.ts: quota refusal propagates typed OutOfMemoryError.
  {
    name: 'owned-allocation-quota-refusal',
    source: `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
struct QuotaAllocator { remaining: i32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == 0 { fail OutOfMemoryError {} }
  self.remaining = self.remaining - 1
  let mut inner = Allocator.systemAllocatorProvider()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
  let block = run recipe
  return move block
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = QuotaAllocator { remaining: 1 }
  let first = Layout.of<[i32; 2]>()
  let recipeA = Allocator.allocate(move first) |> Effect.provideMut(&mut allocator)
  let a = run recipeA
  let second = Layout.of<[i32; 2]>()
  let recipeB = Allocator.allocate(move second) |> Effect.provideMut(&mut allocator)
  let b = run recipeB
  drop a
  drop b
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 {
  return run Effect.catchAll(build(), recover)
}`,
    expected: { _tag: 'Completes', result: 7 },
  },
  // folded from OwnedAllocationAcceptance.test.ts: caller-funded shared-core initialization.
  {
    name: 'local-shared-control-block-allocation',
    source: `import silk.effect as Effect

effect fn construct() -> i32 ! Intrinsic.StorageFailure {
  let layout = Intrinsic.sharedLayout<i32>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    drop core
  }
  return 42
}

effect fn recover(error: Intrinsic.StorageFailure) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'local-shared-lifecycle-operations',
    source: `import silk.effect as Effect

fn selected(value: &mut i32) -> i32 { return 21 }
fn conflict() -> i32 { return 0 }

effect fn construct() -> i32 ! Intrinsic.StorageFailure {
  let layout = Intrinsic.sharedLayout<i32>()
  let allocation = run Intrinsic.systemAllocationAcquire(move layout)
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    let clone = Intrinsic.sharedClone<i32>(&core)
    let first = Intrinsic.sharedWithMut<i32, i32>(&core, selected, conflict)
    let second = Intrinsic.sharedWithMut<i32, i32>(&clone, selected, conflict)
    drop clone
    drop core
    return first + second
  }
}

effect fn recover(error: Intrinsic.StorageFailure) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'local-shared-standard-library-wrapper',
    source: `import silk.allocator { Allocator }
import silk.allocator { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effect as Effect
import silk.shared as Shared

struct Counter { value: i32 }

fn increment(value: &mut Counter) -> i32 {
  value.value = value.value + 1
  return value.value
}

fn read(value: &Counter) -> i32 { return value.value }

effect fn construct() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let first = run (Shared.make<Counter>(Counter { value: 41 })
    |> Effect.provideMut<Allocator>(&mut allocator))
  let second = Shared.clone<Counter>(&first)
  let updated = Shared.withMut<Counter, i32>(&second, increment)
  let answer = Shared.with<Counter, i32>(&first, read)
  drop second
  drop first
  return answer
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'local-shared-recursive-cleanup',
    source: `import silk.effect as Effect

struct Empty {}
struct Node { next: Intrinsic.SharedCore<Node> | Empty }

effect fn construct() -> i32 ! Intrinsic.StorageFailure {
  let firstAllocation = run Intrinsic.systemAllocationAcquire(Intrinsic.sharedLayout<Node>())
  let secondAllocation = run Intrinsic.systemAllocationAcquire(Intrinsic.sharedLayout<Node>())
  let thirdAllocation = run Intrinsic.systemAllocationAcquire(Intrinsic.sharedLayout<Node>())
  unsafe {
    let third = Intrinsic.sharedFromAllocation<Node>(
      move thirdAllocation,
      Node { next: Empty {} },
    )
    let second = Intrinsic.sharedFromAllocation<Node>(
      move secondAllocation,
      Node { next: move third },
    )
    let first = Intrinsic.sharedFromAllocation<Node>(
      move firstAllocation,
      Node { next: move second },
    )
    drop first
    return 42
  }
}

effect fn recover(error: Intrinsic.StorageFailure) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from SlotLaneWidth.test.ts: u8 lane writes, copies, and takes through a raw buffer.
  {
    name: 'slot-lane-u8',
    source: `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.raw_buffer as RawBuffer
import silk.slot as Slot
import silk.u8 as u8
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[u8; 4]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<u8>(move allocation, 4)
    let firstWritten = Slot.write<u8>(RawBuffer.slot(&mut buffer, 0), 7)
    let secondWritten = Slot.write<u8>(RawBuffer.slot(&mut buffer, 3), 11)
    let firstCopy = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    let secondCopy = Slot.copy(RawBuffer.slot(&mut buffer, 3))
    let firstTake = Slot.take(RawBuffer.slot(&mut buffer, 0))
    let secondTake = Slot.take(RawBuffer.slot(&mut buffer, 3))
    drop buffer
    return 100 + u8.toI32(firstCopy) + u8.toI32(secondCopy) + u8.toI32(firstTake) + u8.toI32(secondTake)
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
    expected: { _tag: 'Completes', result: 136 },
  },
  // folded from SlotLaneWidth.test.ts: f64 lane parity including a negative value.
  {
    name: 'slot-lane-f64',
    source: `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.f64 as f64
import silk.layout { Layout }
import silk.raw_buffer as RawBuffer
import silk.slot as Slot
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[f64; 4]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<f64>(move allocation, 4)
    let firstWritten = Slot.write<f64>(RawBuffer.slot(&mut buffer, 0), -7.0)
    let secondWritten = Slot.write<f64>(RawBuffer.slot(&mut buffer, 1), 11.0)
    let firstCopy = Slot.copy(RawBuffer.slot(&mut buffer, 0))
    let secondCopy = Slot.copy(RawBuffer.slot(&mut buffer, 1))
    let firstTake = Slot.take(RawBuffer.slot(&mut buffer, 0))
    let secondTake = Slot.take(RawBuffer.slot(&mut buffer, 1))
    drop buffer
    return 100 + f64.toI32(firstCopy) + f64.toI32(secondCopy) + f64.toI32(firstTake) + f64.toI32(secondTake)
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
    expected: { _tag: 'Completes', result: 108 },
  },
  // folded from BytesAcceptance.test.ts: copy, append, and mutate through byte slices (exit 180).
  {
    name: 'bytes-parity',
    source: `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.u8 as u8
import silk.usize as usize
import silk.bytes { Bytes, copy, append, asMutSlice, asSlice, length }

fn octet(value: u8) -> u8 { return value }

fn checksum(values: &[u8]) -> i32 {
  let mut index = usize.add(0, 0)
  let mut total = 0
  while index < values.length {
    total = total + u8.toI32(values[index])
    index = index + usize.add(0, 1)
  }
  return total
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let source = [octet(0), octet(255), octet(128), octet(1)]
  let copying = copy(&source) |> Effect.provideMut(&mut allocator)
  let mut bytes = run copying
  let suffix = [octet(42), octet(7)]
  let appending = append(&mut bytes, &suffix) |> Effect.provideMut(&mut allocator)
  let appended = run appending
  let mut writable = asMutSlice(&mut bytes)
  writable[1] = octet(2)
  let readable = asSlice(&bytes)
  if length(&bytes) == 6 {} else { return 1 }
  return checksum(move readable)
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 180 },
  },
  // folded from StaticByteViewIndexing.test.ts: out-of-bounds static byte read traps.
  {
    name: 'static-byte-view-bounds',
    source: `import silk.u8 as u8
import silk.usize as usize
pub fn main() -> i32 {
  let bytes = b"\\x99\\x13\\x1d\\x00"
  let index = usize.add(0, 4)
  return u8.toI32(bytes[index])
}`,
    expected: { _tag: 'Trap' },
  },
  // folded from OwnedAllocationAcceptance.test.ts: guarded slot writes and takes release cleanly.
  {
    name: 'owned-allocation-guard',
    source: `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.raw_buffer as RawBuffer
import silk.slot as Slot
struct Element { value: i32 }

effect fn build(count: usize) -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[Element; 4]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<Element>(move allocation, 4)
    let head0 = Element { value: 11 }
    let tail0 = Element { value: 31 }
    let first = Slot.write(RawBuffer.slot(&mut buffer, 0), move head0)
    let second = Slot.write(RawBuffer.slot(&mut buffer, 1), move tail0)
    let head = Slot.take(RawBuffer.slot(&mut buffer, 0))
    let tail = Slot.take(RawBuffer.slot(&mut buffer, 1))
    drop buffer
    return head.value + tail.value
  }
  return 0
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 {
  return run Effect.catchAll(build(4), recover)
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from RuntimeSliceAcceptance.test.ts: exclusive slice writes reach the caller.
  {
    name: 'runtime-slice-exclusive',
    source: `import silk.usize as usize
struct Token {
  value: i32
}

fn replace(values: &mut [Token], index: usize) -> i32 {
  values[index] = Token {value: 42}
  return usize.toI32(values.length)
}

pub fn main() -> i32 {
  let mut values = [Token {value: 1}, Token {value: 2}]
  let length = replace(&mut values, 0)
  if length != 2 {
    return 0
  }
  return values[0].value
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from EffectRuntime.test.ts: Effect.retry gives every attempt fresh locals while the
  // exclusive capture persists, and the third attempt's count is the exit (3, not 42).
  {
    name: 'effect-retry-captures',
    source: `import silk.effect as Effect
struct Problem { code: i32 }
effect fn retrying() -> i32 ! Problem {
  let mut counter = 0
  let work = effect {
    counter = counter + 1
    if counter < 3 { fail Problem { code: counter } }
    return counter
  }
  let retried = move work |> Effect.retry(2)
  return run retried
}
effect fn recover(problem: Problem) -> i32 { return 99 }
pub fn main() -> i32 {
  let handled = retrying() |> Effect.catchAll(recover)
  return run handled
}`,
    expected: { _tag: 'Completes', result: 3 },
  },
  // folded from EffectSuspensionComposition.test.ts: a suspended source inside Effect.retry fails
  // every attempt, and the recovery answers with the failure exit (7).
  {
    name: 'suspension-retry-failure',
    source: `import silk.effect as Effect
struct Problem { code: i32 }
effect fn attempt() -> i32 ! Problem {
  let observed = run Effect.suspend(effect { return 1 })
  fail Problem { code: observed }
}
effect fn recover(error: Problem) -> i32 { return 7 }
pub fn main() -> i32 {
  return run Effect.catchAll(
    attempt() |> Effect.retry(2),
    recover
  )
}`,
    expected: { _tag: 'Completes', result: 7 },
  },
  {
    name: 'suspension-repeated-states',
    source: `import silk.effect as Effect
effect fn twice() -> i32 {
  let left = run Effect.suspend(effect { return 40 })
  let right = run Effect.suspend(effect { return 2 })
  return left + right
}
pub fn main() -> i32 { return run twice() }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'stored-catch-suspension',
    source: storedCatchSuspension,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'owned-allocator-suspension-success',
    source: ownedAllocatorSuspensionSuccess,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'owned-allocator-suspension-failure',
    source: ownedAllocatorSuspensionFailure,
    expected: { _tag: 'Completes', result: 7 },
  },
  {
    name: 'audit-allocator-suspension',
    source: auditAllocatorSuspension,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from StoredCallableRuntime.test.ts: an uncalled stored callable owning a Drop guard is
  // cleaned exactly once when a typed failure exits the frame.
  {
    name: 'stored-callable-cleanup-typed-failure',
    source: `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
struct Guard {
  tag: i32
  storage: Allocation
}
impl Drop for Guard {
  fn drop(self: &mut Guard) -> () {
    return ()
  }
}
struct Holder<F: once fn(i32) -> i32> { step: F }
fn consume(value: i32, guard: Guard) -> i32 { return value + guard.tag }
fn keep<F: once fn(i32) -> i32>(holder: Holder<F>) -> i32 { return 42 }
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let guard = Guard { tag: 2, storage: move allocation }
  let holder = Holder { step: consume(move guard) }
  fail OutOfMemoryError {}
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from DropHookExecution.test.ts: a guard live at a failing run releases through its hook
  // before the typed failure propagates to the recovery (exit 7).
  {
    name: 'drop-hook-failure-propagation',
    source: `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
struct Guard {
  tag: i32
  storage: Allocation
}

impl Drop for Guard {
  fn drop(self: &mut Guard) -> () { return () }
}

struct ExhaustedAllocator { tag: i32 }

effect fn allocate(self: &mut ExhaustedAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  fail OutOfMemoryError {}
}

impl Allocator for ExhaustedAllocator { allocate: ExhaustedAllocator.allocate }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut empty = ExhaustedAllocator { tag: 0 }
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let guard = Guard { tag: 5, storage: move allocation }
  let second = Layout.of<[i32; 2]>()
  let refused = Allocator.allocate(move second) |> Effect.provideMut(&mut empty)
  let never = run refused
  drop never
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 7 },
  },
  // folded from OpaqueRepresentationEngines.test.ts: opaque callable returns keep their hidden
  // concrete identity, so two captures of the same shape stay distinct.
  {
    name: 'opaque-callable',
    source: `fn add(left: i32, right: i32) -> i32 { return left + right }
fn make(value: i32) -> some<F: fn(i32) -> i32> F { return add(value) }
pub fn main() -> i32 {
  let first = make(40)
  let second = make(1)
  return first(1) + second(0)
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // Finite executable representations remain static even when an ordinary union carries them.
  {
    name: 'ordinary-union-executable-members',
    source: `fn add(left: i32, right: i32) -> i32 { return left + right }
fn selectedCallable() -> typeof(add) | i32 { return add }
fn selectedEffect() -> some<F: Effect<i32>> F | i32 {
  return effect { return 42 }
}
pub fn main() -> i32 {
  drop selectedCallable()
  drop selectedEffect()
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // A non-nominal active payload still owns and releases every droppable element exactly once.
  {
    name: 'ordinary-union-droppable-array',
    source: `struct Token { value: i32 }
impl Drop for Token { fn drop(self: &mut Token) -> () { return () } }
fn accept(value: i32 | [Token; 2]) -> i32 { drop value return 42 }
pub fn main() -> i32 {
  return accept([Token { value: 1 }, Token { value: 2 }])
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // One program covers success bypass, selected recovery, and residual propagation. Keeping the
  // complete branch matrix in the shared corpus proves native execution without a feature-local
  // compile/link test.
  {
    name: 'effect-selective-catch',
    source: `import silk.effect as Effect
struct Selected { code: i32 }
struct Residual { code: i32 }
effect fn risky(mode: i32) -> i32 ! Selected | Residual {
  if mode == 0 { return 10 }
  if mode == 1 { fail Selected { code: 10 } }
  fail Residual { code: 16 }
}
effect fn recoverSelected(problem: Selected) -> i32 { return problem.code + 4 }
effect fn recoverResidual(problem: Residual) -> i32 { return problem.code + 2 }
effect fn selective(mode: i32) -> i32 ! Residual {
  return run Effect.catch<Selected>(risky(mode), recoverSelected)
}
effect fn completed(mode: i32) -> i32 {
  return run Effect.catchAll(selective(mode), recoverResidual)
}
pub fn main() -> i32 {
  return (run completed(0)) + (run completed(1)) + (run completed(2))
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'effect-selective-catch-direct-stored',
    source: `import silk.effect as Effect
struct Selected { code: i32 }
struct Residual { code: i32 }
effect fn risky() -> i32 ! Selected | Residual { fail Selected { code: 10 } }
effect fn recoverSelected(problem: Selected) -> i32 { return problem.code + 1 }
effect fn recoverResidual(problem: Residual) -> i32 { return problem.code + 2 }
pub fn main() -> i32 {
  let selected = Intrinsic.catchFailure<Selected>(risky(), recoverSelected)
  return run Effect.catchAll(move selected, recoverResidual)
}`,
    expected: { _tag: 'Completes', result: 11 },
  },
  {
    name: 'effect-heterogeneous-failure-payload',
    source: heterogeneousFailurePayload,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'effect-heterogeneous-owned-failure-payload',
    source: heterogeneousOwnedFailurePayload,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'effect-heterogeneous-owned-failure-result-drop',
    source: heterogeneousOwnedFailureResultDrop,
    expected: { _tag: 'Completes', result: 42 },
  },
  // Affine owned service providers remain in the parent frame while a pre-read scalar suspends,
  // then release exactly once after either success or typed failure.
  {
    name: 'owned-provider-suspended-success',
    source: ownedProviderSuspendedSuccess,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'owned-provider-suspended-failure',
    source: ownedProviderSuspendedFailure,
    expected: { _tag: 'Completes', result: 7 },
  },
  // One service call site is specialized with both a synchronous and a suspendable provider. The
  // outer allocator binding must relay through the intermediate wrappers without contaminating
  // either provider-specific execution classification.
  {
    name: 'mixed-service-provider-suspension',
    source: mixedServiceProviderSuspension,
    expected: { _tag: 'Completes', result: 42 },
  },
  // Two suspendable specializations of the same wrapper differ only in contractRow (which
  // provider satisfies the requirement). Each carries its own coroutine frame entry, so frame
  // lookups must key on the full suspension identity including contractRow — a name-and-type-
  // arguments match selects the wrong specialization's frame.
  {
    name: 'contract-row-suspension-frames',
    source: `import silk.effect as Effect
service Value {
  effect fn read() -> i32 ? &mut Value
}
struct DelayedA { value: i32 }
effect fn readA(self: &mut DelayedA) -> i32 {
  let value = self.value
  return run Effect.suspend(effect { return value })
}
impl Value for DelayedA { read: DelayedA.readA }
struct DelayedB { value: i32 }
effect fn readB(self: &mut DelayedB) -> i32 {
  let value = self.value + 1
  return run Effect.suspend(effect { return value })
}
impl Value for DelayedB { read: DelayedB.readB }
effect fn use() -> i32 ? &mut Value {
  let base = 1
  let got = run Value.read()
  return got + base
}
effect fn first() -> i32 {
  let mut provider = DelayedA { value: 19 }
  return run Intrinsic.bindRequirementMut<Value>(use(), &mut provider)
}
effect fn second() -> i32 {
  let mut provider = DelayedB { value: 20 }
  return run Intrinsic.bindRequirementMut<Value>(use(), &mut provider)
}
effect fn program() -> i32 {
  return (run first()) + (run second())
}
pub fn main() -> i32 {
  return run program()
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // The float math conformance programs join the corpus so the native differential compiles and
  // runs each one, which is the third engine behind the evaluator and direct WebAssembly.
  ...floatMathPrograms.map((program) => ({
    name: program.name,
    source: program.source,
    expected: { _tag: 'Completes', result: 42 } as const,
  })),
]

/**
 * Native-only extensions to the shared evaluator corpus. These programs retain the optimized
 * single compile/link loop without making the evaluator's pinned-outcome gate repeat large
 * feature fixtures that already have focused evaluator coverage.
 */
const localSharedPressure = readFileSync(
  new URL('../../../../examples/language-pressure/local-shared-slp1/main.silk', import.meta.url),
  'utf8',
)
const renamedLocalSharedPressure = readFileSync(
  new URL(
    '../../../../examples/language-pressure/local-shared-slp1/renamed-main.silk',
    import.meta.url,
  ),
  'utf8',
)
const independentExecutionPressure = (name: string): string =>
  readFileSync(
    new URL(`../fixtures/independent-execution-separation/${name}.silk`, import.meta.url),
    'utf8',
  )
const schedulerFiber = (name: string): string =>
  readFileSync(new URL(`../fixtures/scheduler-fiber/${name}.silk`, import.meta.url), 'utf8')
const localSharedPressureFailure = (ordinal: 0 | 1): string =>
  localSharedPressure.replace(
    ordinal === 0
      ? 'let mut firstAllocator = Allocator.systemAllocatorProvider()'
      : 'let mut secondAllocator = Allocator.systemAllocatorProvider()',
    ordinal === 0
      ? 'let mut firstAllocator = ExhaustedAllocator {}'
      : 'let mut secondAllocator = ExhaustedAllocator {}',
  )

export const nativeCorpus: ReadonlyArray<CorpusProgram> = [
  ...corpus,
  // These two canonical programs cover the public single-threaded Fiber story through the shared
  // evaluator/native differential: root/fork/join, FIFO siblings, repeated yield, nested forks,
  // completion-before-join, typed child failure, structured cancellation, and reuse of one
  // LocalScheduler value.
  {
    name: 'scheduler-fiber-semantics',
    source: schedulerFiber('local-scheduler-semantics'),
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'scheduler-fiber-shutdown',
    source: schedulerFiber('local-scheduler-shutdown'),
    expected: { _tag: 'Completes', result: 42 },
  },
  ...[
    { name: 'first-activation', result: 21 },
    { name: 'coroutine', result: 123 },
    { name: 'dormant-cancel', result: 1111 },
    { name: 'selective-ready', result: 22 },
    { name: 'timer', result: 42 },
  ].map(
    (program): CorpusProgram => ({
      name: `independent-execution-separation-${program.name}`,
      source: independentExecutionPressure(program.name),
      expected: { _tag: 'Completes', result: program.result },
    }),
  ),
  ...[
    { name: 'timer', source: 'timer', result: 42 },
    { name: 'coroutine', source: 'coroutine', result: 123 },
    { name: 'selective-ready', source: 'selective-ready', result: 22 },
  ].map(
    (program): CorpusProgram => ({
      name: `independent-execution-separation-renamed-${program.name}`,
      source: renameIndependentPolicy(independentExecutionPressure(program.source)),
      expected: { _tag: 'Completes', result: program.result },
    }),
  ),
  {
    name: 'independent-execution-non-lifo',
    source: independentExecutionNonLifo,
    expected: { _tag: 'Completes', result: 240 },
  },
  {
    name: 'independent-execution-illegal-dormant-drive',
    source: independentExecutionIllegalDormantDrive,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'independent-execution-illegal-notifying-drive',
    source: independentExecutionIllegalNotifyingDrive,
    expected: { _tag: 'Trap' },
  },
  {
    name: 'independent-execution-stack-exhaustion',
    source: independentExecutionStackExhaustion,
    nativeEnvironment: { SILK_PRIVATE_EXECUTION_STACK_LIMIT_BYTES: '1' },
    expected: { _tag: 'Trap' },
  },
  {
    name: 'independent-execution-multiple-packages',
    source: independentExecutionMultiplePackages,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'independent-execution-late-cancelled-wake',
    source: independentExecutionLateCancelledWake,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'independent-execution-reentrant-destroy',
    source: independentExecutionReentrantDestroy,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'independent-execution-local-reactor',
    source: independentExecutionLocalReactor,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'independent-execution-repeated-generations',
    source: independentExecutionRepeatedGenerations,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'independent-execution-eligible-drop',
    source: independentExecutionEligibleDrop,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'independent-execution-parked-typed-failure',
    source: independentExecutionParkedTypedFailure,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'local-shared-pressure-success',
    source: localSharedPressure,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'local-shared-pressure-renamed',
    source: renamedLocalSharedPressure,
    expected: { _tag: 'Completes', result: 42 },
  },
  ...([0, 1] as const).map(
    (ordinal): CorpusProgram => ({
      name: `local-shared-pressure-quota-${ordinal}`,
      source: localSharedPressureFailure(ordinal),
      expected: { _tag: 'Completes', result: 142 },
    }),
  ),
  ...(['with', 'withMut'] as const).flatMap(
    (outer): ReadonlyArray<CorpusProgram> =>
      (['with', 'withMut'] as const).map((inner): CorpusProgram => {
        const outerReference = outer === 'with' ? '&Counter' : '&mut Counter'
        const innerCallback = inner === 'with' ? 'read' : 'increment'
        return {
          name: `local-shared-conflict-${outer}-${inner}`,
          source: `import silk.allocator { Allocator }
import silk.allocator { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effect as Effect
import silk.shared as Shared
struct Counter { value: i32 }
fn read(value: &Counter) -> i32 { return value.value }
fn increment(value: &mut Counter) -> i32 {
  value.value = value.value + 1
  return value.value
}
fn nested(value: ${outerReference}, alias: Shared.Shared<Counter>) -> i32 {
  return Shared.${inner}<Counter, i32>(&alias, ${innerCallback})
}
effect fn conflictCase() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let first = run (Shared.make<Counter>(Counter { value: 41 })
    |> Effect.provideMut<Allocator>(&mut allocator))
  let alias = Shared.clone<Counter>(&first)
  return Shared.${outer}<Counter, i32>(&first, nested(move alias))
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(conflictCase(), recover) }`,
          expected: { _tag: 'Trap' },
        }
      }),
  ),
  {
    name: 'local-shared-affine-movement',
    source: `import silk.allocator { Allocator }
import silk.allocator { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.shared as Shared
struct Empty {}
struct Token { storage: Allocation }
struct Mailbox { state: Empty | Token }
fn take(self: &mut Mailbox) -> Empty | Token {
  return Intrinsic.replace(self.state, Empty {})
}
fn consume(value: Empty | Token) -> i32 {
  return match move value {
    Empty {} => 0
    Token { storage } => release(move storage)
  }
}
fn release(storage: Allocation) -> i32 { drop storage return 42 }
effect fn useCell() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let storage = run (Allocator.allocate(Layout.of<i32>())
    |> Effect.provideMut<Allocator>(&mut allocator))
  let mailbox = run (Shared.make<Mailbox>(Mailbox {
    state: Token { storage: move storage }
  }) |> Effect.provideMut<Allocator>(&mut allocator))
  let token = Shared.withMut<Mailbox, Empty | Token>(&mailbox, take)
  drop mailbox
  return consume(move token)
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(useCell(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'local-shared-two-frame-failure',
    source: `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
struct Problem {}
effect fn failInner(core: Intrinsic.SharedCore<i32>) -> i32 ! Problem {
  let inner = Intrinsic.sharedClone<i32>(&core)
  fail Problem {}
}
effect fn construct() -> i32 ! Problem | Intrinsic.StorageFailure {
  let allocation = run Intrinsic.systemAllocationAcquire(Intrinsic.sharedLayout<i32>())
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    let transferred = Intrinsic.sharedClone<i32>(&core)
    return run failInner(move transferred)
  }
}
effect fn recover(error: Problem | Intrinsic.StorageFailure) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'local-shared-construction-exhaustion',
    source: `import silk.allocator { Allocator }
import silk.allocator { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effect as Effect
import silk.layout { Layout }
import silk.shared as Shared
struct Token { storage: Allocation }
struct Exhausted {}
effect fn reject(self: &mut Exhausted, layout: Layout) -> Allocation ! OutOfMemoryError {
  fail OutOfMemoryError {}
}
impl Allocator for Exhausted { allocate: Exhausted.reject }
effect fn construct() -> i32 ! OutOfMemoryError {
  let mut system = Allocator.systemAllocatorProvider()
  let payload = run (Allocator.allocate(Layout.of<i32>())
    |> Effect.provideMut<Allocator>(&mut system))
  let token = Token { storage: move payload }
  let mut exhausted = Exhausted {}
  let shared = run (Shared.make<Token>(move token)
    |> Effect.provideMut<Allocator>(&mut exhausted))
  drop shared
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 42 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'local-shared-clone-drop-during-access',
    source: `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
fn selected(value: &mut i32) -> i32 { return 0 }
fn conflict() -> i32 { return 21 }
fn unused(value: &mut i32, captured: Intrinsic.SharedCore<i32>) -> i32 {
  drop captured
  return 0
}
fn nested(value: &mut i32, core: Intrinsic.SharedCore<i32>) -> i32 {
  let cleanupCore = Intrinsic.sharedClone<i32>(&core)
  return Intrinsic.sharedWithMut<i32, i32>(&core, unused(move cleanupCore), conflict) + 21
}
effect fn construct() -> i32 ! Intrinsic.StorageFailure {
  let allocation = run Intrinsic.systemAllocationAcquire(Intrinsic.sharedLayout<i32>())
  unsafe {
    let core = Intrinsic.sharedFromAllocation<i32>(move allocation, 42)
    let nestedCore = Intrinsic.sharedClone<i32>(&core)
    let result = Intrinsic.sharedWithMut<i32, i32>(&core, nested(move nestedCore), conflict)
    drop core
    return result
  }
}
effect fn recover(error: Intrinsic.StorageFailure) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'local-shared-strong-cycle',
    source: `import silk.allocator { OutOfMemoryError }
import silk.effect as Effect
struct Empty {}
struct Bomb {}
impl Drop for Bomb {
  fn drop(self: &mut Bomb) -> () { let boom = 1 / 0 return () }
}
struct Node { bomb: Bomb next: Intrinsic.SharedCore<Node> | Empty }
fn link(value: &mut Node, next: Intrinsic.SharedCore<Node>) -> i32 {
  value.next = move next
  return 0
}
fn conflict() -> i32 { return 0 }
effect fn construct() -> i32 ! Intrinsic.StorageFailure {
  let firstAllocation = run Intrinsic.systemAllocationAcquire(Intrinsic.sharedLayout<Node>())
  let secondAllocation = run Intrinsic.systemAllocationAcquire(Intrinsic.sharedLayout<Node>())
  unsafe {
    let first = Intrinsic.sharedFromAllocation<Node>(
      move firstAllocation,
      Node { bomb: Bomb {}, next: Empty {} },
    )
    let second = Intrinsic.sharedFromAllocation<Node>(
      move secondAllocation,
      Node { bomb: Bomb {}, next: Empty {} },
    )
    let secondEdge = Intrinsic.sharedClone<Node>(&second)
    let firstLink = Intrinsic.sharedWithMut<Node, i32>(&first, link(move secondEdge), conflict)
    let firstEdge = Intrinsic.sharedClone<Node>(&first)
    let secondLink = Intrinsic.sharedWithMut<Node, i32>(&second, link(move firstEdge), conflict)
    if firstLink == 999 { let bomb = Bomb {} drop bomb }
    drop first
    drop second
    return firstLink + secondLink + 42
  }
}
effect fn recover(error: Intrinsic.StorageFailure) -> i32 { return 0 }
pub fn main() -> i32 { return run Effect.catchAll(construct(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // Static-composition runtime parity belongs in the shared native differential rather than a
  // feature-local compile/link loop. Trapping Drop variants causally prove the three cleanup exits.
  ...staticCompositionCorpus,
]

/** Invalid generic programs that must stop before target layout and MIR. */
export const invalidGenericCorpus: ReadonlyArray<InvalidCorpusProgram> = [
  {
    name: 'generic-explicit-arity',
    source:
      'fn identity<T>(value: T) -> T { return move value }\npub fn main() -> i32 { return identity<i32, bool>(42) }',
    codes: ['SEM0051'],
  },
  {
    name: 'generic-explicit-arity-past-prefix',
    source:
      'fn pair<A, B>(left: A, right: B) -> A { return move left }\npub fn main() -> i32 { return pair<i32, bool, u8>(1, true) }',
    codes: ['SEM0051'],
  },
  {
    name: 'generic-uninferred-prefix-remainder',
    source:
      'fn phantom<A, B>(value: A) -> A { return move value }\npub fn main() -> i32 { return phantom<i32>(1) }',
    codes: ['SEM0099'],
  },
  {
    name: 'generic-contradicted-prefix',
    source:
      'fn pair<A, B>(left: A, right: B) -> A { return move left }\npub fn main() -> i32 { return pair<bool>(1, true) }',
    codes: ['SEM0100'],
  },
  {
    name: 'generic-conflicting-inference',
    source:
      'fn same<T>(left: T, right: T) -> T { return move left }\npub fn main() -> i32 { return same(1, true) }',
    codes: ['SEM0052'],
  },
  {
    name: 'generic-polymorphic-recursion',
    source: `fn expand<T>(value: T) -> i32 { return expand<[T; 1]>([move value]) }
pub fn main() -> i32 { return expand<i32>(1) }`,
    codes: ['SEM0053'],
  },
]

/** Phase-owned invalid matching programs shared by diagnostics and release-gate tests. */
export const invalidMatchCorpus: ReadonlyArray<InvalidCorpusProgram> = [
  {
    name: 'match-incomplete',
    source: `struct Left {}
struct Right {}
fn inspect(input: Left | Right) -> i32 { return match &input { Left {} => 1 } }
pub fn main() -> i32 { return 0 }`,
    codes: ['SEM0044'],
  },
  {
    name: 'match-unreachable',
    source: `struct Token {}
fn inspect(input: Token) -> i32 { return match &input { _ => 0 Token {} => 1 } }
pub fn main() -> i32 { return 0 }`,
    codes: ['SEM0043'],
  },
  {
    name: 'match-invalid-member-and-field',
    source: `struct Token { value: i32 }
struct Other {}
fn inspect(input: Token) -> i32 { return match &input { Other {} => 0 Token { value, missing } => value } }
pub fn main() -> i32 { return 0 }`,
    codes: ['SEM0042', 'SEM0022'],
  },
  {
    name: 'match-invalid-guard-and-join',
    source: `struct Left {}
struct Right {}
fn inspect(input: Left | Right) -> i32 { return match &input { Left {} if 1 => 0 Left {} => 0 Right {} => false } }
pub fn main() -> i32 { return 0 }`,
    codes: ['SEM0045'],
  },
  {
    name: 'match-guard-consumes',
    source: `struct Payload {}
struct Box { value: Payload }
fn accept(value: Payload) -> bool { return true }
fn inspect(input: Box) -> i32 { return match move input { Box { value } if accept(move value) => 1 Box { value: fallback } => 0 } }
pub fn main() -> i32 { return 0 }`,
    codes: ['OWN0008'],
  },
  {
    name: 'match-borrow-escape-exclusive-immutable',
    source: `struct Payload {}
struct Box { value: Payload }
fn escape(input: Box) -> Payload { return match &input { Box { value } => value } }
fn exclusive(input: Box) -> i32 { return match &mut input { Box { .. } => 0 } }
pub fn main() -> i32 { return 0 }`,
    codes: ['OWN0006', 'OWN0007'],
  },
  {
    name: 'match-malformed-pattern',
    source: `struct Token { value: i32 }
fn inspect(input: Token) -> i32 { return match &input { Token { value: } 0 } }
pub fn main() -> i32 { return 0 }`,
    codes: ['PAR0001'],
  },
]
