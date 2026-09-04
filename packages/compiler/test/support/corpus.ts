/**
 * The shared native acceptance corpus: programs with independently pinned process outcomes.
 */
import { readFileSync } from 'node:fs'
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
import { recoveredProvidedWrite, recoveredWriterModule } from './recoveredProvidedWrite.js'
import { storedCatchSuspension } from './storedCatchSuspension.js'

// Folded from Transcendental.test.ts: every runtime bit pattern is committed independently of the
// compiler implementation. `referenceBits` records the high-precision oracle; `acceptedBits`
// records an explicitly reviewed result within the four-ulp language tolerance when they differ.
interface TranscendentalVector {
  readonly width: 32 | 64
  readonly inputBits: string
  readonly operation: 'Sin' | 'Cos'
  readonly referenceBits: string
  readonly acceptedBits?: string
}

const transcendentalFixture = JSON.parse(
  readFileSync(new URL('../fixtures/transcendental-vectors.json', import.meta.url), 'utf8'),
) as { readonly vectors: ReadonlyArray<TranscendentalVector> }

const transcendentalVectors: ReadonlyArray<TranscendentalVector> = [
  ...transcendentalFixture.vectors,
  { width: 32, inputBits: '0x00000000', operation: 'Sin', referenceBits: '0x00000000' },
  { width: 32, inputBits: '0x80000000', operation: 'Sin', referenceBits: '0x80000000' },
  { width: 32, inputBits: '0x00000000', operation: 'Cos', referenceBits: '0x3f800000' },
  { width: 32, inputBits: '0x7f800000', operation: 'Sin', referenceBits: '0x7fc00000' },
  { width: 32, inputBits: '0xff800000', operation: 'Cos', referenceBits: '0x7fc00000' },
  { width: 32, inputBits: '0x7fc12345', operation: 'Sin', referenceBits: '0x7fc00000' },
  {
    width: 64,
    inputBits: '0x0000000000000000',
    operation: 'Sin',
    referenceBits: '0x0000000000000000',
  },
  {
    width: 64,
    inputBits: '0x8000000000000000',
    operation: 'Sin',
    referenceBits: '0x8000000000000000',
  },
  {
    width: 64,
    inputBits: '0x0000000000000000',
    operation: 'Cos',
    referenceBits: '0x3ff0000000000000',
  },
  {
    width: 64,
    inputBits: '0x7ff0000000000000',
    operation: 'Sin',
    referenceBits: '0x7ff8000000000000',
  },
  {
    width: 64,
    inputBits: '0xfff0000000000000',
    operation: 'Cos',
    referenceBits: '0x7ff8000000000000',
  },
  {
    width: 64,
    inputBits: '0x7ff8123456789abc',
    operation: 'Sin',
    referenceBits: '0x7ff8000000000000',
  },
]

/** Canonical-bits transcendental program with independently committed native expectations. */
export const transcendentalCanonicalBits = `import silk.f32 as f32
import silk.f64 as f64
pub fn main() -> i32 {
${transcendentalVectors
  .map((vector, index) => {
    const inputBits = BigInt(vector.inputBits)
    const expectedBits = BigInt(vector.acceptedBits ?? vector.referenceBits)
    return `  if f${vector.width}.toBits(f${vector.width}.${vector.operation.toLowerCase()}(f${vector.width}.fromBits(${inputBits.toString()}))) != ${expectedBits.toString()} { return ${index + 1} }`
  })
  .join('\n')}
  return 42
}`

export interface CorpusProgram {
  readonly name: string
  readonly source: string
  readonly nativeSource?: string
  readonly nativeImports?: Readonly<Record<string, string>>
  readonly nativeEnvironment?: Readonly<Record<string, string>>
  /** C translation units compiled with `compileCObject` and linked as structured object inputs. */
  readonly nativeCSources?: Readonly<Record<string, string>>
  readonly nativeDynamicLibraries?: ReadonlyArray<string>
  readonly nativeStdout?: string
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

/** A generic Display call selects the interface-owned inline i32 witness on every engine. */
export const scalarDisplayAcceptance = `import silk.effect { Effect }
import silk.format { Format }
import silk.u8 as u8
import silk.usize as usize
import silk.writer { Writer, WriterError }

struct Capture { index: usize valid: bool }

effect fn writeAll(self: &mut Capture, bytes: &[u8]) -> () {
  let mut offset = usize.ZERO
  while offset < bytes.length {
    let mut expected = u8.toU8(50)
    if self.index == usize.ZERO { expected = u8.toU8(45) }
    if self.index == usize.ONE { expected = u8.toU8(52) }
    if bytes[offset] != expected { self.valid = false }
    self.index = self.index + usize.ONE
    offset = offset + usize.ONE
  }
  return ()
}

effect fn flush(self: &mut Capture) -> () { return () }

impl Writer for Capture {
  writeAll: Capture.writeAll
  flush: Capture.flush
}

effect fn render() -> i32 ! WriterError {
  let mut capture = Capture { index: usize.ZERO, valid: true }
  let value = -42
  run Format.display<i32>(&value) |> Effect.provideMut<Writer>(&mut capture)
  if !capture.valid { return 1 }
  if capture.index != 3 { return 2 }
  return 42
}

effect fn recover(error: WriterError) -> i32 { return 3 }

pub fn main() -> i32 {
  return run Effect.catchAll(render(), recover)
}`

/** Static template parsing and reflection erase before the shared engine differential. */
export const templateFormattingAcceptance = `import silk.effect { Effect }
import silk.format { Format }
import silk.u8 as u8
import silk.usize as usize
import silk.writer { Writer, WriterError }

struct Capture { index: usize valid: bool }

effect fn writeAll(self: &mut Capture, bytes: &[u8]) -> () {
  let expected = [
    u8.toU8(74), u8.toU8(117), u8.toU8(108), u8.toU8(105),
    u8.toU8(97), u8.toU8(58), u8.toU8(51), u8.toU8(49)
  ]
  let mut offset = usize.ZERO
  while offset < bytes.length {
    if 8 <= self.index || bytes[offset] != expected[self.index] { self.valid = false }
    self.index = self.index + usize.ONE
    offset = offset + usize.ONE
  }
  return ()
}

effect fn flush(self: &mut Capture) -> () { return () }

impl Writer for Capture {
  writeAll: Capture.writeAll
  flush: Capture.flush
}

effect fn render() -> i32 ! WriterError {
  let mut capture = Capture { index: usize.ZERO, valid: true }
  let args = .{ name: "Julia", age: 31 }
  run Format.format("{name}", &args)
    |> Effect.provideMut<Writer>(&mut capture)
  run Format.format(":{age}", &args)
    |> Effect.provideMut<Writer>(&mut capture)
  if !capture.valid { return 1 }
  if capture.index != 8 { return 2 }
  return 42
}

effect fn recover(error: WriterError) -> i32 { return 3 }

pub fn main() -> i32 {
  return run Effect.catchAll(render(), recover)
}`

/** Explicit referent projection preserves runtime-indexed reads and writes on every engine. */
export const referenceProjectionAcceptance = `import silk.usize as usize

struct Buffer { values: [i32; 3] }

fn update(buffer: &mut Buffer, index: usize) -> i32 {
  buffer.*.values[index] = 42
  return buffer.*.values[index]
}

pub fn main() -> i32 {
  let mut buffer = Buffer { values: [1, 2, 3] }
  return update(&mut buffer, usize.ONE)
}`

/** Statement-form `if let` keeps pattern bindings live on every native success branch. */
export const retainedIfLetMatchAcceptance = `import silk.option { Option }

fn inspect(taken: Option<i32>) -> i32 {
  if let Option<i32>.Some { value: i } = move taken {
    return i
  } else {
    return 0
  }
}

pub fn main() -> i32 {
  return inspect(Option<i32>.Some { value: 42 }) + inspect(Option<i32>.None)
}`

/** A top-level loop header must remain distinct from the LLVM function entry block. */
export const whileEntryBackedgeAcceptance = `import silk.option { Option }

pub fn main() -> i32 {
  while true {
    let taken = Option<i32>.None
    if let Option<i32>.Some { value: i } = move taken {
      if i == 0 { break }
    } else {
      break
    }
  }
  return 42
}`

/** Two independent roots resume in reverse suspension order without sharing a frame stack. */
export const independentExecutionNonLifo = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }
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
effect fn program() -> i32 ! OutOfMemoryError {
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
effect fn recover(error: OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionIllegalDormantDrive = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }
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
effect fn program() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut owner = Owner { slot: Empty {} }
  let execution = run Execution.make(body(), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  run driveOnce(move execution, &mut owner)
  let selected = Intrinsic.replace(owner.slot, Empty {})
  run driveStored(move selected, &mut owner)
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

/** Attempts to drive a Dormant execution reentrantly while its fixed endpoint is being notified. */
export const independentExecutionIllegalNotifyingDrive = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }
import silk.shared { Shared }
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
fn ready(owner: &Shared<Owner>) -> () {
  let selected = Shared.withMut(owner, take)
  let mut state = ()
  run reenter(move selected, &mut state)
  return ()
}
fn complete(state: &mut (), result: i32) -> () { return () }
fn suspend(
  state: &mut (),
  execution: Intrinsic.Execution<i32>,
  owner: Shared<Owner>
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
effect fn program() -> i32 ! OutOfMemoryError {
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
effect fn recover(error: OutOfMemoryError) -> i32 { return -2 }
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
export const independentExecutionStackExhaustion = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }
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
effect fn program() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut state = State { completed: 0 }
  let execution = run Execution.make(body(), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  run driveOnce(move execution, &mut state)
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return -2 }
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

export const independentExecutionMultiplePackages = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }
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
effect fn program() -> i32 ! OutOfMemoryError {
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
effect fn recover(error: OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionLateCancelledWake = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }
import silk.shared { Shared }
struct Empty {}
struct Waiting { wake: Intrinsic.Wake }
struct Mailbox { slot: Empty | Waiting }
struct Guard { mailbox: Shared<Mailbox> }
struct ReadyState { called: i32 }
fn install(mailbox: &mut Mailbox, wake: Intrinsic.Wake) -> () {
  let previous = Intrinsic.replace(mailbox.slot, Waiting { wake: move wake })
  drop previous
  return ()
}
fn register(wake: Intrinsic.Wake, mailbox: Shared<Mailbox>) -> Guard {
  let installing = install(move wake)
  Shared.withMut(&mailbox, move installing)
  return Guard { mailbox: move mailbox }
}
fn extract(mailbox: &mut Mailbox) -> Empty | Waiting {
  return Intrinsic.replace(mailbox.slot, Empty {})
}
effect fn body(mailbox: Shared<Mailbox>) -> i32 {
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
fn ready(state: &Shared<ReadyState>) -> () {
  Shared.withMut(state, markReady)
  return ()
}
fn readReady(state: &mut ReadyState) -> i32 { return state.called }
effect fn driveOnce(execution: Intrinsic.Execution<i32>, state: &mut ()) -> () {
  return run Execution.drive(move execution, move state, complete, cancel)
}
effect fn program() -> i32 ! OutOfMemoryError {
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
effect fn recover(error: OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionReentrantDestroy = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }
import silk.shared { Shared }
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
fn ready(owner: &Shared<Owner>) -> () {
  let selected = Shared.withMut(owner, take)
  drop selected
  return ()
}
fn complete(state: &mut (), result: i32) -> () { return () }
fn suspend(
  state: &mut (),
  execution: Intrinsic.Execution<i32>,
  owner: Shared<Owner>
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
effect fn program() -> i32 ! OutOfMemoryError {
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
effect fn recover(error: OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionLocalReactor = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }
import silk.shared { Shared }
struct Empty {}
struct Armed { wake: Intrinsic.Wake }
struct Reactor { slot: Empty | Armed }
struct Guard { reactor: Shared<Reactor> }
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored result: i32 }
fn install(reactor: &mut Reactor, wake: Intrinsic.Wake) -> () {
  let previous = Intrinsic.replace(reactor.slot, Armed { wake: move wake })
  drop previous
  return ()
}
fn register(wake: Intrinsic.Wake, reactor: Shared<Reactor>) -> Guard {
  let installing = install(move wake)
  Shared.withMut(&reactor, move installing)
  return Guard { reactor: move reactor }
}
fn extract(reactor: &mut Reactor) -> Empty | Armed {
  return Intrinsic.replace(reactor.slot, Empty {})
}
fn poll(reactor: &Shared<Reactor>) -> () {
  let selected = Shared.withMut(reactor, extract)
  return match move selected {
    Empty {} => ()
    Armed { wake } => Intrinsic.wake(move wake)
  }
}
effect fn body(reactor: Shared<Reactor>) -> i32 {
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
effect fn program() -> i32 ! OutOfMemoryError {
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
effect fn recover(error: OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionRepeatedGenerations = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }
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
effect fn program() -> i32 ! OutOfMemoryError {
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
effect fn recover(error: OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionEligibleDrop = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }
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
effect fn program() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut owner = Owner { slot: Empty {} }
  let execution = run Execution.make(body(), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  run drive(move execution, &mut owner)
  let selected = Intrinsic.replace(owner.slot, Empty {})
  drop selected
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const independentExecutionParkedTypedFailure = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }
import silk.result { Result }
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
      Result<i32, Failed>.Success { value } => value
      Result<i32, Failed>.Failure { error } => match move error { Failed { code } => code }
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
effect fn program() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut owner = Owner { slot: Empty {}, result: 0 }
  let execution = run Execution.make(body(), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  run drive(move execution, &mut owner)
  let selected = Intrinsic.replace(owner.slot, Empty {})
  run driveStored(move selected, &mut owner)
  return owner.result
}
effect fn recover(error: OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

/**
 * The outer suspension frame retains a never-driven Execution whose package owns continuation
 * storage. Native cleanup emission used to re-expand the module's frame inventory for that
 * retained handle until the compiler exhausted its heap; the handle is released on the frame's
 * ordinary completion path.
 */
export const frameRetainedExecutionCompletion = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }

fn ready(state: &()) -> () { return () }

effect fn child() -> i32 {
  return run Effect.suspend(effect { return 42 })
}

effect fn holdAcrossSuspension() -> i32 ! OutOfMemoryError ? &mut Allocator {
  let execution = run Execution.make(child(), (), ready)
  let result = run Effect.suspend(effect { return 0 })
  drop execution
  return result
}

effect fn program() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run holdAcrossSuspension()
    |> Effect.provideMut<Allocator>(&mut allocator)
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 1 }

pub fn main() -> i32 {
  return run Effect.catchAll(program(), recover)
}`

/**
 * An eligible outer Execution is dropped while its parked frame still retains an inner Execution,
 * so frame cleanup must release the nested package through the runtime cleanup path.
 */
export const frameRetainedExecutionAbandonedFrame = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.execution { Execution }
struct Empty {}
struct Stored { execution: Intrinsic.Execution<i32> }
struct Owner { slot: Empty | Stored }
struct Guard {}
fn register(wake: Intrinsic.Wake) -> Guard { Intrinsic.wake(move wake) return Guard {} }
fn ready(state: &()) -> () { return () }
effect fn child() -> i32 { return run Effect.suspend(effect { return 42 }) }
effect fn holder(execution: Intrinsic.Execution<i32>) -> i32 {
  run Execution.park(register)
  drop execution
  return 1
}
fn complete(owner: &mut Owner, result: i32) -> () { return () }
fn suspend(owner: &mut Owner, execution: Intrinsic.Execution<i32>) -> () {
  let previous = Intrinsic.replace(owner.slot, Stored { execution: move execution })
  drop previous
  return ()
}
effect fn drive(execution: Intrinsic.Execution<i32>, owner: &mut Owner) -> () {
  return run Execution.drive(move execution, move owner, complete, suspend)
}
effect fn program() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut owner = Owner { slot: Empty {} }
  let retained = run Execution.make(child(), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  let outer = run Execution.make(holder(move retained), (), ready)
    |> Effect.provideMut<Allocator>(&mut allocator)
  run drive(move outer, &mut owner)
  let selected = Intrinsic.replace(owner.slot, Empty {})
  drop selected
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return -2 }
pub fn main() -> i32 { return run Effect.catchAll(program(), recover) }`

export const constrainedCallableForwarding = `import silk.effect { Effect }
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

/** Fixed-seed xoshiro256** known answers pinned for native execution. */
export const seededRandomFingerprint = `import silk.effect { Effect }
import silk.insecure_random { InsecureRandom }
import silk.u64 as u64
import silk.usize as usize

fn matches(seed: u64, expected: &[u64]) -> bool {
  let mut provider = InsecureRandom.seeded(seed)
  let mut index = usize.ZERO
  while index < expected.length {
    let actual = run InsecureRandom.nextU64()
      |> Effect.provideMut<InsecureRandom>(&mut provider)
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

/** Portable secure-provider and stable insecure-seed behavior pinned for native execution. */
export const portableRandomCapabilities = `import silk.effect { Effect }
import silk.insecure_seed { InsecureSeed }
import silk.random { Random }
import silk.u64 as u64
import silk.usize as usize

struct ScriptedRandom {
  first: u64
  second: u64
  calls: usize
}

effect fn fill(self: &mut ScriptedRandom, output: &mut [u8]) -> () {
  if output.length == usize.ZERO { return () }
  let mut word = self.second
  if self.calls == usize.ZERO { word = self.first }
  self.calls = self.calls + usize.ONE
  let mut index = usize.ZERO
  while index < output.length {
    output[index] = u64.toU8(u64.bitAnd(word, 255))
    word = u64.shiftRight(word, 8)
    index = index + usize.ONE
  }
  return ()
}

impl Random for ScriptedRandom { fillBytes: ScriptedRandom.fill }

pub fn main() -> i32 {
  let fixed = InsecureSeed.fixed(40, 2)
  let fixedSeed = run InsecureSeed.get()
    |> Effect.provide<InsecureSeed>(&fixed)
  if InsecureSeed.first(&fixedSeed) + InsecureSeed.second(&fixedSeed) != 42 { return 1 }

  let mut random = ScriptedRandom { first: u64.toU64(20), second: u64.toU64(22), calls: usize.ZERO }
  let sampled = run InsecureSeed.fromRandom()
    |> Effect.provideMut<Random>(&mut random)
  let sampledSeed = run InsecureSeed.get()
    |> Effect.provide<InsecureSeed>(&sampled)
  if random.calls != 2 { return 2 }
  if InsecureSeed.first(&sampledSeed) + InsecureSeed.second(&sampledSeed) != 42 { return 3 }
  return 42
}`

/** Failure payloads retain member bits while rows change their widest physical carrier lane. */
export const heterogeneousFailurePayload = `import silk.effect { Effect }
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
import silk.effect { Effect }
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
import silk.effect { Effect }
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
    let index: usize = 1
    self.value = values[index]
    return ()`,
  )

const staticCompositionCorpus: ReadonlyArray<CorpusProgram> = [
  ...staticCompositionScenarios.map((scenario): CorpusProgram => ({
    name: `static-composition-${scenario.name}`,
    source: selectStaticCompositionScenario(scenario.selection),
    expected: { _tag: 'Completes', result: scenario.result },
  })),
  ...staticCompositionScenarios
    .filter((scenario) => scenario.cleanupWitness)
    .map((scenario): CorpusProgram => ({
      name: `static-composition-${scenario.name}-cleanup-witness`,
      source: withTrappingStaticCompositionDrop(
        selectStaticCompositionScenario(scenario.selection),
      ),
      expected: { _tag: 'Trap' },
    })),
]

/** A Silk callee stores through a `*mut i32` parameter; the caller reloads the place afterwards. */
const pointerParameterWrite = `import silk.pointer { Pointer }
fn store(target: *mut i32, value: i32) -> () {
  unsafe { Pointer.write(target, value) }
}
pub fn main() -> i32 {
  let mut result = 0
  store(Pointer.fromMutRef(&mut result), 42)
  return result
}`

export const corpus: ReadonlyArray<CorpusProgram> = [
  {
    name: 'literal',
    source: 'pub fn main() -> i32 { return 42 }',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'tuple-record-aggregates',
    source: `tuple Point(i32, i32)
struct Pair { left: i32 right: i32 }
enum Choice { First, Second }
fn identity<T>(value: T) -> T { return move value }
fn sumPair(value: Pair) -> i32 { return value.left + value.right }
fn select(choice: Choice) -> Pair {
  let selected: Pair = match choice {
    Choice.First => .{ left: 20, right: 22 }
    Choice.Second => .{ left: 21, right: 21 }
  }
  return move selected
}
pub fn main() -> i32 {
  let point = Point(20, 22)
  if point.0 + point.1 != 42 { return 1 }
  let coordinates = (20, 22)
  if coordinates.0 + coordinates.1 != 42 { return 2 }
  let record = identity(.{ left: 20, right: 22 })
  if record.left + record.right != 42 { return 3 }
  if sumPair(.{ left: 20, right: 22 }) != 42 { return 4 }
  let selected = select(Choice.First)
  return selected.left + selected.right
}`,
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
    name: 'scalar-display',
    source: scalarDisplayAcceptance,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'template-formatting',
    source: templateFormattingAcceptance,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'retained-if-let-match-binding',
    source: retainedIfLetMatchAcceptance,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'while-entry-backedge',
    source: whileEntryBackedgeAcceptance,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'reference-projection',
    source: referenceProjectionAcceptance,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'nominal-result-compound-error',
    source: `struct Data { value: i32 }
union HttpErrorCode { DNSTimeout, DNSError { rcode: i32 } }
struct OutOfMemoryError {}
union Result<A, E> { Success { value: A }, Failure { error: E } }

fn inspect(result: Result<Data, HttpErrorCode | OutOfMemoryError>) -> i32 {
  return match move result {
    Result<Data, HttpErrorCode | OutOfMemoryError>.Success { value } => value.value
    Result<Data, HttpErrorCode | OutOfMemoryError>.Failure { error } => match move error {
      HttpErrorCode.DNSTimeout => 1
      HttpErrorCode.DNSError { rcode } => rcode
      OutOfMemoryError other => 0
    }
  }
}

pub fn main() -> i32 {
  let success = Result<Data, HttpErrorCode | OutOfMemoryError>.Success {
    value: Data { value: 21 },
  }
  let failure = Result<Data, HttpErrorCode | OutOfMemoryError>.Failure {
    error: HttpErrorCode.DNSError { rcode: 21 },
  }
  return inspect(move success) + inspect(move failure)
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'nominal-union-represented-copy-drop',
    source: `union Parser<F: once fn(i32) -> i32> { Empty, Ready { parse: F } }
union Deferred<F: once Effect<i32>> { Empty, Ready { operation: F } }
union Flag { Empty, Value { value: i32 } }
impl Copy for Flag {}
struct Token {}
impl Drop for Token { fn drop(self: &mut Token) -> () { return () } }
union Owner { Empty, Present { token: Token, value: i32 } }

fn increment(value: i32) -> i32 { return value + 1 }
fn parse<F: once fn(i32) -> i32>(parser: Parser<F>) -> i32 {
  return match move parser {
    Parser<F>.Empty => 0
    Parser<F>.Ready { parse } => parse(19)
  }
}
fn force<F: once Effect<i32>>(deferred: Deferred<F>) -> i32 {
  return match move deferred {
    Deferred<F>.Empty => 0
    Deferred<F>.Ready { operation } => run operation
  }
}
fn copyFlag(flag: Flag) -> i32 {
  let copied = flag
  return match move copied { Flag.Empty => 0 Flag.Value { value } => value }
}
fn consume(owner: Owner) -> i32 {
  return match move owner { Owner.Empty => 0 Owner.Present { value, .. } => value }
}
pub fn main() -> i32 {
  let parser = Parser.Ready { parse: increment }
  let deferred = Deferred.Ready { operation: effect { return 18 } }
  let owner = Owner.Present { token: Token {}, value: 2 }
  return parse(move parser) + force(move deferred) + copyFlag(Flag.Value { value: 2 }) + consume(move owner)
}`,
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
  // CALLABLE-002: every stage may pass through a binding, keeping the captured suffix in place.
  {
    name: 'staged-callable-bindings',
    source: `fn combine(a: i32, b: i32, c: i32) -> i32 {
  return a * 100 + b * 10 + c
}
pub fn main() -> i32 {
  let withThree = combine(3)
  let withTwoAndThree = withThree(2)
  return withTwoAndThree(1)
}`,
    expected: { _tag: 'Completes', result: 123 },
  },
  // CALLABLE-002: a stage over an erased callable parameter splices the hidden environment.
  {
    name: 'staged-callable-parameter',
    source: `fn combine(a: i32, b: i32, c: i32) -> i32 { return a * 100 + b * 10 + c }
fn stage(f: fn(i32, i32) -> i32) -> i32 {
  let g = f(2)
  return g(1)
}
pub fn main() -> i32 { return stage(combine(3)) - 123 + 42 }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // JUL-72: one source-defined anonymous environment covers Copy, shared, exclusive, and moved
  // captures so the native corpus owns runtime coverage for the feature.
  {
    name: 'anonymous-callable-capture-modes',
    source: `struct Token { value: i32 }
fn consume(token: Token) -> i32 { return token.value }
pub fn main() -> i32 {
  let copied = 2
  let extra = 40
  let shared = Token { value: 10 }
  let mut counter = 0
  let owned = Token { value: 17 }
  let copyStep = fn(base: i32) -> i32 { return base + extra + move copied - 40 }
  let sharedStep = fn() -> i32 { return shared.value }
  let mut mutateStep = fn() -> i32 {
    counter = counter + 1
    return counter
  }
  let consumeStep = fn() -> i32 { return consume(move owned) }
  return copyStep(0) + sharedStep() + sharedStep()
    + mutateStep() + mutateStep() + consumeStep()
}`,
    expected: { _tag: 'Completes', result: 42 },
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
import silk.effect { Effect }
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
    name: 'applied-interface-operation-calls',
    source: `interface Encodable<Format> {
  effect fn encode(self: &Self) -> i32
}
struct Numeric {}
struct Textual {}
struct Age { value: i32 }
impl Encodable<Numeric> for Age {
  effect fn encode(self: &Self) -> i32 { return self.value }
}
impl Encodable<Textual> for Age {
  effect fn encode(self: &Self) -> i32 { return 10 }
}
pub fn main() -> i32 {
  let age = Age { value: 32 }
  let numeric = run Encodable<Numeric>.encode(&age)
  let textual = run &age |> Encodable<Textual>.encode
  return numeric + textual
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
import silk.effect { Effect }
import silk.u32 as u32
import silk.string { ScalarCursor, ScalarStep, String }
import silk.option { Option }
import silk.char { toU32 as charToU32 }

fn scalarSum(value: string, cursor: ScalarCursor) -> u32 {
  return match move String.nextScalar(value, move cursor) {
    Option<ScalarStep>.Some { value: step } => continueSum(value, move step)
    Option<ScalarStep>.None => u32.toU32(0)
  }
}

fn continueSum(value: string, step: ScalarStep) -> u32 {
  let scalar = charToU32(String.scalarValue(&step))
  let cursor = String.nextCursor(move step)
  return scalar + scalarSum(value, move cursor)
}

effect fn build() -> i32 ! OutOfMemoryError {
  let literal = "A\\u{a2}"
  if literal == "A\\u{a2}" {} else { return 1 }
  if literal != "A\\u{a3}" {} else { return 2 }

  let mut allocator = Allocator.systemAllocatorProvider()
  let copying = String.copy(literal) |> Effect.provideMut(&mut allocator)
  let mut owned = run copying
  let appending = String.append(&mut owned, "\\u{20ac}\\u{10348}")
    |> Effect.provideMut(&mut allocator)
  let appended = run appending
  let borrowed = String.view(&owned)
  if borrowed == "A\\u{a2}\\u{20ac}\\u{10348}" {} else { return 3 }
  if scalarSum(borrowed, String.scalarCursor()) == u32.toU32(74967) {} else { return 4 }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  // folded from UnicodeNormalization.test.ts: the two normalized owners compared directly, which
  // native execution answers correctly while WebAssembly support remains incomplete.
  {
    name: 'unicode-compared-directly',
    source: `import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
import silk.string { String }
import silk.unicode { Unicode }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let left = run Unicode.normalizeNfc("\\u{e9}") |> Effect.provideMut(&mut allocator)
  let right = run Unicode.normalizeNfc("e\\u{301}") |> Effect.provideMut(&mut allocator)
  if String.view(&left) == String.view(&right) {} else { return 1 }
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
  if gt('\u{10ffff}', '\u{10000}') {} else { return 26 }
  if gt('\u{e000}', '\u{d7ff}') {} else { return 27 }
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
  {
    name: 'module-string-constants',
    source: `import silk.usize as usize
import silk.string { String }
const escapedPattern: string = "\\\\d+\\\\.\\\\d+"
const rawPattern: string = r"\\d+\\.\\d+"
const windowsPath: string = r"C:\\Users\\build"
fn sameText(left: string, right: string) -> bool {
  if String.byteLength(left) != String.byteLength(right) { return false }
  let leftBytes = String.utf8Bytes(left)
  let rightBytes = String.utf8Bytes(right)
  let mut index = usize.ZERO
  while index < leftBytes.length {
    if leftBytes[index] != rightBytes[index] { return false }
    index = index + usize.ONE
  }
  return true
}
pub fn main() -> i32 {
  if !sameText(escapedPattern, r"\\d+\\.\\d+") { return 1 }
  if !sameText(rawPattern, "\\\\d+\\\\.\\\\d+") { return 2 }
  if !sameText(rawPattern, escapedPattern) { return 3 }
  if usize.toI32(String.byteLength(rawPattern)) != 8 { return 4 }
  if usize.toI32(String.byteLength(windowsPath)) != 14 { return 5 }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'whole-member-layout-extraction',
    source: `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.layout { Layout, LayoutOverflow }
import silk.raw_buffer { RawBuffer }
import silk.slot { Slot }
fn trapLayout() -> Layout { let boom = 1 / 0 return trapLayout() }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let plan = Layout.repeat(Layout.of<i32>(), 3)
  let layout = match move plan {
    Layout value => move value
    LayoutOverflow overflow => trapLayout()
  }
  let allocation = run Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  unsafe {
    let mut buffer = RawBuffer.from<i32>(move allocation, 3)
    let written = Slot.write(RawBuffer.slot(&mut buffer, 2), 42)
    let taken = Slot.take(RawBuffer.slot(&mut buffer, 2))
    drop buffer
    return taken
  }
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'whole-member-affine-extraction',
    source: `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.layout { Layout }
struct Empty {}
struct Full { storage: Allocation }
fn wrap(cell: Full) -> Empty | Full { return move cell }
fn takeStorage(full: Full) -> Allocation {
  return match move full { Full { storage } => move storage }
}
effect fn fallback() -> Full ! OutOfMemoryError { fail OutOfMemoryError {} }
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let allocation = run Allocator.allocate(Layout.of<[i32; 2]>())
    |> Effect.provideMut(&mut allocator)
  let widened = wrap(Full { storage: move allocation })
  let restored = match move widened {
    Empty empty => run fallback()
    Full full => Full { storage: takeStorage(move full) }
  }
  drop restored
  return 42
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
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
  {
    name: 'bitwise-values-and-precedence',
    source: `import silk.u32 as u32
pub fn main() -> i32 {
  let a: u32 = 12
  let b: u32 = 10
  if (a & b) != u32.bitAnd(a, b) { return 1 }
  if (a | b) != u32.bitOr(a, b) { return 2 }
  if (a ^ b) != u32.bitXor(a, b) { return 3 }
  if ~a != u32.bitNot(a) { return 4 }
  if u32.toI32(a & b) != 8 { return 5 }
  if u32.toI32(a | b) != 14 { return 6 }
  if u32.toI32(a ^ b) != 6 { return 7 }
  if u32.toI32(1 | 2 ^ 3 & 1) != 3 { return 8 }
  if (6 & 3) == 2 {} else { return 9 }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'option-result-combinators',
    source: `import silk.option { Option }
import silk.result { Result }

struct Wide { code: i32 }
fn double(value: i32) -> i32 { return value * 2 }
fn halve(value: i32) -> Option<i32> {
  if value == 0 { return Option.none<i32>() }
  return Option.some<i32>(value / 2)
}
fn addTwo(value: i32) -> i32 { return value + 2 }
fn halfResult(value: i32) -> Result<i32, i32> {
  if value == 0 { return Result.failResult<i32, i32>(9) }
  return Result.succeed<i32, i32>(value / 2)
}
fn widen(error: i32) -> Wide { return Wide { code: error + 30 } }
fn observeWide(self: Result<i32, Wide>) -> i32 {
  return match move self {
    Result<i32, Wide>.Success { value: success } => success
    Result<i32, Wide>.Failure { error } => error.code
  }
}

pub fn main() -> i32 {
  let mappedSome = Option.map<i32, i32>(Option.some<i32>(20), double)
  let mappedNone = Option.map<i32, i32>(Option.none<i32>(), double)
  let chainedSome = Option.flatMap<i32, i32>(Option.some<i32>(80), halve)
  let chainedNone = Option.flatMap<i32, i32>(Option.some<i32>(0), halve)
  if Option.unwrapOr<i32>(move mappedSome, 0) != 40 { return 1 }
  if Option.unwrapOr<i32>(move mappedNone, 2) != 2 { return 2 }
  if Option.unwrapOr<i32>(move chainedSome, 0) != 40 { return 3 }
  if Option.unwrapOr<i32>(move chainedNone, 2) != 2 { return 4 }

  let mapped = Result.map<i32, i32, i32>(Result.succeed<i32, i32>(36), addTwo)
  let chained = Result.flatMap<i32, i32, i32>(move mapped, halfResult)
  if Result.unwrapOr<i32, i32>(move chained, 0) != 19 { return 5 }
  let carriedFailure = Result.map<i32, i32, i32>(Result.failResult<i32, i32>(7), addTwo)
  if Result.unwrapOr<i32, i32>(move carriedFailure, 23) != 23 { return 6 }
  let changed = Result.mapError<i32, i32, Wide>(Result.failResult<i32, i32>(4), widen)
  if observeWide(move changed) != 34 { return 7 }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'effect-source-combinators',
    source: `import silk.effect { Effect }
import silk.result { Result }

struct First { code: i32 }
struct Second { code: i32 }
effect fn succeed(value: i32) -> i32 ! First { return value }
effect fn failFirst() -> i32 ! First { fail First { code: 2 } }
fn addTwo(value: i32) -> i32 { return value + 2 }
effect fn addTwoEffect(value: i32) -> i32 ! Second { return value + 2 }
effect fn preserve(value: i32) -> i32 ! Second { return value }
effect fn recover(error: First) -> i32 ! Second { return error.code + 40 }
fn observeFirst(result: Result<i32, First>) -> i32 {
  return match move result {
    Result<i32, First>.Success { value } => value
    Result<i32, First>.Failure { error } => error.code
  }
}
fn observeBoth(result: Result<i32, First | Second>) -> i32 {
  return match move result {
    Result<i32, First | Second>.Success { value } => value
    Result<i32, First | Second>.Failure { error } => match move error {
      First { code: firstCode } => firstCode
      Second { code: secondCode } => secondCode
    }
  }
}
fn observeSecond(result: Result<i32, Second>) -> i32 {
  return match move result {
    Result<i32, Second>.Success { value } => value
    Result<i32, Second>.Failure { error } => error.code
  }
}
effect fn inner(value: i32) -> i32 { return value * 2 }
effect fn outer(value: i32) -> Effect<i32> { return inner(value) }

pub fn main() -> i32 {
  let mapped = run Effect.result(succeed(40) |> Effect.map(addTwo))
  if observeFirst(move mapped) != 42 { return 1 }
  let chained = run Effect.result(succeed(40) |> Effect.flatMap(addTwoEffect))
  if observeBoth(move chained) != 42 { return 2 }
  let tapped = run Effect.result(succeed(42) |> Effect.tap(preserve))
  if observeBoth(move tapped) != 42 { return 3 }
  let recovered = run Effect.result(failFirst() |> Effect.catchAll(recover))
  if observeSecond(move recovered) != 42 { return 4 }
  let nested = outer(21)
  let flattened = Effect.flatten(move nested)
  let flattenedValue = run flattened
  if flattenedValue != 42 { return 5 }
  let created = Effect.of(42)
  let createdValue = run created
  if createdValue != 42 { return 6 }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'effect-retry-and-provide-effect',
    source: `import silk.effect { Effect }
import silk.result { Result }

struct Problem { code: i32 }
service Clock { effect fn value() -> i32 ? &Clock }
struct FixedClock { value: i32 }
effect fn clockValue(self: &FixedClock) -> i32 { return self.value }
impl Clock for FixedClock { value: FixedClock.clockValue }
service Journal { effect fn acquire() -> () ? &mut Journal }
struct MemoryJournal { count: i32 }
effect fn acquire(self: &mut MemoryJournal) -> () {
  self.count = self.count + 1
  return ()
}
impl Journal for MemoryJournal { acquire: MemoryJournal.acquire }
effect fn read() -> i32 ? &Clock { return run Clock.value() }
effect fn nestedRead(inner: &FixedClock) -> i32 ? &Clock {
  let before = run read()
  let inside = run read() |> Effect.provide(inner)
  let after = run read()
  return before + inside + after
}
effect fn makeClock() -> FixedClock { return FixedClock { value: 42 } }
effect fn makeTrackedClock() -> FixedClock ? &mut Journal {
  run Journal.acquire()
  return FixedClock { value: 42 }
}
effect fn readAndFail() -> i32 ! Problem ? &Clock {
  let value = run Clock.value()
  fail Problem { code: value }
}
effect fn succeed() -> i32 ! Problem { return 42 }
effect fn failAlways() -> i32 ! Problem { fail Problem { code: 2 } }
fn observe(result: Result<i32, Problem>) -> i32 {
  return match move result {
    Result<i32, Problem>.Success { value } => value
    Result<i32, Problem>.Failure { error } => error.code
  }
}

pub fn main() -> i32 {
  let success = run Effect.result(succeed() |> Effect.retry(3))
  let failure = run Effect.result(failAlways() |> Effect.retry(2))
  if observe(move success) != 42 { return 1 }
  if observe(move failure) != 2 { return 2 }
  let fixed = FixedClock { value: 42 }
  let direct = run read() |> Effect.provide(&fixed)
  if direct != 42 { return 3 }
  let provided = run read() |> Effect.provideEffect(makeClock())
  if provided != 42 { return 4 }
  let outer = FixedClock { value: 20 }
  let inner = FixedClock { value: 2 }
  let nested = run nestedRead(&inner) |> Effect.provide(&outer)
  if nested != 42 { return 5 }
  let mut journal = MemoryJournal { count: 0 }
  let tracked = readAndFail() |> Effect.provideEffect(makeTrackedClock()) |> Effect.retry(2)
  let trackedResult = run Effect.result(move tracked) |> Effect.provideMut(&mut journal)
  if observe(move trackedResult) != 42 { return 6 }
  if journal.count != 3 { return 7 }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'effect-ensuring-outcomes-and-order',
    source: `import silk.effect { Effect }

struct Problem { code: i32 }
service Journal { effect fn mark(value: i32) -> () ? &mut Journal }
struct MemoryJournal { value: i32 }
effect fn mark(self: &mut MemoryJournal, value: i32) -> () {
  self.value = self.value * 10 + value
  return ()
}
impl Journal for MemoryJournal { mark: MemoryJournal.mark }
effect fn succeed() -> i32 ? &mut Journal { run Journal.mark(1) return 5 }
effect fn failWork() -> i32 ! Problem ? &mut Journal {
  run Journal.mark(1)
  fail Problem { code: 3 }
}
effect fn finalize() -> () ? &mut Journal { return run Journal.mark(2) }
effect fn recover(error: Problem) -> i32 { return error.code }

pub fn main() -> i32 {
  let mut first = MemoryJournal { value: 0 }
  let success = run Effect.ensuring(succeed(), finalize()) |> Effect.provideMut(&mut first)
  if success != 5 { return 1 }
  if first.value != 12 { return 2 }
  let mut second = MemoryJournal { value: 0 }
  let guarded = Effect.ensuring(failWork(), finalize()) |> Effect.catchAll(recover)
  let failure = run guarded |> Effect.provideMut(&mut second)
  if failure != 3 { return 3 }
  if second.value != 12 { return 4 }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'effect-zip-order-and-failures',
    source: `import silk.effect { Effect }
import silk.effect { Pair, Triple }

struct Problem { code: i32 }
service Journal { effect fn mark(value: i32) -> () ? &mut Journal }
struct MemoryJournal { value: i32 }
effect fn mark(self: &mut MemoryJournal, value: i32) -> () {
  self.value = self.value * 10 + value
  return ()
}
impl Journal for MemoryJournal { mark: MemoryJournal.mark }
effect fn first() -> i32 ? &mut Journal { run Journal.mark(1) return 20 }
effect fn second() -> i32 ? &mut Journal { run Journal.mark(2) return 22 }
effect fn third() -> i32 ? &mut Journal { run Journal.mark(3) return 0 }
effect fn failFirst() -> i32 ! Problem ? &mut Journal {
  run Journal.mark(1)
  fail Problem { code: 7 }
}
effect fn failSecond() -> i32 ! Problem ? &mut Journal {
  run Journal.mark(2)
  fail Problem { code: 8 }
}
fn pairValue(pair: Pair<i32, i32>) -> i32 { return pair.first + pair.second }
fn tripleValue(triple: Triple<i32, i32, i32>) -> i32 {
  return triple.first + triple.second + triple.third
}
effect fn recover(error: Problem) -> i32 { return error.code }

pub fn main() -> i32 {
  let mut successLog = MemoryJournal { value: 0 }
  let success = run Effect.zip(first(), second()) |> Effect.map(pairValue)
    |> Effect.provideMut(&mut successLog)
  if success != 42 { return 1 }
  if successLog.value != 12 { return 2 }
  let mut tripleLog = MemoryJournal { value: 0 }
  let triple = run Effect.zip3(first(), second(), third()) |> Effect.map(tripleValue)
    |> Effect.provideMut(&mut tripleLog)
  if triple != 42 { return 3 }
  if tripleLog.value != 123 { return 4 }
  let mut firstFailureLog = MemoryJournal { value: 0 }
  let firstFailure = run Effect.zip(failFirst(), second()) |> Effect.map(pairValue)
    |> Effect.catchAll(recover) |> Effect.provideMut(&mut firstFailureLog)
  if firstFailure != 7 { return 5 }
  if firstFailureLog.value != 1 { return 6 }
  let mut secondFailureLog = MemoryJournal { value: 0 }
  let secondFailure = run Effect.zip(first(), failSecond()) |> Effect.map(pairValue)
    |> Effect.catchAll(recover) |> Effect.provideMut(&mut secondFailureLog)
  if secondFailure != 8 { return 7 }
  if secondFailureLog.value != 12 { return 8 }
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
import silk.effect { Effect }
import silk.i32 as i32
import silk.hash { Hash }
import silk.hash { HashKey, HashSeed, Word }
import silk.hash_map { HashMap }
import silk.option { Option }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut map = HashMap.make<Word, i32>(Hash.seed(4242))
  let mut key = 0
  while key < 40 {
    let previous = run HashMap.insert<Word, i32>(&mut map, Hash.word(i32.toU64(key)), key * 3)
      |> Effect.provideMut(&mut allocator)
    drop previous
    key = key + 1
  }
  if HashMap.length<Word, i32>(&map) != 40 { return 1 }
  if HashMap.bucketCount<Word, i32>(&map) <= 40 { return 2 }
  let mut probe = 0
  let mut total = 0
  while probe < 40 {
    let found = Option.unwrapOr<i32>(HashMap.get<Word, i32>(&map, Hash.word(i32.toU64(probe))), -1)
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
import silk.effect { Effect }
import silk.vector { Vector }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<i32>()
  let pending0 = Vector.append<i32>(&mut values, 10) |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let pending1 = Vector.append<i32>(&mut values, 11) |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let pending2 = Vector.append<i32>(&mut values, 12) |> Effect.provideMut(&mut allocator)
  let appended2 = run pending2
  let pending3 = Vector.append<i32>(&mut values, 13) |> Effect.provideMut(&mut allocator)
  let appended3 = run pending3
  let pending4 = Vector.append<i32>(&mut values, 14) |> Effect.provideMut(&mut allocator)
  let appended4 = run pending4
  let pending5 = Vector.append<i32>(&mut values, 15) |> Effect.provideMut(&mut allocator)
  let appended5 = run pending5
  if Vector.length<i32>(&values) == 6 {} else { return 0 }
  if Vector.capacity<i32>(&values) == 8 {} else { return 1 }
  let first = Vector.get<i32>(&values, 0)
  let last = Vector.get<i32>(&values, 5)
  return first + last + 17
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }

pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'vector-mutation-views-and-capacity',
    source: `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.option { Option }
import silk.usize as usize
import silk.vector { Vector }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = Vector.make<i32>()
  let a = run Vector.append<i32>(&mut values, 10) |> Effect.provideMut(&mut allocator)
  let b = run Vector.append<i32>(&mut values, 20) |> Effect.provideMut(&mut allocator)
  let c = run Vector.append<i32>(&mut values, 30) |> Effect.provideMut(&mut allocator)
  let d = run Vector.append<i32>(&mut values, 40) |> Effect.provideMut(&mut allocator)
  let originalCapacity = Vector.capacity<i32>(&values)
  let mutable = Vector.asMutSlice<i32>(&mut values)
  mutable[usize.ONE] = 22
  Vector.set<i32>(&mut values, usize.ONE + usize.ONE, 33)
  let shared = Vector.asSlice<i32>(&values)
  if shared[usize.ZERO] != 10 { return 1 }
  if shared[usize.ONE] != 22 { return 2 }
  if shared[usize.ONE + usize.ONE] != 33 { return 3 }
  let removed = Vector.remove<i32>(&mut values, usize.ONE)
  if removed != 22 { return 4 }
  if Vector.get<i32>(&values, usize.ONE) != 33 { return 5 }
  let popped = Vector.pop<i32>(&mut values)
  let poppedValue = match move popped {
    Option<i32>.Some { value } => value
    Option<i32>.None => 0
  }
  if poppedValue != 40 { return 6 }
  Vector.truncate<i32>(&mut values, usize.ONE)
  if Vector.length<i32>(&values) != usize.ONE { return 7 }
  Vector.clear<i32>(&mut values)
  if Vector.length<i32>(&values) != usize.ZERO { return 8 }
  if Vector.capacity<i32>(&values) != originalCapacity { return 9 }
  let empty = Vector.pop<i32>(&mut values)
  return match move empty {
    Option<i32>.Some { value } => 10
    Option<i32>.None => 42
  }
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'vector-sort-search-stability',
    source: `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.option { Option }
import silk.order { Order }
import silk.usize as usize
import silk.vector { Vector }

struct Item { key: i32 tag: i32 }
fn itemLess(left: &Item, right: &Item) -> bool { return left.key < right.key }
impl Order for Item { lessThan: Item.itemLess }

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut items = Vector.make<Item>()
  let a = run Vector.append<Item>(&mut items, Item { key: 1, tag: 1 }) |> Effect.provideMut(&mut allocator)
  let b = run Vector.append<Item>(&mut items, Item { key: 0, tag: 2 }) |> Effect.provideMut(&mut allocator)
  let c = run Vector.append<Item>(&mut items, Item { key: 1, tag: 3 }) |> Effect.provideMut(&mut allocator)
  let d = run Vector.append<Item>(&mut items, Item { key: 0, tag: 4 }) |> Effect.provideMut(&mut allocator)
  let ordered = run Vector.sort<Item>(&mut items) |> Effect.provideMut(&mut allocator)
  let view = Vector.asSlice<Item>(&items)
  let folded = view[usize.ZERO].tag * 1000 + view[usize.ONE].tag * 100
    + view[usize.ONE + usize.ONE].tag * 10 + view[usize.ONE + usize.ONE + usize.ONE].tag
  if folded != 2413 { return 1 }

  let mut numbers = Vector.make<i32>()
  let n0 = run Vector.append<i32>(&mut numbers, 9) |> Effect.provideMut(&mut allocator)
  let n1 = run Vector.append<i32>(&mut numbers, 2) |> Effect.provideMut(&mut allocator)
  let n2 = run Vector.append<i32>(&mut numbers, 7) |> Effect.provideMut(&mut allocator)
  let n3 = run Vector.append<i32>(&mut numbers, 2) |> Effect.provideMut(&mut allocator)
  let n4 = run Vector.append<i32>(&mut numbers, 5) |> Effect.provideMut(&mut allocator)
  let sorted = run Vector.sort<i32>(&mut numbers) |> Effect.provideMut(&mut allocator)
  let hit = Vector.binarySearch<i32>(&numbers, 7)
  let miss = Vector.binarySearch<i32>(&numbers, 4)
  let duplicate = Vector.binarySearch<i32>(&numbers, 2)
  let hitIndex = match move hit { Option<usize>.Some { value } => usize.toI32(value) _ => -1 }
  let missIndex = match move miss { Option<usize>.Some { value } => usize.toI32(value) _ => -1 }
  let duplicateIndex = match move duplicate { Option<usize>.Some { value } => usize.toI32(value) _ => -1 }
  if hitIndex != 3 { return 2 }
  if missIndex != -1 { return 3 }
  if duplicateIndex != 0 { return 4 }
  let empty = Vector.make<i32>()
  let emptyMiss = Vector.binarySearch<i32>(&empty, 1)
  return match move emptyMiss { Option<usize>.Some { value } => 5 _ => 42 }
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'vector-move-only-sort-and-cleanup',
    source: `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.order { Order }
import silk.usize as usize
import silk.vector { Vector }

struct Tracked { key: i32 payload: Vector<i32> }
fn trackedLess(left: &Tracked, right: &Tracked) -> bool { return left.key < right.key }
impl Order for Tracked { lessThan: Tracked.trackedLess }
effect fn hold(key: i32) -> Tracked ! OutOfMemoryError ? &mut Allocator {
  let mut payload = Vector.make<i32>()
  let filled = run Vector.append<i32>(&mut payload, key)
  return Tracked { key: key, payload: move payload }
}
effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut items = Vector.make<Tracked>()
  let first = run hold(3) |> Effect.provideMut(&mut allocator)
  let a = run Vector.append<Tracked>(&mut items, move first) |> Effect.provideMut(&mut allocator)
  let second = run hold(1) |> Effect.provideMut(&mut allocator)
  let b = run Vector.append<Tracked>(&mut items, move second) |> Effect.provideMut(&mut allocator)
  let third = run hold(2) |> Effect.provideMut(&mut allocator)
  let c = run Vector.append<Tracked>(&mut items, move third) |> Effect.provideMut(&mut allocator)
  let ordered = run Vector.sort<Tracked>(&mut items) |> Effect.provideMut(&mut allocator)
  let view = Vector.asSlice<Tracked>(&items)
  let mut folded = 0
  let mut index = usize.ZERO
  while index < Vector.length<Tracked>(&items) {
    folded = folded * 10 + view[index].key
    index = index + usize.ONE
  }
  return folded
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 99 }
pub fn main() -> i32 { return run Effect.catchAll(build(), recover) }`,
    expected: { _tag: 'Completes', result: 123 },
  },
  {
    name: 'raw-buffer-copy-range',
    source: `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.raw_buffer { RawBuffer }
import silk.slot { Slot }
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let sourceLayout = Layout.of<[i32; 4]>()
  let sourceAllocation = run Allocator.allocate(move sourceLayout) |> Effect.provideMut(&mut allocator)
  let targetLayout = Layout.of<[i32; 4]>()
  let targetAllocation = run Allocator.allocate(move targetLayout) |> Effect.provideMut(&mut allocator)
  unsafe {
    let mut source = RawBuffer.from<i32>(move sourceAllocation, 4)
    let mut target = RawBuffer.from<i32>(move targetAllocation, 4)
    let first = Slot.write(RawBuffer.slot(&mut source, 0), 3)
    let second = Slot.write(RawBuffer.slot(&mut source, 1), 5)
    let third = Slot.write(RawBuffer.slot(&mut source, 2), 7)
    let prefix = RawBuffer.view<i32>(&source, 1, 2)
    let copied = RawBuffer.copy<i32>(&mut target, 2, prefix, 2)
    let low = RawBuffer.read<i32>(&target, 2)
    let high = RawBuffer.read<i32>(&target, 3)
    let kept = RawBuffer.read<i32>(&source, 1)
    let clearedFirst = Slot.take(RawBuffer.slot(&mut target, 2))
    let clearedSecond = Slot.take(RawBuffer.slot(&mut target, 3))
    let sourceFirst = Slot.take(RawBuffer.slot(&mut source, 0))
    let sourceSecond = Slot.take(RawBuffer.slot(&mut source, 1))
    let sourceThird = Slot.take(RawBuffer.slot(&mut source, 2))
    drop source
    drop target
    return low * 10 + high * 3 + kept
  }
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
    expected: { _tag: 'Completes', result: 76 },
  },
  {
    name: 'raw-buffer-fill-range',
    source: `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.layout { Layout }
import silk.raw_buffer { RawBuffer }
import silk.slot { Slot }
import silk.u8 as u8
effect fn store() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let layout = Layout.of<[u8; 4]>()
  let allocation = run Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  unsafe {
    let mut buffer = RawBuffer.from<u8>(move allocation, 4)
    let cleared = RawBuffer.fill(&mut buffer, 0, 4, u8.toU8(9))
    let refilled = RawBuffer.fill(&mut buffer, 1, 2, u8.toU8(3))
    let first = RawBuffer.read<u8>(&buffer, 0)
    let second = RawBuffer.read<u8>(&buffer, 1)
    let third = RawBuffer.read<u8>(&buffer, 2)
    let fourth = RawBuffer.read<u8>(&buffer, 3)
    let takenFirst = Slot.take(RawBuffer.slot(&mut buffer, 0))
    let takenSecond = Slot.take(RawBuffer.slot(&mut buffer, 1))
    let takenThird = Slot.take(RawBuffer.slot(&mut buffer, 2))
    let takenFourth = Slot.take(RawBuffer.slot(&mut buffer, 3))
    drop buffer
    return u8.toI32(first) * 8 + u8.toI32(second) * 4 + u8.toI32(third) * 2 + u8.toI32(fourth)
  }
  return 0
}
effect fn recover(error: OutOfMemoryError) -> i32 { return 7 }
pub fn main() -> i32 { return run Effect.catchAll(store(), recover) }`,
    expected: { _tag: 'Completes', result: 99 },
  },
  // folded from OwnedAllocationDispatch.test.ts: quota refusal propagates typed OutOfMemoryError.
  {
    name: 'owned-allocation-quota-refusal',
    source: `import silk.allocator { Allocator }
import silk.allocator { OutOfMemoryError }
import silk.allocator { Allocator }
import silk.allocator { SystemAllocator }
import silk.effect { Effect }
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
    source: `import silk.effect { Effect }

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
    source: `import silk.effect { Effect }

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
import silk.effect { Effect }
import silk.shared { Shared }

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
    source: `import silk.effect { Effect }

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
import silk.effect { Effect }
import silk.layout { Layout }
import silk.raw_buffer { RawBuffer }
import silk.slot { Slot }
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
import silk.effect { Effect }
import silk.f64 as f64
import silk.layout { Layout }
import silk.raw_buffer { RawBuffer }
import silk.slot { Slot }
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
import silk.effect { Effect }
import silk.u8 as u8
import silk.usize as usize
import silk.bytes { Bytes }

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
  let copying = Bytes.copy(&source) |> Effect.provideMut(&mut allocator)
  let mut bytes = run copying
  let suffix = [octet(42), octet(7)]
  let appending = Bytes.append(&mut bytes, &suffix) |> Effect.provideMut(&mut allocator)
  let appended = run appending
  let mut writable = Bytes.asMutSlice(&mut bytes)
  writable[1] = octet(2)
  let readable = Bytes.asSlice(&bytes)
  if Bytes.length(&bytes) == 6 {} else { return 1 }
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
import silk.effect { Effect }
import silk.layout { Layout }
import silk.raw_buffer { RawBuffer }
import silk.slot { Slot }
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
    source: `import silk.effect { Effect }
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
    source: `import silk.effect { Effect }
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
    source: `import silk.effect { Effect }
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
import silk.effect { Effect }
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
import silk.effect { Effect }
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
    source: `import silk.effect { Effect }
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
    source: `import silk.effect { Effect }
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
    source: `import silk.effect { Effect }
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
  // runs each one against the independently pinned result.
  ...floatMathPrograms.map((program) => ({
    name: program.name,
    source: program.source,
    expected: { _tag: 'Completes', result: 42 } as const,
  })),
  // Raw pointer parity: a formed slice pointer offset and written, then observed through the
  // array place; and a Silk callee writing through a `*mut i32` parameter observed by its caller.
  {
    name: 'pointer-slice-offset-write',
    source: `import silk.pointer { Pointer }
pub fn main() -> i32 {
  let mut values = [1, 2, 3, 4]
  let pointer = Pointer.fromMutSlice(&mut values)
  unsafe {
    let third = Pointer.offsetMut(pointer, 2)
    Pointer.write(third, 40)
    if Pointer.read(third) != 40 { return 1 }
  }
  return values[2] + values[3] - 2
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'pointer-parameter-write',
    source: pointerParameterWrite,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    // A bound method value is a section capturing parameter zero: every receiver mode and a
    // receiver-only member applied with no arguments.
    name: 'bound-method-values',
    source: `pub struct Counter { value: i32 }
impl Counter {
  pub fn read(self: &Self) -> i32 { return self.value }
  pub fn bump(self: &mut Self) -> i32 {
    self.value = self.value + 1
    return self.value
  }
  pub fn add(self: &Self, other: &Self) -> i32 { return self.value + other.value }
  pub fn take(self: Self) -> i32 { return self.value }
}
pub fn main() -> i32 {
  let shared = Counter { value: 10 }
  let read = shared.read
  let mut exclusive = Counter { value: 0 }
  let mut bump = exclusive.bump
  let first = bump()
  let owned = Counter { value: 5 }
  let add = owned.add
  let taken = Counter { value: 20 }
  let take = taken.take
  return read() + shared.read() + first + bump() + add(&shared) + take()
}`,
    expected: { _tag: 'Completes', result: 58 },
  },
  {
    // FAIL-004: both the protected `i32` and the handler `string` are injected into the recovered
    // `i32 | string`, so the later `match` finds its active member on every engine.
    name: 'catch-union-success-match',
    source: `import silk.effect { Effect }
struct NotFoundError {}
effect fn load(flag: bool) -> i32 ! NotFoundError {
  if flag { fail NotFoundError {} }
  return 5
}
effect fn recover(error: NotFoundError) -> string { return "missing" }
fn handled(flag: bool) -> Effect<i32 | string> {
  return Effect.catch<NotFoundError>(load(flag), recover)
}
pub fn main() -> i32 {
  let a = run handled(true)
  let b = run handled(false)
  let x = match move a { i32 n => n
    string _ => 100 }
  let y = match move b { i32 n => n
    string _ => 100 }
  return x + y
}`,
    expected: { _tag: 'Completes', result: 105 },
  },
  {
    // FAIL-004: a handler whose success type is `never` contributes no success member; its run
    // diverges through the failure channel instead of copying into the recovered success.
    name: 'catch-never-handler',
    source: `import silk.effect { Effect }
struct NotFoundError {}
struct OtherError {}
effect fn load(flag: bool) -> i32 ! NotFoundError {
  if flag { fail NotFoundError {} }
  return 5
}
effect fn rethrow(error: NotFoundError) -> never ! OtherError { fail OtherError {} }
fn handled(flag: bool) -> Effect<i32 ! OtherError> {
  return Effect.catch<NotFoundError>(load(flag), rethrow)
}
effect fn rec(e: OtherError) -> i32 { return 1 }
pub fn main() -> i32 {
  let a = run Effect.catch<OtherError>(handled(true), rec)
  let b = run Effect.catch<OtherError>(handled(false), rec)
  return a * 10 + b
}`,
    expected: { _tag: 'Completes', result: 15 },
  },
  {
    // EFF-013: distinct return sites join under the declared `once Effect<i32>`; each call runs
    // the alternative its own branch constructed.
    name: 'effect-return-site-join-once',
    source: `struct Token { value: i32 }
impl Drop for Token {
  fn drop(self: &mut Token) -> () { return () }
}
effect fn withToken(t: Token) -> i32 { return t.value }
fn choose(flag: bool) -> once Effect<i32> {
  if flag {
    let t = Token { value: 1 }
    return withToken(move t)
  }
  return effect { return 2 }
}
pub fn main() -> i32 {
  let a = run choose(true)
  let b = run choose(false)
  return a * 10 + b
}`,
    expected: { _tag: 'Completes', result: 12 },
  },
  {
    // EFF-007: a fail-only `effect {}` block has success type `never` and still carries its
    // declared failure through Effect.catch.
    name: 'effect-block-fail-only',
    source: `import silk.effect { Effect }
struct ProblemError {}
fn fallible() -> Effect<i32 ! ProblemError> {
  return effect { fail ProblemError {} }
}
effect fn recover(e: ProblemError) -> i32 { return 7 }
pub fn main() -> i32 {
  return run Effect.catch<ProblemError>(fallible(), recover)
}`,
    expected: { _tag: 'Completes', result: 7 },
  },
]

/**
 * Native-only extensions to the shared corpus. These programs retain the optimized single
 * compile/link loop for target-specific claims.
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

const deterministicSecureRandom = `import silk.effect { Effect }
import silk.random { Random }
import silk.u8 as u8
import silk.usize as usize
struct FixedRandom {}
effect fn fixedFill(self: &mut FixedRandom, output: &mut [u8]) -> () {
  let mut index = usize.ZERO
  while index < output.length {
    output[index] = u8.toU8(7)
    index = index + usize.ONE
  }
  return ()
}
impl Random for FixedRandom { fillBytes: FixedRandom.fixedFill }
pub fn main() -> i32 {
  let mut provider = FixedRandom {}
  let mut output = [u8.toU8(0), u8.toU8(0), u8.toU8(0)]
  run Effect.provideMut(Random.fillBytes(&mut output), &mut provider)
  if output[0] != 7 { return 1 }
  if output[1] != 7 { return 2 }
  if output[2] != 7 { return 3 }
  return 42
}`

const nativeSecureRandom = `import silk.effect { Effect }
import silk.os_random { OsRandom }
import silk.random { Random }
import silk.u8 as u8
pub fn main() -> i32 {
  let mut provider = OsRandom.make()
  let mut output = [u8.toU8(0), u8.toU8(0), u8.toU8(0)]
  run Effect.provideMut(Random.fillBytes(&mut output), &mut provider)
  return 42
}`

/**
 * One C function per admitted foreign scalar class plus a void function observed through a second
 * query, so the native leg exercises every C ABI lane shape the classifier can select.
 */
export const foreignScalarFixture = `#include <stdint.h>
#include <stddef.h>
int8_t silk_test_add_i8(int8_t a, int8_t b) { return (int8_t)(a + b); }
uint8_t silk_test_add_u8(uint8_t a, uint8_t b) { return (uint8_t)(a + b); }
int16_t silk_test_add_i16(int16_t a, int16_t b) { return (int16_t)(a + b); }
uint16_t silk_test_add_u16(uint16_t a, uint16_t b) { return (uint16_t)(a + b); }
int32_t silk_test_add_i32(int32_t a, int32_t b) { return a + b; }
uint32_t silk_test_add_u32(uint32_t a, uint32_t b) { return a + b; }
int64_t silk_test_add_i64(int64_t a, int64_t b) { return a + b; }
uint64_t silk_test_add_u64(uint64_t a, uint64_t b) { return a + b; }
intptr_t silk_test_add_isize(intptr_t a, intptr_t b) { return a + b; }
size_t silk_test_add_usize(size_t a, size_t b) { return a + b; }
float silk_test_scale_f32(float value, float factor) { return value * factor; }
double silk_test_scale_f64(double value, double factor) { return value * factor; }
static int32_t silk_test_counter = 0;
void silk_test_touch(void) { silk_test_counter += 1; }
int32_t silk_test_touched(void) { return silk_test_counter; }
`

/** The extern leg of the scalar fixture; `foreignScalarReference` computes the same checksum. */
export const foreignScalarNative = `import silk.i8 as i8
import silk.u8 as u8
import silk.i16 as i16
import silk.u16 as u16
import silk.i32 as i32
import silk.u32 as u32
import silk.i64 as i64
import silk.u64 as u64
import silk.isize as isize
import silk.usize as usize
import silk.f32 as f32
import silk.f64 as f64
unsafe extern "C" fn silk_test_add_i8(a: i8, b: i8) -> i8
unsafe extern "C" fn silk_test_add_u8(a: u8, b: u8) -> u8
unsafe extern "C" fn silk_test_add_i16(a: i16, b: i16) -> i16
unsafe extern "C" fn silk_test_add_u16(a: u16, b: u16) -> u16
unsafe extern "C" fn silk_test_add_i32(a: i32, b: i32) -> i32
unsafe extern "C" fn silk_test_add_u32(a: u32, b: u32) -> u32
unsafe extern "C" fn silk_test_add_i64(a: i64, b: i64) -> i64
unsafe extern "C" fn silk_test_add_u64(a: u64, b: u64) -> u64
unsafe extern "C" fn silk_test_add_isize(a: isize, b: isize) -> isize
unsafe extern "C" fn silk_test_add_usize(a: usize, b: usize) -> usize
unsafe extern "C" fn silk_test_scale_f32(value: f32, factor: f32) -> f32
unsafe extern "C" fn silk_test_scale_f64(value: f64, factor: f64) -> f64
unsafe extern "C" fn silk_test_touch() -> ()
unsafe extern "C" fn silk_test_touched() -> i32
pub fn main() -> i32 {
  let mut sum = 0
  sum = sum + i8.toI32(unsafe silk_test_add_i8(i32.toI8(-7), i32.toI8(3)))
  sum = sum + u8.toI32(unsafe silk_test_add_u8(i32.toU8(200), i32.toU8(5)))
  sum = sum + i16.toI32(unsafe silk_test_add_i16(i32.toI16(-300), i32.toI16(50)))
  sum = sum + u16.toI32(unsafe silk_test_add_u16(i32.toU16(60000), i32.toU16(7)))
  sum = sum + unsafe silk_test_add_i32(-11, 4)
  sum = sum + u32.toI32(unsafe silk_test_add_u32(i32.toU32(9), i32.toU32(13)))
  sum = sum + i64.toI32(unsafe silk_test_add_i64(i32.toI64(-21), i32.toI64(6)))
  sum = sum + u64.toI32(unsafe silk_test_add_u64(i32.toU64(17), i32.toU64(23)))
  sum = sum + isize.toI32(unsafe silk_test_add_isize(i32.toIsize(-5), i32.toIsize(2)))
  sum = sum + usize.toI32(unsafe silk_test_add_usize(i32.toUsize(8), i32.toUsize(9)))
  sum = sum + f32.toI32(unsafe silk_test_scale_f32(i32.toF32(3), i32.toF32(7)))
  sum = sum + f64.toI32(unsafe silk_test_scale_f64(i32.toF64(-4), i32.toF64(5)))
  unsafe silk_test_touch()
  unsafe silk_test_touch()
  unsafe silk_test_touch()
  sum = sum + unsafe silk_test_touched() * 10
  return i32.remainder(sum, 256)
}`

/** Pure-Silk control for `foreignScalarNative`: the same checksum without C. */
export const foreignScalarReference = `import silk.i8 as i8
import silk.u8 as u8
import silk.i16 as i16
import silk.u16 as u16
import silk.i32 as i32
import silk.u32 as u32
import silk.i64 as i64
import silk.u64 as u64
import silk.isize as isize
import silk.usize as usize
import silk.f32 as f32
import silk.f64 as f64
fn addI8(a: i8, b: i8) -> i8 { return a + b }
fn addU8(a: u8, b: u8) -> u8 { return a + b }
fn addI16(a: i16, b: i16) -> i16 { return a + b }
fn addU16(a: u16, b: u16) -> u16 { return a + b }
fn addI32(a: i32, b: i32) -> i32 { return a + b }
fn addU32(a: u32, b: u32) -> u32 { return a + b }
fn addI64(a: i64, b: i64) -> i64 { return a + b }
fn addU64(a: u64, b: u64) -> u64 { return a + b }
fn addIsize(a: isize, b: isize) -> isize { return a + b }
fn addUsize(a: usize, b: usize) -> usize { return a + b }
fn scaleF32(value: f32, factor: f32) -> f32 { return value * factor }
fn scaleF64(value: f64, factor: f64) -> f64 { return value * factor }
pub fn main() -> i32 {
  let mut sum = 0
  sum = sum + i8.toI32(addI8(i32.toI8(-7), i32.toI8(3)))
  sum = sum + u8.toI32(addU8(i32.toU8(200), i32.toU8(5)))
  sum = sum + i16.toI32(addI16(i32.toI16(-300), i32.toI16(50)))
  sum = sum + u16.toI32(addU16(i32.toU16(60000), i32.toU16(7)))
  sum = sum + addI32(-11, 4)
  sum = sum + u32.toI32(addU32(i32.toU32(9), i32.toU32(13)))
  sum = sum + i64.toI32(addI64(i32.toI64(-21), i32.toI64(6)))
  sum = sum + u64.toI32(addU64(i32.toU64(17), i32.toU64(23)))
  sum = sum + isize.toI32(addIsize(i32.toIsize(-5), i32.toIsize(2)))
  sum = sum + usize.toI32(addUsize(i32.toUsize(8), i32.toUsize(9)))
  sum = sum + f32.toI32(scaleF32(i32.toF32(3), i32.toF32(7)))
  sum = sum + f64.toI32(scaleF64(i32.toF64(-4), i32.toF64(5)))
  let mut touched = 0
  touched = touched + 1
  touched = touched + 1
  touched = touched + 1
  sum = sum + touched * 10
  return i32.remainder(sum, 256)
}`

/** C writes through a Silk-formed `*mut i32` and fills a Silk-formed `*mut u8` buffer. */
export const foreignPointerFixture = `#include <stdint.h>
#include <stddef.h>
void silk_test_store(int32_t *out, int32_t value) { *out = value; }
void silk_test_fill(uint8_t *buffer, size_t length, uint8_t byte) {
  for (size_t index = 0; index < length; index += 1) buffer[index] = byte;
}
`

/** `Pointer.fromMutRef(&mut result)` handed to C; the Silk read afterwards observes the store. */
export const foreignPointerStoreNative = `import silk.pointer { Pointer }
unsafe extern "C" fn silk_test_store(out: *mut i32, value: i32) -> ()
pub fn main() -> i32 {
  let mut result = 0
  unsafe silk_test_store(Pointer.fromMutRef(&mut result), 42)
  return result
}`

/** `Pointer.fromMutSlice(&mut bytes)` plus `bytes.length` handed to C; `bytes[0]` observes the fill. */
export const foreignPointerFillNative = `import silk.i32 as i32
import silk.u8 as u8
import silk.pointer { Pointer }
unsafe extern "C" fn silk_test_fill(buffer: *mut u8, length: usize, byte: u8) -> ()
fn fill(bytes: &mut [u8], byte: u8) -> () {
  unsafe silk_test_fill(Pointer.fromMutSlice(&mut bytes), bytes.length, byte)
}
pub fn main() -> i32 {
  let mut bytes = [i32.toU8(0), i32.toU8(0), i32.toU8(0)]
  fill(&mut bytes, i32.toU8(42))
  return u8.toI32(bytes[0]) + u8.toI32(bytes[2]) - 42
}`

/** Pure-Silk control for `foreignPointerFillNative`. */
export const foreignPointerFillReference = `import silk.i32 as i32
import silk.u8 as u8
import silk.usize as usize
fn fill(bytes: &mut [u8], byte: u8) -> () {
  let mut index = usize.ZERO
  while index < bytes.length {
    bytes[index] = byte
    index = index + 1
  }
}
pub fn main() -> i32 {
  let mut bytes = [i32.toU8(0), i32.toU8(0), i32.toU8(0)]
  fill(&mut bytes, i32.toU8(42))
  return u8.toI32(bytes[0]) + u8.toI32(bytes[2]) - 42
}`

/** A libc call writes through a pointer to a C-layout record and Silk reads the populated fields. */
export const foreignClockRecordNative = `import silk.pointer { Pointer }
extern "C" struct Timespec {
  seconds: i64
  nanoseconds: i64
}
unsafe extern "C" fn clock_gettime(clock: i32, value: *mut Timespec) -> i32
pub fn main() -> i32 {
  let mut value = Timespec { seconds: -1, nanoseconds: -1 }
  if unsafe clock_gettime(0, Pointer.fromMutRef(&mut value)) != 0 { return 1 }
  if value.seconds < 0 { return 2 }
  if value.nanoseconds < 0 { return 3 }
  if value.nanoseconds >= 1000000000 { return 4 }
  return 0
}`

/**
 * The libc round trip: allocate, copy a Silk byte view in, NUL-terminate, compare, write to
 * standard output, free. The backend declares `malloc` as `ptr(i64)`, `free` as `void(ptr)`, and
 * `memcmp` as `i32(ptr, ptr, i64)` on a 64-bit host (`NativeProgram.ts`); these externs classify
 * to exactly those LLVM types (`*mut`/`*const` -> `ptr`, `usize` -> `i64`, `()` -> `void`) so the
 * redeclarations share one entry instead of reporting `ForeignSymbolConflict`.
 */
export const foreignLibcRoundtripNative = `import silk.i32 as i32
import silk.isize as isize
import silk.usize as usize
import silk.pointer { Pointer }
unsafe extern "C" fn malloc(size: usize) -> *mut u8
unsafe extern "C" fn free(pointer: *mut u8) -> ()
unsafe extern "C" fn memcpy(destination: *mut u8, source: *const u8, length: usize) -> *mut u8
unsafe extern "C" fn memcmp(left: *const u8, right: *const u8, length: usize) -> i32
unsafe extern "C" fn strlen(text: *const u8) -> usize
unsafe extern "C" fn write(descriptor: i32, data: *const u8, length: usize) -> isize
pub fn main() -> i32 {
  let bytes = b"hello\\n"
  let length = bytes.length
  let buffer = unsafe malloc(length + 1)
  if Pointer.isNull(buffer) { return 1 }
  unsafe {
    let copied = memcpy(buffer, Pointer.fromSlice(bytes), length)
    Pointer.write(Pointer.offsetMut(buffer, length), i32.toU8(0))
    if memcmp(buffer, Pointer.fromSlice(bytes), length) != 0 { return 2 }
    if strlen(buffer) != length { return 3 }
    if isize.toI32(write(1, buffer, length)) != usize.toI32(length) { return 4 }
    free(buffer)
  }
  return usize.toI32(length) * 7
}`

/** Pure-Silk control for `foreignLibcRoundtripNative`: the same status without libc. */
export const foreignLibcRoundtripReference = `import silk.usize as usize
pub fn main() -> i32 {
  let bytes = b"hello\\n"
  return usize.toI32(bytes.length) * 7
}`

/** C calls back into an exported Silk function from a Silk-called C function. */
export const foreignExportRoundtripFixture = `#include <stdint.h>
int32_t silk_test_double_v1(int32_t);
int32_t silk_test_roundtrip(int32_t value) { return silk_test_double_v1(value) + 1; }
`

export const foreignExportRoundtripNative = `unsafe extern "C" fn silk_test_roundtrip(value: i32) -> i32
export "C" fn silk_test_double_v1(value: i32) -> i32 { return value * 2 }
pub fn main() -> i32 { return unsafe silk_test_roundtrip(20) }`

export const foreignExportRoundtripReference = `fn double(value: i32) -> i32 { return value * 2 }
fn roundtrip(value: i32) -> i32 { return double(value) + 1 }
pub fn main() -> i32 { return roundtrip(20) }`

/**
 * C calls one export per admitted scalar class plus a void export whose effect (a call back into
 * the fixture's counter) is observed through a second export; the checksum equals
 * `foreignScalarReference`.
 */
export const foreignExportScalarFixture = `#include <stdint.h>
#include <stddef.h>
int8_t silk_test_export_add_i8(int8_t, int8_t);
uint8_t silk_test_export_add_u8(uint8_t, uint8_t);
int16_t silk_test_export_add_i16(int16_t, int16_t);
uint16_t silk_test_export_add_u16(uint16_t, uint16_t);
int32_t silk_test_export_add_i32(int32_t, int32_t);
uint32_t silk_test_export_add_u32(uint32_t, uint32_t);
int64_t silk_test_export_add_i64(int64_t, int64_t);
uint64_t silk_test_export_add_u64(uint64_t, uint64_t);
intptr_t silk_test_export_add_isize(intptr_t, intptr_t);
size_t silk_test_export_add_usize(size_t, size_t);
float silk_test_export_scale_f32(float, float);
double silk_test_export_scale_f64(double, double);
void silk_test_export_touch(void);
int32_t silk_test_export_touched(void);
static int32_t silk_test_export_counter = 0;
void silk_test_export_note(void) { silk_test_export_counter += 1; }
int32_t silk_test_export_noted(void) { return silk_test_export_counter; }
int32_t silk_test_export_checksum(void) {
  int32_t sum = 0;
  sum += silk_test_export_add_i8(-7, 3);
  sum += silk_test_export_add_u8(200, 5);
  sum += silk_test_export_add_i16(-300, 50);
  sum += silk_test_export_add_u16(60000, 7);
  sum += silk_test_export_add_i32(-11, 4);
  sum += (int32_t)silk_test_export_add_u32(9, 13);
  sum += (int32_t)silk_test_export_add_i64(-21, 6);
  sum += (int32_t)silk_test_export_add_u64(17, 23);
  sum += (int32_t)silk_test_export_add_isize(-5, 2);
  sum += (int32_t)silk_test_export_add_usize(8, 9);
  sum += (int32_t)silk_test_export_scale_f32(3.0f, 7.0f);
  sum += (int32_t)silk_test_export_scale_f64(-4.0, 5.0);
  silk_test_export_touch();
  silk_test_export_touch();
  silk_test_export_touch();
  sum += silk_test_export_touched() * 10;
  return ((sum % 256) + 256) % 256;
}
`

export const foreignExportScalarNative = `import silk.i8 as i8
import silk.u8 as u8
import silk.i16 as i16
import silk.u16 as u16
import silk.i32 as i32
import silk.u32 as u32
import silk.i64 as i64
import silk.u64 as u64
import silk.isize as isize
import silk.usize as usize
import silk.f32 as f32
import silk.f64 as f64
unsafe extern "C" fn silk_test_export_note() -> ()
unsafe extern "C" fn silk_test_export_noted() -> i32
unsafe extern "C" fn silk_test_export_checksum() -> i32
export "C" fn silk_test_export_add_i8(a: i8, b: i8) -> i8 { return a + b }
export "C" fn silk_test_export_add_u8(a: u8, b: u8) -> u8 { return a + b }
export "C" fn silk_test_export_add_i16(a: i16, b: i16) -> i16 { return a + b }
export "C" fn silk_test_export_add_u16(a: u16, b: u16) -> u16 { return a + b }
export "C" fn silk_test_export_add_i32(a: i32, b: i32) -> i32 { return a + b }
export "C" fn silk_test_export_add_u32(a: u32, b: u32) -> u32 { return a + b }
export "C" fn silk_test_export_add_i64(a: i64, b: i64) -> i64 { return a + b }
export "C" fn silk_test_export_add_u64(a: u64, b: u64) -> u64 { return a + b }
export "C" fn silk_test_export_add_isize(a: isize, b: isize) -> isize { return a + b }
export "C" fn silk_test_export_add_usize(a: usize, b: usize) -> usize { return a + b }
export "C" fn silk_test_export_scale_f32(value: f32, factor: f32) -> f32 { return value * factor }
export "C" fn silk_test_export_scale_f64(value: f64, factor: f64) -> f64 { return value * factor }
export "C" fn silk_test_export_touch() -> () {
  unsafe silk_test_export_note()
  return ()
}
export "C" fn silk_test_export_touched() -> i32 { return unsafe silk_test_export_noted() }
pub fn main() -> i32 { return unsafe silk_test_export_checksum() }`

/** Replacement cleanup: a displaced local, field, array element, and slice element clean once. */
export const replaceCleanupProgram = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.shared { Shared }
import silk.format { Format }
import silk.writer { Writer, WriterError }

struct Log {
  slots: [i32; 16]
  count: usize
}
struct Tracer {
  id: i32
  log: Shared<Log>
}
fn record(log: &mut Log, id: i32) -> i32 {
  log.slots[log.count] = id
  log.count = log.count + 1
  return 0
}
impl Drop for Tracer {
  fn drop(self: &mut Tracer) -> () {
    let id = self.id
    let r = Shared.withMut<Log, i32>(&self.log, record(id))
    return ()
  }
}
fn encode(log: &Log) -> i32 {
  let mut r = 0
  let mut i: usize = 0
  while i < log.count {
    r = r * 10 + log.slots[i]
    i = i + 1
  }
  return r
}
fn tracer(id: i32, log: &Shared<Log>) -> Tracer {
  return Tracer { id: id, log: Shared.clone<Log>(log) }
}
effect fn printLog(log: &Shared<Log>) -> () ! WriterError {
  let code = Shared.with<Log, i32>(log, encode)
  let mut writer = Writer.stdoutWriterProvider()
  return run (Format.display(&code) |> Effect.provideMut<Writer>(&mut writer))
}
effect fn makeLog() -> Shared<Log> ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run (Shared.make<Log>(Log { slots: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], count: 0 }) |> Effect.provideMut<Allocator>(&mut allocator))
}
effect fn recoverAny(e: OutOfMemoryError | WriterError) -> i32 { return -1 }
struct Pair {
  first: Tracer
  second: Tracer
}
fn localReplace(log: &Shared<Log>) -> i32 {
  let mut a = tracer(1, log)
  a = tracer(2, log)
  return 0
}
fn fieldReplace(log: &Shared<Log>) -> i32 {
  let mut p = Pair { first: tracer(3, log), second: tracer(4, log) }
  p.first = tracer(5, log)
  return 0
}
fn arrayReplace(log: &Shared<Log>) -> i32 {
  let mut arr = [tracer(6, log)]
  arr[0] = tracer(7, log)
  return 0
}
fn sliceReplace(values: &mut [Tracer], log: &Shared<Log>) -> i32 {
  values[0] = tracer(9, log)
  return 0
}
fn paramReplace(mut t: Tracer, log: &Shared<Log>) -> i32 {
  t = tracer(9, log)
  return 0
}
effect fn body() -> i32 ! OutOfMemoryError | WriterError {
  let log = run makeLog()
  let a = localReplace(&log)
  let b = fieldReplace(&log)
  let c = arrayReplace(&log)
  let mut arr = [tracer(8, &log)]
  let d = sliceReplace(&mut arr, &log)
  drop arr
  run printLog(&log)
  return 0
}
pub fn main() -> i32 {
  return run Effect.catchAll(body(), recoverAny)
}`

/** Replacement cleanup runs at the write, before an explicit drop and scope exit. */
export const replaceDropProgram = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.shared { Shared }
import silk.format { Format }
import silk.writer { Writer, WriterError }

struct Log {
  slots: [i32; 16]
  count: usize
}
struct Tracer {
  id: i32
  log: Shared<Log>
}
fn record(log: &mut Log, id: i32) -> i32 {
  log.slots[log.count] = id
  log.count = log.count + 1
  return 0
}
impl Drop for Tracer {
  fn drop(self: &mut Tracer) -> () {
    let id = self.id
    let r = Shared.withMut<Log, i32>(&self.log, record(id))
    return ()
  }
}
fn encode(log: &Log) -> i32 {
  let mut r = 0
  let mut i: usize = 0
  while i < log.count {
    r = r * 10 + log.slots[i]
    i = i + 1
  }
  return r
}
fn tracer(id: i32, log: &Shared<Log>) -> Tracer {
  return Tracer { id: id, log: Shared.clone<Log>(log) }
}
effect fn printLog(log: &Shared<Log>) -> () ! WriterError {
  let code = Shared.with<Log, i32>(log, encode)
  let mut writer = Writer.stdoutWriterProvider()
  return run (Format.display(&code) |> Effect.provideMut<Writer>(&mut writer))
}
effect fn makeLog() -> Shared<Log> ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run (Shared.make<Log>(Log { slots: [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0], count: 0 }) |> Effect.provideMut<Allocator>(&mut allocator))
}
effect fn recoverAny(e: OutOfMemoryError | WriterError) -> i32 { return -1 }
fn replaceAndDrop(log: &Shared<Log>) -> i32 {
  let mut a = tracer(1, log)
  let b = tracer(2, log)
  a = tracer(3, log)
  drop b
  let c = tracer(4, log)
  return 0
}
effect fn body() -> i32 ! OutOfMemoryError | WriterError {
  let log = run makeLog()
  let s = replaceAndDrop(&log)
  run printLog(&log)
  return 0
}
pub fn main() -> i32 {
  return run Effect.catchAll(body(), recoverAny)
}`

const algorithmExampleIds = [
  'breadth-first-search',
  'crc-32',
  'fft',
  'game-of-life',
  'matrix-multiplication',
  'quicksort',
  'sieve',
] as const

/** Complete algorithm examples execute as independently pinned native-corpus cases. */
const algorithmExamples: ReadonlyArray<CorpusProgram> = algorithmExampleIds.map((id) => {
  const root = new URL(`../../../../examples/algorithms/${id}/`, import.meta.url)
  const manifest = JSON.parse(readFileSync(new URL('example.json', root), 'utf8')) as {
    readonly source: string
    readonly expected: { readonly entryResult: number }
  }
  return Object.freeze({
    name: `algorithm-${id}`,
    source: readFileSync(new URL(manifest.source, root), 'utf8'),
    expected: Object.freeze({ _tag: 'Completes' as const, result: manifest.expected.entryResult }),
  })
})

const algorithmicFixtureRoot = new URL('../fixtures/algorithmic-acceptance/', import.meta.url)
const algorithmicCompilerFold: CorpusProgram = Object.freeze({
  name: 'algorithmic-compiler-fold',
  source: readFileSync(new URL('app/Main.silk', algorithmicFixtureRoot), 'utf8'),
  nativeImports: Object.freeze({
    'compiler/Coverage': readFileSync(
      new URL('compiler/Coverage.silk', algorithmicFixtureRoot),
      'utf8',
    ),
    'compiler/Member': readFileSync(
      new URL('compiler/Member.silk', algorithmicFixtureRoot),
      'utf8',
    ),
  }),
  expected: Object.freeze({ _tag: 'Completes', result: 42 }),
})

const pressurePrograms: ReadonlyArray<CorpusProgram> = [
  {
    name: 'scanner-owned-token-vector',
    source: readFileSync(
      new URL('../fixtures/scanner-acceptance/Main.silk', import.meta.url),
      'utf8',
    ),
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'language-pressure-lexer',
    source: readFileSync(
      new URL('../../../../examples/language-pressure/lexer/main.silk', import.meta.url),
      'utf8',
    ),
    expected: { _tag: 'Completes', result: 0 },
  },
  {
    name: 'language-pressure-stack-vm',
    source: readFileSync(
      new URL('../../../../examples/language-pressure/stack-vm/main.silk', import.meta.url),
      'utf8',
    ),
    expected: { _tag: 'Completes', result: 0 },
  },
]

export const nativeCorpus: ReadonlyArray<CorpusProgram> = [
  {
    name: 'replace-cleanup',
    source: 'pub fn main() -> i32 { return 0 }',
    nativeSource: replaceCleanupProgram,
    nativeStdout: '123546789',
    expected: { _tag: 'Completes', result: 0 },
  },
  {
    name: 'replace-drop',
    source: 'pub fn main() -> i32 { return 0 }',
    nativeSource: replaceDropProgram,
    nativeStdout: '1243',
    expected: { _tag: 'Completes', result: 0 },
  },
  ...corpus,
  ...algorithmExamples,
  algorithmicCompilerFold,
  ...pressurePrograms,
  {
    name: 'foreign-export-roundtrip',
    source: foreignExportRoundtripReference,
    nativeSource: foreignExportRoundtripNative,
    nativeCSources: { silk_test_foreign_export_roundtrip: foreignExportRoundtripFixture },
    expected: { _tag: 'Completes', result: 41 },
  },
  {
    name: 'foreign-export-scalars',
    source: foreignScalarReference,
    nativeSource: foreignExportScalarNative,
    nativeCSources: { silk_test_foreign_export_scalars: foreignExportScalarFixture },
    expected: { _tag: 'Completes', result: 139 },
  },
  {
    name: 'foreign-scalar-fixture',
    source: foreignScalarReference,
    nativeSource: foreignScalarNative,
    nativeCSources: { silk_test_foreign_scalars: foreignScalarFixture },
    expected: { _tag: 'Completes', result: 139 },
  },
  {
    name: 'foreign-libc-abs',
    source: `fn magnitude(value: i32) -> i32 { if value < 0 { return 0 - value } return value }
pub fn main() -> i32 { return magnitude(-42) }`,
    nativeSource: `unsafe extern "C" fn abs(value: i32) -> i32
pub fn main() -> i32 { return unsafe abs(-42) }`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'foreign-libc-qsort-callback',
    source: 'pub fn main() -> i32 { return 42 }',
    nativeSource: `import silk.pointer { Pointer }
unsafe extern "C" fn qsort(base: *mut i32, count: usize, size: usize, compare: extern "C" fn(*const i32, *const i32) -> i32) -> ()
export "C" fn compare(left: *const i32, right: *const i32) -> i32 {
  let leftValue = unsafe Pointer.read(left)
  let rightValue = unsafe Pointer.read(right)
  if leftValue < rightValue { return -1 }
  if leftValue > rightValue { return 1 }
  return 0
}
pub fn main() -> i32 {
  let mut values = [4, 2]
  unsafe qsort(Pointer.fromMutSlice(&mut values), 2, 4, compare)
  return values[1] * 10 + values[0]
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'foreign-libc-environ-static',
    source: 'pub fn main() -> i32 { return 42 }',
    nativeSource: `import silk.pointer { Pointer }
unsafe extern "C" static environment: *mut *mut u8 as "environ"
pub fn main() -> i32 {
  unsafe { if Pointer.isNull(environment) { return 1 } }
  return 42
}`,
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'foreign-object-libm-order',
    source: 'pub fn main() -> i32 { return 42 }',
    nativeSource: `import silk.i32 as i32
unsafe extern "C" fn silk_test_libm_order(value: f64) -> i32
pub fn main() -> i32 { return unsafe silk_test_libm_order(i32.toF64(85)) }`,
    nativeCSources: {
      silk_test_libm_order: `#include <stdint.h>
#include <math.h>
int32_t silk_test_libm_order(double value) { return (int32_t)fmod(value, 43.0); }
`,
    },
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'pointer-foreign-store',
    source: pointerParameterWrite,
    nativeSource: foreignPointerStoreNative,
    nativeCSources: { silk_test_foreign_pointers: foreignPointerFixture },
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'pointer-foreign-fill',
    source: foreignPointerFillReference,
    nativeSource: foreignPointerFillNative,
    nativeCSources: { silk_test_foreign_pointers: foreignPointerFixture },
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'foreign-c-layout-clock',
    source: 'pub fn main() -> i32 { return 0 }',
    nativeSource: foreignClockRecordNative,
    expected: { _tag: 'Completes', result: 0 },
  },
  {
    name: 'pointer-libc-roundtrip',
    source: foreignLibcRoundtripReference,
    nativeSource: foreignLibcRoundtripNative,
    nativeStdout: 'hello\n',
    expected: { _tag: 'Completes', result: 42 },
  },
  {
    name: 'recovered-provided-write',
    source: 'pub fn main() -> i32 { return 0 }',
    nativeSource: recoveredProvidedWrite,
    nativeImports: { recovered_writer: recoveredWriterModule },
    nativeStdout: 'Hello',
    expected: { _tag: 'Completes', result: 0 },
  },
  {
    name: 'secure-random-provider',
    source: deterministicSecureRandom,
    nativeSource: nativeSecureRandom,
    expected: { _tag: 'Completes', result: 42 },
  },
  // These two canonical programs cover the public single-threaded Fiber story through the shared
  // native corpus: root/fork/join, FIFO siblings, repeated yield, nested forks,
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
  {
    name: 'scheduler-fiber-timers',
    source: schedulerFiber('local-scheduler-timer-basic'),
    nativeSource: schedulerFiber('local-scheduler-timers-native'),
    expected: { _tag: 'Completes', result: 42 },
  },
  ...[
    { name: 'first-activation', result: 21 },
    { name: 'coroutine', result: 123 },
    { name: 'dormant-cancel', result: 1111 },
    { name: 'selective-ready', result: 22 },
    { name: 'timer', result: 42 },
  ].map((program): CorpusProgram => ({
    name: `independent-execution-separation-${program.name}`,
    source: independentExecutionPressure(program.name),
    expected: { _tag: 'Completes', result: program.result },
  })),
  ...[
    { name: 'timer', source: 'timer', result: 42 },
    { name: 'coroutine', source: 'coroutine', result: 123 },
    { name: 'selective-ready', source: 'selective-ready', result: 22 },
  ].map((program): CorpusProgram => ({
    name: `independent-execution-separation-renamed-${program.name}`,
    source: renameIndependentPolicy(independentExecutionPressure(program.source)),
    expected: { _tag: 'Completes', result: program.result },
  })),
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
    name: 'frame-retained-execution-completion',
    source: frameRetainedExecutionCompletion,
    expected: { _tag: 'Completes', result: 0 },
  },
  {
    name: 'frame-retained-execution-abandoned-frame',
    source: frameRetainedExecutionAbandonedFrame,
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
  ...([0, 1] as const).map((ordinal): CorpusProgram => ({
    name: `local-shared-pressure-quota-${ordinal}`,
    source: localSharedPressureFailure(ordinal),
    expected: { _tag: 'Completes', result: 142 },
  })),
  ...(['with', 'withMut'] as const).flatMap((outer): ReadonlyArray<CorpusProgram> =>
    (['with', 'withMut'] as const).map((inner): CorpusProgram => {
      const outerReference = outer === 'with' ? '&Counter' : '&mut Counter'
      const innerCallback = inner === 'with' ? 'read' : 'increment'
      return {
        name: `local-shared-conflict-${outer}-${inner}`,
        source: `import silk.allocator { Allocator }
import silk.allocator { Allocator, OutOfMemoryError, SystemAllocator }
import silk.effect { Effect }
import silk.shared { Shared }
struct Counter { value: i32 }
fn read(value: &Counter) -> i32 { return value.value }
fn increment(value: &mut Counter) -> i32 {
  value.value = value.value + 1
  return value.value
}
fn nested(value: ${outerReference}, alias: Shared<Counter>) -> i32 {
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
import silk.effect { Effect }
import silk.layout { Layout }
import silk.shared { Shared }
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
import silk.effect { Effect }
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
import silk.effect { Effect }
import silk.layout { Layout }
import silk.shared { Shared }
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
import silk.effect { Effect }
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
import silk.effect { Effect }
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
