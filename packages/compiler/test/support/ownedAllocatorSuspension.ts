export const ownedAllocatorSuspensionSuccess = `import silk.result { Result, Success, Failure }
struct OwnedAllocator { storage: Allocation }
effect fn allocate(self: &mut OwnedAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  let mut inner = SystemAllocator.make()
  return run Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
}
impl Allocator for OwnedAllocator { allocate: OwnedAllocator.allocate }
effect fn open() -> OwnedAllocator ! OutOfMemory ? &mut Allocator {
  let layout = Layout.of<[i32; 2]>()
  let storage = run Allocator.allocate(move layout)
  return OwnedAllocator { storage: move storage }
}
effect fn protected() -> i32 ? &mut Allocator {
  return run Effect.suspend(effect { return 42 })
}
effect fn inspect(self: once Effect<i32 ? &mut Allocator>) -> Result<i32, never>
? &mut Allocator {
  return run Effect.result(move self)
}
effect fn program() -> i32 ! OutOfMemory ? &mut Allocator {
  let provider = run open()
  let inspected = inspect(protected())
  let completed = run Intrinsic.bindRequirementOwned<&mut Allocator>(move inspected, move provider)
  return match move completed {
    Result<i32, never> { value: outcome } => match move outcome {
      Success<i32> { value } => move value
      Failure<never> { error } => move error
    }
  }
}
effect fn outerRecover(error: OutOfMemory) -> i32 { return 9 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catchAll(
    Effect.provideMut(program(), &mut allocator),
    outerRecover,
  )
}`

export const ownedAllocatorSuspensionFailure = `import silk.result { Result, Success, Failure }
struct Problem { code: i32 }
struct OwnedAllocator { storage: Allocation }
effect fn allocate(self: &mut OwnedAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  let mut inner = SystemAllocator.make()
  return run Allocator.allocate(move layout) |> Effect.provideMut(&mut inner)
}
impl Allocator for OwnedAllocator { allocate: OwnedAllocator.allocate }
effect fn open() -> OwnedAllocator ! OutOfMemory ? &mut Allocator {
  let layout = Layout.of<[i32; 2]>()
  let storage = run Allocator.allocate(move layout)
  return OwnedAllocator { storage: move storage }
}
effect fn protected() -> i32 ! Problem ? &mut Allocator {
  let code = run Effect.suspend(effect { return 42 })
  fail Problem { code: code }
}
effect fn recover(error: Problem) -> i32 { return 7 }
effect fn inspect(self: once Effect<i32 ? &mut Allocator>)
-> Result<i32, never>
? &mut Allocator {
  return run Effect.result(move self)
}
effect fn program() -> i32 ! OutOfMemory ? &mut Allocator {
  let provider = run open()
  let inspected = inspect(Intrinsic.catchFailure<Problem>(protected(), recover))
  let completed = run Intrinsic.bindRequirementOwned<&mut Allocator>(move inspected, move provider)
  return match move completed {
    Result<i32, never> { value: outcome } => match move outcome {
      Success<i32> { value } => move value
      Failure<never> { error } => move error
    }
  }
}
effect fn outerRecover(error: OutOfMemory) -> i32 { return 9 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catchAll(
    Effect.provideMut(program(), &mut allocator),
    outerRecover,
  )
}`

/**
 * Both Audit providers conform to Allocator, and one is supplied with exclusive provider strength.
 * Suspension preserves those explicit requirements exactly but never selects either provider for
 * private coroutine storage. The Audit implementation always refuses, so accidental selection is
 * observable on every engine.
 */
export const auditAllocatorSuspension = `import silk.effects as Effect
struct AuditAllocator { tag: i32 }
effect fn allocate(self: &mut AuditAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  fail OutOfMemory {}
}
impl Allocator for AuditAllocator { allocate: AuditAllocator.allocate }
effect fn protected() -> i32
? &Allocator@SharedAudit | &Allocator@ExclusiveAudit {
  return run Effect.suspend(effect { return 42 })
}
pub fn main() -> i32 {
  let sharedAudit = AuditAllocator { tag: 0 }
  let mut exclusiveAudit = AuditAllocator { tag: 0 }
  return run (protected()
    |> Effect.provide<&Allocator@SharedAudit>(&sharedAudit)
    |> Effect.provideMut<&Allocator@ExclusiveAudit>(&mut exclusiveAudit))
}`

export const ownedProviderSuspendedSuccess = `import silk.effects as Effect
service Counter {
  effect fn increment() -> i32 ? &mut Counter
}
struct Cell { value: i32 storage: Allocation }
effect fn increment(self: &mut Cell) -> i32 {
  let value = self.value
  return run Effect.suspend(effect { return value + 1 })
}
impl Counter for Cell { increment: Cell.increment }
effect fn read() -> i32 ? &mut Counter {
  return run Counter.increment()
}
effect fn program() -> i32 ! OutOfMemory ? &mut Allocator {
  let layout = Layout.of<[i32; 2]>()
  let storage = run Allocator.allocate(move layout)
  let cell = Cell { value: 41, storage: move storage }
  let bound = Intrinsic.bindRequirementOwned<&mut Counter>(read(), move cell)
  return run move bound
}
effect fn recover(error: OutOfMemory) -> i32 { return -1 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catchAll(
    Effect.provideMut<&mut Allocator>(program(), &mut allocator),
    recover,
  )
}`

export const ownedProviderSuspendedFailure = `import silk.effects as Effect
struct Problem { code: i32 }
service Value {
  effect fn read() -> i32 ! Problem ? &mut Value
}
struct Provider { code: i32 storage: Allocation }
effect fn read(self: &mut Provider) -> i32 ! Problem {
  let providerCode = self.code
  let code = run Effect.suspend(effect { return providerCode })
  fail Problem { code: code }
}
impl Value for Provider { read: Provider.read }
effect fn open() -> Provider ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let storage = run Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  return Provider { code: 42, storage: move storage }
}
effect fn use() -> i32 ! Problem ? &mut Value {
  return run Value.read()
}
effect fn program() -> i32 ! Problem | OutOfMemory ? &mut Allocator {
  let provider = run open()
  return run Intrinsic.bindRequirementOwned<&mut Value>(use(), move provider)
}
effect fn recover(error: Problem | OutOfMemory) -> i32 { return 7 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return run Effect.catchAll(Effect.provideMut(program(), &mut allocator), recover)
}`

export const mixedServiceProviderSuspension = `import silk.effects as Effect
service Value {
  effect fn read() -> i32 ? &mut Value
}
struct Immediate { value: i32 }
effect fn readImmediate(self: &mut Immediate) -> i32 {
  return self.value
}
impl Value for Immediate { read: Immediate.readImmediate }
struct Delayed { value: i32 }
effect fn readDelayed(self: &mut Delayed) -> i32 {
  let value = self.value
  return run Effect.suspend(effect { return value })
}
impl Value for Delayed { read: Delayed.readDelayed }
effect fn use() -> i32 ? &mut Value {
  return run Value.read()
}
effect fn immediate() -> i32 {
  let mut provider = Immediate { value: 20 }
  return run Intrinsic.bindRequirementMut<&mut Value>(use(), &mut provider)
}
effect fn delayed() -> i32 {
  let mut provider = Delayed { value: 22 }
  return run Intrinsic.bindRequirementMut<&mut Value>(use(), &mut provider)
}
effect fn program() -> i32 {
  return (run immediate()) + (run delayed())
}
pub fn main() -> i32 {
  return run program()
}`
