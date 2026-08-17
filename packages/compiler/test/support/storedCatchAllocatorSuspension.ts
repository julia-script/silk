/** A stored catch wrapper whose outer provider must remain the continuation allocator. */
export const storedCatchAllocatorSuspension = `struct Problem { code: i32 }
effect fn delayed() -> i32 ! Problem | OutOfMemory ? &mut Allocator {
  let value = run Effect.suspend(effect { return 2 })
  if value == 2 { fail Problem { code: 35 } }
  return value
}
effect fn recover(error: Problem | OutOfMemory) -> i32 { return 42 }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  let handled = delayed() |> Effect.catchAll(recover)
  return run (move handled |> Effect.provideMut(&mut allocator))
}`
