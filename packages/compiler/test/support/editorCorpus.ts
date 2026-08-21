/** Shared acceptance programs for compiler-owned editor intelligence. */
export const allocatorSource = `import silk.core { SystemAllocator }
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return 0
}`

export const effectHandlerSource = `import silk.core { OutOfMemoryError }
import silk.effect as Effect
effect fn recover(error: OutOfMemoryError) -> i32 { return 0 }
pub fn main() -> i32 {
  return run Effect.catchAll(store(), recover)
}`

export const pipedCatchSource = `import silk.effect as Effect
struct Problem {}
effect fn recover(error: Problem) -> i32 { return 0 }
pub fn main() -> i32 {
  let recipe = relay(0)
    |> Effect.catchAll(recover)
  return run recipe
}`

export const nestedBindingSource = `pub fn main() -> i32 {
  let value = 1
  if true {
    let value = 2
    let selected = val
  }
  let before = lat
  let later = 3
  return value
}`

export const recoveredMemberSource = `import silk.core { SystemAllocator }
import silk.effect as Effect
pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return Effect.
}`
