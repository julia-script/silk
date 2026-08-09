/** Shared acceptance programs for compiler-owned editor intelligence. */
export const allocatorSource = `pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return 0
}`

export const effectHandlerSource = `effect fn recover(error: OutOfMemory) -> i32 { return 0 }
pub fn main() -> i32 {
  return run Effect.catch<OutOfMemory>(store(), recover)
}`

export const pipedCatchSource = `struct Problem {}
effect fn recover(error: Problem) -> i32 { return 0 }
pub fn main() -> i32 {
  let recipe = relay(0)
    |> Effect.catch<Problem>(recover)
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

export const recoveredMemberSource = `pub fn main() -> i32 {
  let mut allocator = SystemAllocator.make()
  return Effect.
}`
