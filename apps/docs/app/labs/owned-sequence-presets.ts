export const vectorGrowthSource = `import silk.allocator {Allocator}

import silk.allocator {SystemAllocator}

import silk.allocator {OutOfMemoryError}

import silk.effect as Effect

import silk.i32

import silk.vector {Vector, make, append, get, length, capacity}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = make<i32>()
  let pending0 = append<i32>(&mut values, 10)
    |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let pending1 = append<i32>(&mut values, 11)
    |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let pending2 = append<i32>(&mut values, 12)
    |> Effect.provideMut(&mut allocator)
  let appended2 = run pending2
  let pending3 = append<i32>(&mut values, 13)
    |> Effect.provideMut(&mut allocator)
  let appended3 = run pending3
  let pending4 = append<i32>(&mut values, 14)
    |> Effect.provideMut(&mut allocator)
  let appended4 = run pending4
  let pending5 = append<i32>(&mut values, 15)
    |> Effect.provideMut(&mut allocator)
  let appended5 = run pending5
  if length<i32>(&values) == 6 {} else {
    return 0
  }
  if capacity<i32>(&values) == 8 {} else {
    return 1
  }
  let first = get<i32>(&values, 0)
  let last = get<i32>(&values, 5)
  return first + last + 17
}

effect fn recover(error: OutOfMemoryError) -> i32 {
  return 7
}

pub fn main() -> i32 {
  return run Effect.catchAll(build(), recover)
}
`

export const vectorFailedGrowthSource = `import silk.allocator {Allocator}

import silk.allocator {SystemAllocator}

import silk.allocator {OutOfMemoryError}

import silk.effect as Effect

import silk.i32

import silk.layout {Layout}

import silk.vector {Vector, make, append, get, length, capacity}

struct QuotaAllocator {
  remaining: i32
}

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  if self.remaining == 0 {
    fail OutOfMemoryError {}
  }
  self.remaining = self.remaining - 1
  let mut inner = Allocator.systemAllocatorProvider()
  let pending = Allocator.allocate(move layout)
    |> Effect.provideMut(&mut inner)
  let block = run pending
  return move block
}

impl Allocator for QuotaAllocator {
  allocate: QuotaAllocator.allocate
}

effect fn grow(values: &mut Vector<i32>) -> i32 ! OutOfMemoryError ? &mut Allocator {
  let appended = run append<i32>(move values, 14)
  return 1
}

effect fn recover(error: OutOfMemoryError) -> i32 {
  return 7
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = QuotaAllocator {remaining: 1}
  let mut values = make<i32>()
  let pending0 = append<i32>(&mut values, 10)
    |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let pending1 = append<i32>(&mut values, 11)
    |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let pending2 = append<i32>(&mut values, 12)
    |> Effect.provideMut(&mut allocator)
  let appended2 = run pending2
  let pending3 = append<i32>(&mut values, 13)
    |> Effect.provideMut(&mut allocator)
  let appended3 = run pending3
  let marker = run grow(&mut values)
    |> Effect.catchAll(recover)
    |> Effect.provideMut(&mut allocator)
  if marker == 7 {} else {
    return 0
  }
  if length<i32>(&values) == 4 {} else {
    return 1
  }
  if capacity<i32>(&values) == 4 {} else {
    return 2
  }
  let first = get<i32>(&values, 0)
  let last = get<i32>(&values, 3)
  return first + last + 19
}

effect fn outerRecover(error: OutOfMemoryError) -> i32 {
  return 0
}

pub fn main() -> i32 {
  return run Effect.catchAll(build(), outerRecover)
}
`

export const vectorDestructionOrderSource = `import silk.allocator {Allocator}

import silk.allocator {SystemAllocator}

import silk.allocator {OutOfMemoryError}

import silk.effect as Effect

import silk.i32

import silk.vector {Vector, make, append, capacity}

struct Entry {
  value: i32
  marker: Vector<i32>
}

fn record(value: i32) -> () {
  return ()
}

impl Drop for Entry {
  fn drop(self: &mut Entry) -> () {
    return record(self.value)
  }
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = make<Entry>()
  let entry0 = Entry {value: 3, marker: make<i32>()}
  let pending0 = append<Entry>(&mut values, move entry0)
    |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let entry1 = Entry {value: 5, marker: make<i32>()}
  let pending1 = append<Entry>(&mut values, move entry1)
    |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let entry2 = Entry {value: 7, marker: make<i32>()}
  let pending2 = append<Entry>(&mut values, move entry2)
    |> Effect.provideMut(&mut allocator)
  let appended2 = run pending2
  if capacity<Entry>(&values) == 4 {} else {
    return 0
  }
  return 42
}

effect fn recover(error: OutOfMemoryError) -> i32 {
  return 7
}

pub fn main() -> i32 {
  return run Effect.catchAll(build(), recover)
}
`

export const vectorEarlyDropSource = `import silk.allocator {Allocator}

import silk.allocator {SystemAllocator}

import silk.allocator {OutOfMemoryError}

import silk.effect as Effect

import silk.i32

import silk.vector {Vector, make, append}

struct Entry {
  value: i32
  marker: Vector<i32>
}

fn record(value: i32) -> () {
  return ()
}

impl Drop for Entry {
  fn drop(self: &mut Entry) -> () {
    return record(self.value)
  }
}

fn consume(values: Vector<Entry>) -> i32 {
  drop values
  return 40
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut values = make<Entry>()
  let entry0 = Entry {value: 11, marker: make<i32>()}
  let pending0 = append<Entry>(&mut values, move entry0)
    |> Effect.provideMut(&mut allocator)
  let appended0 = run pending0
  let entry1 = Entry {value: 13, marker: make<i32>()}
  let pending1 = append<Entry>(&mut values, move entry1)
    |> Effect.provideMut(&mut allocator)
  let appended1 = run pending1
  let consumed = consume(move values)
  return consumed + 2
}

effect fn recover(error: OutOfMemoryError) -> i32 {
  return 7
}

pub fn main() -> i32 {
  return run Effect.catchAll(build(), recover)
}
`

export const scannerSource = `import silk.allocator {Allocator}

import silk.allocator {OutOfMemoryError}

import silk.allocator {SystemAllocator}

import silk.effect as Effect

import silk.usize

import silk.vector {Vector, make, append, get, length, capacity}

struct U8 {
  value: i32
}

struct Token {
  kind: i32
}

impl Copy for Token {}

fn observe(kind: i32) -> i32 {
  return kind
}

effect fn scan(source: &[U8]) -> Vector<Token> ! OutOfMemoryError ? &mut Allocator {
  let mut tokens = make<Token>()
  let mut index = usize.add(0, 0)
  while index < source.length {
    let byte = source[index].value
    let mut kind = 3
    if byte == 1 {
      kind = 1
    }
    if byte == 2 {
      kind = 2
    }
    let token = Token {kind: kind}
    let appended = run append<Token>(&mut tokens, move token)
    index = index + 1
  }
  return move tokens
}

effect fn build() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let source = [
    U8 {value: 1},
    U8 {value: 2},
    U8 {value: 3},
    U8 {value: 1},
    U8 {value: 2},
    U8 {value: 3},
    U8 {value: 1},
    U8 {value: 2},
    U8 {value: 3},
    U8 {value: 1},
  ]
  let pending = scan(&source)
    |> Effect.provideMut(&mut allocator)
  let mut tokens = run pending
  if length<Token>(&tokens) == 10 {} else {
    return 0
  }
  if capacity<Token>(&tokens) == 16 {} else {
    return 1
  }
  let token0 = get<Token>(&tokens, 0)
  let token1 = get<Token>(&tokens, 1)
  let token2 = get<Token>(&tokens, 2)
  let token3 = get<Token>(&tokens, 3)
  let token4 = get<Token>(&tokens, 4)
  let token5 = get<Token>(&tokens, 5)
  let token6 = get<Token>(&tokens, 6)
  let token7 = get<Token>(&tokens, 7)
  let token8 = get<Token>(&tokens, 8)
  let token9 = get<Token>(&tokens, 9)
  let kind0 = observe(token0.kind)
  let kind1 = observe(token1.kind)
  let kind2 = observe(token2.kind)
  let kind3 = observe(token3.kind)
  let kind4 = observe(token4.kind)
  let kind5 = observe(token5.kind)
  let kind6 = observe(token6.kind)
  let kind7 = observe(token7.kind)
  let kind8 = observe(token8.kind)
  let kind9 = observe(token9.kind)
  return kind0 + kind1 + kind2 + kind3 + kind4 + kind5 + kind6 + kind7 + kind8 + kind9 + 23
}

effect fn recover(error: OutOfMemoryError) -> i32 {
  return 7
}

pub fn main() -> i32 {
  return run Effect.catchAll(build(), recover)
}
`
