export const vectorGrowthSource = `import silk.vector { Vector, make, append, get, length, capacity }

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut values = make<I32>()
  let pending0 = append<I32>(&mut values, 10) |> Allocator.provide(&mut allocator)
  let appended0 = run pending0
  let pending1 = append<I32>(&mut values, 11) |> Allocator.provide(&mut allocator)
  let appended1 = run pending1
  let pending2 = append<I32>(&mut values, 12) |> Allocator.provide(&mut allocator)
  let appended2 = run pending2
  let pending3 = append<I32>(&mut values, 13) |> Allocator.provide(&mut allocator)
  let appended3 = run pending3
  let pending4 = append<I32>(&mut values, 14) |> Allocator.provide(&mut allocator)
  let appended4 = run pending4
  let pending5 = append<I32>(&mut values, 15) |> Allocator.provide(&mut allocator)
  let appended5 = run pending5
  if length<I32>(&values) == 6 {} else { return 0 }
  if capacity<I32>(&values) == 8 {} else { return 1 }
  let first = get<I32>(&mut values, 0)
  let last = get<I32>(&mut values, 5)
  return first + last + 17
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }
`

export const vectorFailedGrowthSource = `import silk.vector { Vector, make, append, get, length, capacity }

struct QuotaAllocator { remaining: I32 }

effect fn allocate(self: &mut QuotaAllocator, layout: Layout) -> Allocation ! OutOfMemory {
  if self.remaining == 0 { fail OutOfMemory {} }
  self.remaining = self.remaining - 1
  let mut inner = SystemAllocator.make()
  let pending = Allocator.allocate(move layout) |> Allocator.provide(&mut inner)
  let block = run pending
  return move block
}

impl Allocator for QuotaAllocator { allocate: QuotaAllocator.allocate }

effect fn grow(values: &mut Vector<I32>) -> I32 ! OutOfMemory ? &mut Allocator {
  let appended = run append<I32>(move values, 14)
  return 1
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = QuotaAllocator { remaining: 1 }
  let mut values = make<I32>()
  let pending0 = append<I32>(&mut values, 10) |> Allocator.provide(&mut allocator)
  let appended0 = run pending0
  let pending1 = append<I32>(&mut values, 11) |> Allocator.provide(&mut allocator)
  let appended1 = run pending1
  let pending2 = append<I32>(&mut values, 12) |> Allocator.provide(&mut allocator)
  let appended2 = run pending2
  let pending3 = append<I32>(&mut values, 13) |> Allocator.provide(&mut allocator)
  let appended3 = run pending3
  let marker = run Effect.catch<OutOfMemory>(
    grow(&mut values) |> Allocator.provide(&mut allocator),
    recover,
  )
  if marker == 7 {} else { return 0 }
  if length<I32>(&values) == 4 {} else { return 1 }
  if capacity<I32>(&values) == 4 {} else { return 2 }
  let first = get<I32>(&mut values, 0)
  let last = get<I32>(&mut values, 3)
  return first + last + 19
}

effect fn outerRecover(error: OutOfMemory) -> I32 { return 0 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), outerRecover) }`

export const vectorDestructionOrderSource = `import silk.vector { Vector, make, append, capacity }

struct Entry {
  value: I32
  marker: Vector<I32>
}

fn record(value: I32) -> Unit { return Unit.make() }

impl Drop for Entry {
  fn drop(self: &mut Entry) -> Unit {
    return record(self.value)
  }
}

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut values = make<Entry>()
  let entry0 = Entry { value: 3, marker: make<I32>() }
  let pending0 = append<Entry>(&mut values, move entry0) |> Allocator.provide(&mut allocator)
  let appended0 = run pending0
  let entry1 = Entry { value: 5, marker: make<I32>() }
  let pending1 = append<Entry>(&mut values, move entry1) |> Allocator.provide(&mut allocator)
  let appended1 = run pending1
  let entry2 = Entry { value: 7, marker: make<I32>() }
  let pending2 = append<Entry>(&mut values, move entry2) |> Allocator.provide(&mut allocator)
  let appended2 = run pending2
  if capacity<Entry>(&values) == 4 {} else { return 0 }
  return 42
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }`

export const vectorEarlyDropSource = `import silk.vector { Vector, make, append }

struct Entry {
  value: I32
  marker: Vector<I32>
}

fn record(value: I32) -> Unit { return Unit.make() }

impl Drop for Entry {
  fn drop(self: &mut Entry) -> Unit {
    return record(self.value)
  }
}

fn consume(values: Vector<Entry>) -> I32 {
  drop values
  return 40
}

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let mut values = make<Entry>()
  let entry0 = Entry { value: 11, marker: make<I32>() }
  let pending0 = append<Entry>(&mut values, move entry0) |> Allocator.provide(&mut allocator)
  let appended0 = run pending0
  let entry1 = Entry { value: 13, marker: make<I32>() }
  let pending1 = append<Entry>(&mut values, move entry1) |> Allocator.provide(&mut allocator)
  let appended1 = run pending1
  let consumed = consume(move values)
  return consumed + 2
}

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }`

export const scannerSource = `import silk.vector { Vector, make, append, get, length, capacity }

struct U8 { value: I32 }
struct Token { kind: I32 }

fn observe(kind: I32) -> I32 { return kind }

effect fn scan(source: &[U8]) -> Vector<Token> ! OutOfMemory ? &mut Allocator {
  let mut tokens = make<Token>()
  let mut index = 0
  while index < source.length {
    let byte = source[index].value
    let mut kind = 3
    if byte == 1 { kind = 1 }
    if byte == 2 { kind = 2 }
    let token = Token { kind: kind }
    let appended = run append<Token>(&mut tokens, move token)
    index = index + 1
  }
  return move tokens
}

effect fn build() -> I32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let source = [
    U8 { value: 1 }, U8 { value: 2 }, U8 { value: 3 }, U8 { value: 1 },
    U8 { value: 2 }, U8 { value: 3 }, U8 { value: 1 }, U8 { value: 2 },
    U8 { value: 3 }, U8 { value: 1 }
  ]
  let pending = scan(&source) |> Allocator.provide(&mut allocator)
  let mut tokens = run pending
  if length<Token>(&tokens) == 10 {} else { return 0 }
  if capacity<Token>(&tokens) == 16 {} else { return 1 }
  let token0 = get<Token>(&mut tokens, 0)
  let token1 = get<Token>(&mut tokens, 1)
  let token2 = get<Token>(&mut tokens, 2)
  let token3 = get<Token>(&mut tokens, 3)
  let token4 = get<Token>(&mut tokens, 4)
  let token5 = get<Token>(&mut tokens, 5)
  let token6 = get<Token>(&mut tokens, 6)
  let token7 = get<Token>(&mut tokens, 7)
  let token8 = get<Token>(&mut tokens, 8)
  let token9 = get<Token>(&mut tokens, 9)
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

effect fn recover(error: OutOfMemory) -> I32 { return 7 }

pub fn main() -> I32 { return run Effect.catch<OutOfMemory>(build(), recover) }
`
