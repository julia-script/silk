/**
 * Every program the per-phase labs shipped as a preset, in one catalog.
 *
 * These are the cases each phase was built against — the recovery paths, the ambiguity, the
 * cycles, the traps. They are the fastest way to put the compiler in a specific state, so they
 * are worth keeping complete rather than trimming to a handful that fit in a button bar.
 *
 * Everything is modelled as a module map even when there is only one module, so the multi-module
 * presets are not a special case: `modules` plus a `root` is what the driver takes.
 *
 * Labels are prefixed so the picker shows intent at a glance:
 * - `ok ·`   — compiles clean and evaluates successfully (or is a declaration-only sample)
 * - `fail ·` — intentionally damaged; expected to surface diagnostics
 * - `trap ·` — compiles clean but is meant to block or trap at evaluation (cycles, missing
 *              entry, runtime overflow / bounds traps)
 */

import {
  scannerSource,
  vectorDestructionOrderSource,
  vectorEarlyDropSource,
  vectorFailedGrowthSource,
  vectorGrowthSource,
} from './owned-sequence-presets'
import type { BootstrapEvaluation } from '@silk-effect/compiler'

export interface Preset {
  readonly label: string
  /** Which phase this program was written to exercise; used to group the picker. */
  readonly group: string
  readonly root: string
  readonly modules: Readonly<Record<string, string>>
  readonly evaluation?: BootstrapEvaluation.Options
}

/** Single-module presets are the common case; this keeps the table readable. */
const one = (group: string, label: string, source: string): Preset => ({
  label,
  group,
  root: 'main',
  modules: { main: source },
})

const mainFn = 'pub fn main() -> i32 { return 42 }'

const identity = 'pub fn identity(value: i32) -> i32 { return value }'

export const presets: ReadonlyArray<Preset> = [
  {
    label: 'ok · Completed runtime recursion',
    group: 'evaluation',
    root: 'main',
    modules: {
      main: `fn countdown(value: i32) -> i32 {
  if value == 0 { return 42 }
  return countdown(value - 1)
}
pub fn main() -> i32 { return countdown(4) }`,
    },
  },
  {
    label: 'trap · Call-depth evaluation limit',
    group: 'evaluation',
    root: 'main',
    modules: { main: 'pub fn main() -> i32 { return main() }' },
    evaluation: { maxCallDepth: 4 },
  },
  one(
    'generics',
    'ok · Inferred and explicit specializations',
    `struct Box<T> { value: T }
fn identity<T>(value: T) -> T { return move value }
pub fn main() -> i32 {
  let flag = identity(true)
  let box = Box<i32> { value: identity<i32>(42) }
  return box.value
}`,
  ),
  // ---- first composed acceptance slice -------------------------------------------------
  {
    label: 'ok · Algorithmic coverage fold',
    group: 'acceptance',
    root: 'app/Main',
    modules: {
      'app/Main': `import compiler.Coverage

pub fn main() -> i32 {
  let short = [1, 2, 3]
  let long = [1, 2, 1, 3, 2, 1]
  let shortScore = Coverage.fold(&short)
  let longScore = Coverage.fold(&long)
  if shortScore != 40 {
    return 0
  }
  return longScore
}\n`,
      'compiler/Member': `pub struct First {}
pub struct Second {}
pub struct Third {}

pub fn decode(code: i32) -> First | Second | Third {
  if code == 1 {
    return First {}
  }
  if code == 2 {
    return Second {}
  }
  return Third {}
}\n`,
      'compiler/Coverage': `import compiler.Member { First, Second, Third, decode }

struct FoldState {
  remaining: i32
  score: i32
  seenFirst: bool
  seenSecond: bool
  seenThird: bool
}

pub fn fold(codes: &[i32]) -> i32 {
  let mut state = FoldState {
    remaining: 3,
    score: 10,
    seenFirst: false,
    seenSecond: false,
    seenThird: false,
  }
  let mut index = usize.add(0, 0)

  while index < codes.length {
    let candidate = decode(codes[index])
    let member = match move candidate {
      First {} if index == 2 => 0
      First {} => 1
      Second {} => 2
      Third {} => 3
    }

    if member == 1 {
      if !state.seenFirst {
        state.seenFirst = true
        state.remaining = state.remaining - 1
        state.score = state.score + 10
      } else {
        state.score = state.score + 1
      }
    }
    if member == 2 {
      if !state.seenSecond {
        state.seenSecond = true
        state.remaining = state.remaining - 1
        state.score = state.score + 10
      } else {
        state.score = state.score + 1
      }
    }
    if member == 3 {
      if !state.seenThird {
        state.seenThird = true
        state.remaining = state.remaining - 1
        state.score = state.score + 10
      } else {
        state.score = state.score + 1
      }
    }

    index = index + 1
  }

  if state.remaining != 0 {
    return 0
  }
  return state.score
}\n`,
    },
  },
  one(
    'acceptance',
    'ok · Exclusive runtime slice',
    `struct Token { value: i32 }

fn replace(values: &mut [Token], index: usize) -> i32 {
  values[index] = Token { value: 42 }
  return usize.toI32(values.length)
}

pub fn main() -> i32 {
  let mut values = [Token { value: 1 }, Token { value: 2 }]
  let length = replace(&mut values, 0)
  if length != 2 {
    return 0
  }
  return values[0].value
}
`,
  ),
  one(
    'effects',
    'ok · Eager setup, lazy Effect body',
    `pub fn main() -> i32 {
  let eager = i32.add(20, 1)
  let delayed = effect {
    return i32.add(eager, 21)
  }
  return run delayed
}
`,
  ),
  one(
    'effects',
    'ok · Reusable exclusive capture',
    `pub fn main() -> i32 {
  let mut counter = 10
  let increment = effect {
    counter = counter + 1
    return counter
  }
  let first = run increment
  return run increment
}
`,
  ),
  one(
    'effects',
    'ok · Retry with persistent capture',
    `struct Problem { code: i32 }

effect fn retrying() -> i32 ! Problem {
  let mut counter = 0
  let attempt = effect {
    counter = counter + 1
    if counter < 3 { fail Problem { code: counter } }
    return counter
  }
  return run attempt |> Effect.retry(2)
}

effect fn recover(problem: Problem) -> i32 { return 99 }

pub fn main() -> i32 {
  return run retrying() |> Effect.catch(recover)
}
`,
  ),
  one(
    'effects',
    'ok · Existing provider capture',
    `struct Clock {}

effect fn read() -> i32 ? &Clock@Primary {
  return 42
}

pub fn main() -> i32 {
  let clock = Clock {}
  let pending = read()
    |> Effect.bindRequirement(&clock, @Primary)
  return run pending
}
`,
  ),
  one(
    'allocation',
    'ok · Validated target Layout',
    `pub fn main() -> i32 {
  let candidate = Layout.make(64, 32)
  return match move candidate {
    Layout { bytes, alignment } => 42
    InvalidAlignment { alignment } => 0
  }
}
`,
  ),
  one(
    'allocation',
    'ok · Self-contained Allocation contract',
    `effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<i32>(move allocation, 2)
    let written = Slot.write(RawBuffer.slot(&mut buffer, 0), 42)
    let result = Slot.take(RawBuffer.slot(&mut buffer, 0))
    drop buffer
    return result
  }
  return 0
}

effect fn recover(error: OutOfMemory) -> i32 { return 0 }

pub fn main() -> i32 {
  return run Effect.catch(store(), recover)
}
`,
  ),
  one(
    'allocation',
    'ok · Early drop releases the buffer once',
    `effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<i32>(move allocation, 2)
    drop buffer
    return 42
  }
  return 0
}

effect fn recover(error: OutOfMemory) -> i32 { return 0 }

pub fn main() -> i32 {
  return run Effect.catch(store(), recover)
}
`,
  ),
  one(
    'allocation',
    'ok · Zero-sized elements keep distinct owners',
    `struct Empty {}

effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[Empty; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let mut buffer = RawBuffer.from<Empty>(move allocation, 2)
    drop buffer
    return 42
  }
  return 0
}

effect fn recover(error: OutOfMemory) -> i32 { return 0 }

pub fn main() -> i32 {
  return run Effect.catch(store(), recover)
}
`,
  ),
  one(
    'allocation',
    'fail · Raw storage outside unsafe is rejected',
    `effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  let mut buffer = RawBuffer.from<i32>(move allocation, 2)
  drop buffer
  return 42
}

effect fn recover(error: OutOfMemory) -> i32 { return 0 }

pub fn main() -> i32 {
  return run Effect.catch(store(), recover)
}
`,
  ),
  one(
    'allocation',
    'ok · Drop hook runs before field cleanup',
    `struct Guard { buffer: RawBuffer<i32> }

impl Drop for Guard {
  fn drop(self: &mut Guard) -> () { return () }
}

effect fn store() -> i32 ! OutOfMemory {
  let mut allocator = SystemAllocator.make()
  let layout = Layout.of<[i32; 2]>()
  let recipe = Allocator.allocate(move layout) |> Effect.provideMut(&mut allocator)
  let allocation = run recipe
  unsafe {
    let buffer = RawBuffer.from<i32>(move allocation, 2)
    let guard = Guard { buffer: move buffer }
    drop guard
    return 42
  }
  return 0
}

effect fn recover(error: OutOfMemory) -> i32 { return 0 }

pub fn main() -> i32 {
  return run Effect.catch(store(), recover)
}
`,
  ),
  one('allocation', 'ok · Vector growing append', vectorGrowthSource),
  one('allocation', 'ok · Vector failed growth preserves contents', vectorFailedGrowthSource),
  one('allocation', 'ok · Vector destruction order', vectorDestructionOrderSource),
  one('allocation', 'ok · Vector early drop', vectorEarlyDropSource),
  one('allocation', 'ok · Scanner returns owned tokens', scannerSource),
  one(
    'effects',
    'ok · Typed Effect recovery',
    `struct Problem { code: i32 }

effect fn risky<T>(value: T, selector: i32) -> T ! Problem {
  if selector == 0 { fail move Problem { code: 41 } }
  return move value
}

effect fn relay(value: i32) -> i32 ! Problem {
  let pending = risky<i32>(value, value)
  return run pending
}

effect fn recover(problem: Problem) -> i32 {
  return problem.code |> i32.add(1)
}

pub fn main() -> i32 {
  let recipe = relay(0)
    |> Effect.catch(recover)
  return run recipe
}
`,
  ),
  one(
    'effects',
    'fail · Unhandled Effect residual',
    `struct Problem { code: i32 }

effect fn risky() -> i32 ! Problem {
  fail move Problem { code: 41 }
}

pub fn main() -> i32 {
  return run risky()
}
`,
  ),

  // ---- first-class callables -----------------------------------------------------------
  one(
    'callables',
    'ok · Named function and stored section',
    `fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 {
  let named = identity
  let plusTwo = i32.add(2)
  return named(plusTwo(40))
}
`,
  ),
  one(
    'callables',
    'trap · Shared reusable section',
    `fn read(value: i32, values: &[i32]) -> i32 { return value + values[0] }
pub fn main() -> i32 {
  let values = [1]
  let callback = read(&values)
  return callback(20) + callback(20)
}
`,
  ),
  one(
    'callables',
    'ok · Exclusive reusable section',
    `fn write(value: i32, values: &mut [i32]) -> i32 {
  values[0] = value
  return values[0]
}
pub fn main() -> i32 {
  let mut values = [0]
  let mut callback = write(&mut values)
  let first = callback(40)
  let second = callback(first + 2)
  drop callback
  return second
}
`,
  ),
  one(
    'callables',
    'ok · Consuming section',
    `struct Token { value: i32 }
fn consume(value: i32, token: Token) -> i32 { return value + token.value }
pub fn main() -> i32 {
  let token = Token { value: 2 }
  let callback = consume(move token)
  return callback(40)
}
`,
  ),
  one(
    'callables',
    'fail · Invalid consuming reuse',
    `struct Token { value: i32 }
fn consume(value: i32, token: Token) -> i32 { return value + token.value }
pub fn main() -> i32 {
  let token = Token { value: 1 }
  let callback = consume(move token)
  let first = callback(20)
  return callback(first)
}
`,
  ),
  one(
    'callables',
    'ok · Generic stored section',
    `fn choose<T>(value: T, fallback: T) -> T { return move value }
pub fn main() -> i32 {
  let chosen = choose<i32>(0)
  return chosen(42)
}
`,
  ),
  one(
    'callables',
    'ok · Dropped uninvoked section',
    `struct Token { value: i32 }
fn consume(value: i32, token: Token) -> i32 { return value + token.value }
pub fn main() -> i32 {
  let token = Token { value: 2 }
  let callback = consume(move token)
  drop callback
  return 42
}
`,
  ),
  one(
    'effects',
    'ok · Callable map section',
    `effect fn succeed(value: i32) -> i32 { return value }
pub fn main() -> i32 { return run succeed(2) |> Effect.map(i32.add(40)) }
`,
  ),
  one(
    'effects',
    'ok · Effectful Logger tap',
    `struct Logger {}
effect fn succeed(value: i32) -> i32 { return value }
effect fn log(value: i32) -> i32 ? &Logger { return value }
pub fn main() -> i32 {
  let logger = Logger {}
  let logged = succeed(42) |> Effect.tap(log)
  let provided = logged |> Effect.provide(&logger)
  return run provided
}
`,
  ),
  one(
    'effects',
    'ok · Mutable mapped callback',
    `effect fn succeed(value: i32) -> i32 { return value }
fn increment(value: i32, state: &mut [i32]) -> i32 {
  state[0] = state[0] + 1
  return value + state[0]
}
pub fn main() -> i32 {
  let mut state = [0]
  let mapped = succeed(20) |> Effect.map(increment(&mut state))
  let first = run mapped
  let second = run mapped
  drop mapped
  return first + second
}
`,
  ),
  one(
    'effects',
    'fail · Take-once retry rejection',
    `struct Payload { value: i32 }
effect fn succeed(value: i32) -> i32 { return value }
fn consume(value: i32, payload: Payload) -> i32 { return value + payload.value }
pub fn main() -> i32 {
  let payload = Payload { value: 2 }
  let mapped = succeed(40) |> Effect.map(consume(move payload))
  let retried = mapped |> Effect.retry(2)
  return 0
}
`,
  ),
  one(
    'effects',
    'ok · Nested map result',
    `effect fn succeed(value: i32) -> i32 { return value }
effect fn double(value: i32) -> i32 { return value * 2 }
pub fn main() -> i32 { return run run succeed(21) |> Effect.map(double) }
`,
  ),
  one(
    'effects',
    'ok · Ungrouped run composes first',
    `effect fn succeed(value: i32) -> i32 { return value }
pub fn main() -> i32 { return run succeed(2) |> Effect.map(i32.add(40)) }
`,
  ),
  one(
    'effects',
    'ok · Grouped run transforms result',
    `effect fn succeed(value: i32) -> i32 { return value }
pub fn main() -> i32 { return (run succeed(2)) |> i32.add(40) }
`,
  ),

  // ---- syntax ---------------------------------------------------------------------------
  one('syntax', 'ok · Literal result', 'pub fn main() -> i32 { return 42 }'),
  one(
    'syntax',
    'ok · Two functions',
    `pub fn answer() -> i32 { return 42 }
pub fn main() -> i32 { return 0 }`,
  ),
  one(
    'syntax',
    'ok · Three functions',
    `pub fn one() -> i32 { return 1 }
pub fn two() -> i32 { return 2 }
pub fn three() -> i32 { return 3 }`,
  ),
  one(
    'syntax',
    'ok · Resolved backward',
    `pub fn answer() -> i32 { return 42 }
pub fn main() -> i32 { return answer() }`,
  ),
  one(
    'syntax',
    'ok · Resolved forward',
    `pub fn main() -> i32 { return answer() }
pub fn answer() -> i32 { return 42 }`,
  ),
  one('syntax', 'trap · Direct cycle', 'pub fn main() -> i32 { return main() }'),
  one('syntax', 'fail · Unknown call', 'pub fn main() -> i32 { return missing() }'),
  one(
    'syntax',
    'fail · Ambiguous call',
    `pub fn same() -> i32 { return 1 }
pub fn same() -> i32 { return 2 }
pub fn main() -> i32 { return same() }`,
  ),
  one('syntax', 'fail · Missing callee', 'pub fn main() -> i32 { return ()() }'),
  one(
    'syntax',
    'fail · Missing call )',
    `pub fn answer() -> i32 { return 42 }
pub fn main() -> i32 { return answer( }`,
  ),
  one(
    'syntax',
    'ok · Resolved parameter',
    `${identity}
pub fn main() -> i32 { return identity(42) }`,
  ),
  one(
    'syntax',
    'fail · Effect unknown reference',
    `pub fn identity(value: i32) -> i32 { return missing }
pub fn main() -> i32 { return identity(42) }`,
  ),
  one(
    'syntax',
    'fail · Effect ambiguous reference',
    `pub fn choose(value: i32, value: i32) -> i32 { return value }
pub fn main() -> i32 { return choose(1, 2) }`,
  ),
  one(
    'syntax',
    'fail · Effect damaged syntax',
    `${identity}
pub fn main() -> i32 { return identity(@) }`,
  ),
  one(
    'syntax',
    'fail · Wrong arity',
    `pub fn choose(left: i32, right: i32) -> i32 { return left }
pub fn main() -> i32 { return choose(1, 2, 3) }`,
  ),
  one(
    'syntax',
    'fail · Too many arguments',
    `${identity}
pub fn main() -> i32 { return identity(1, 2) }`,
  ),
  one(
    'syntax',
    'fail · Unavailable evaluation',
    `pub fn identity(value: Mystery) -> i32 { return 0 }
pub fn main() -> i32 { return identity(42) }`,
  ),
  one(
    'syntax',
    'ok · Second argument result',
    `pub fn second(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { return second(10, 42) }`,
  ),
  one('syntax', 'trap · Missing entry', 'pub fn answer() -> i32 { return 42 }'),
  one('syntax', 'ok · Empty module', ''),
  one(
    'syntax',
    'trap · Mutual cycle',
    `pub fn main() -> i32 { return other() }
pub fn other() -> i32 { return main() }`,
  ),
  one('syntax', 'fail · Unresolved contract call', 'pub fn main() -> i32 { return missing(42) }'),
  one('syntax', 'fail · Unknown value', 'pub fn main() -> i32 { return missing }'),
  one('syntax', 'fail · Missing return keyword', 'pub fn main() -> i32 { missing }'),
  one(
    'syntax',
    'fail · Duplicate parameter',
    'pub fn choose(value: i32, value: i32) -> i32 { return value }',
  ),
  one(
    'syntax',
    'fail · Cross-function parameter',
    `pub fn owner(value: i32) -> i32 { return value }
pub fn other() -> i32 { return value }`,
  ),
  one('syntax', 'fail · Recovered reference', 'pub fn identity(value: i32) -> i32 { return @ value }'),
  one('syntax', 'ok · Two parameters', 'pub fn choose(left: i32, right: i32) -> i32 { return left }'),
  one(
    'syntax',
    'ok · Identifier argument',
    `${identity}
pub fn forward(value: i32) -> i32 { return identity(value) }`,
  ),
  one(
    'syntax',
    'ok · Nested flow · complete',
    `${identity}
pub fn main() -> i32 { return identity(identity(42)) }`,
  ),
  one(
    'syntax',
    'ok · Nested flow · siblings',
    `${identity}
pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { return choose(identity(1), identity(2)) }`,
  ),
  one(
    'syntax',
    'fail · Nested flow · unavailable',
    `${identity}
pub fn uncertain(value: Mystery) -> i32 { return 0 }
pub fn main() -> i32 { return identity(uncertain(42)) }`,
  ),
  one(
    'syntax',
    'fail · Nested flow · wrong arity',
    `${identity}
pub fn main() -> i32 { return identity(identity()) }`,
  ),
  one(
    'syntax',
    'fail · Damaged nested call',
    `${identity}
pub fn main() -> i32 { return identity(identity(@)) }`,
  ),
  one(
    'syntax',
    'fail · Nested flow · inner blocked',
    `${identity}
pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { return choose(identity(1), missing(2)) }`,
  ),
  one(
    'syntax',
    'trap · Nested flow · cycle',
    `${identity}
pub fn main() -> i32 { return identity(main()) }`,
  ),
  one('syntax', 'fail · Missing parameter type', 'pub fn identity(value:) -> i32 { return value }'),
  one(
    'syntax',
    'fail · Missing parameter comma',
    'pub fn choose(left: i32 right: i32) -> i32 { return left }',
  ),
  one('syntax', 'fail · Malformed argument', 'pub fn main(value: i32) -> i32 { return missing(@, value) }'),
  one(
    'syntax',
    'fail · Missing name',
    `pub fn answer() -> i32 { return 42 }
pub fn () -> i32 { return 0 }`,
  ),
  one(
    'syntax',
    'fail · Duplicate names',
    `pub fn same() -> i32 { return 1 }
pub fn same() -> i32 { return 2 }`,
  ),
  one(
    'syntax',
    'fail · Mixed damage',
    `pub fn main() -> i32 { return 42 }
pub fn damaged() -> Mystery { return 2147483648 }`,
  ),
  one(
    'syntax',
    'fail · Missing first }',
    `pub fn answer() -> i32 { return 42
pub fn main() -> i32 { return 0 }`,
  ),
  one('syntax', 'fail · Missing }', 'pub fn main() -> i32 { return 42'),
  one('syntax', 'fail · Unexpected @', 'pub fn @ main() -> i32 { return 42 }'),
  one('syntax', 'fail · Unknown type', 'pub fn main() -> Mystery { return 42 }'),
  one('syntax', 'fail · i32 overflow', 'pub fn main() -> i32 { return 2147483648 }'),
  one('syntax', 'fail · UTF-8', 'pub fn café() -> i32 { return 42 }'),

  // ---- modules --------------------------------------------------------------------------
  {
    label: 'ok · Diamond',
    group: 'modules',
    root: 'root',
    modules: {
      root: `import left\nimport right\n${mainFn}`,
      left: `import shared\n${mainFn}`,
      right: `import shared\n${mainFn}`,
      shared: mainFn,
    },
  },
  {
    label: 'ok · Mutual cycle (imports)',
    group: 'modules',
    root: 'root',
    modules: {
      root: `import beta\n${mainFn}`,
      beta: `import gamma\n${mainFn}`,
      gamma: `import beta\n${mainFn}`,
    },
  },
  {
    label: 'fail · Unknown import',
    group: 'modules',
    root: 'root',
    modules: { root: `import missing\n${mainFn}` },
  },
  {
    label: 'fail · Self import',
    group: 'modules',
    root: 'root',
    modules: { root: `import root\n${mainFn}` },
  },
  {
    label: 'ok · Unreachable island',
    group: 'modules',
    root: 'root',
    modules: {
      root: `import used\n${mainFn}`,
      used: mainFn,
      island: `import used\n${mainFn}`,
    },
  },

  // ---- headers --------------------------------------------------------------------------
  {
    label: 'ok · Two modules',
    group: 'headers',
    root: 'root',
    modules: {
      root: 'import lib\npub fn main() -> i32 { return 42 }',
      lib: 'pub fn answer() -> i32 { return 1 }\npub fn choose(left: i32, right: i32) -> i32 { return left }',
    },
  },
  {
    label: 'ok · Same name twice across modules',
    group: 'headers',
    root: 'root',
    modules: {
      root: 'import lib\npub fn answer() -> i32 { return 1 }',
      lib: 'pub fn answer() -> i32 { return 2 }',
    },
  },
  {
    label: 'fail · Duplicate in one module',
    group: 'headers',
    root: 'root',
    modules: { root: 'pub fn same() -> i32 { return 1 }\npub fn same() -> i32 { return 2 }' },
  },
  {
    label: 'fail · Missing name (header)',
    group: 'headers',
    root: 'root',
    modules: { root: 'pub fn () -> i32 { return 0 }' },
  },
  {
    label: 'fail · Unknown types',
    group: 'headers',
    root: 'root',
    modules: { root: 'pub fn puzzle(value: Mystery) -> Enigma { return 0 }' },
  },

  // ---- structs --------------------------------------------------------------------------
  one('structs', 'ok · Empty struct', 'struct Marker {}\npub fn main() -> i32 { return 42 }'),
  one(
    'structs',
    'ok · Nested physical layout',
    'struct Pair { left: i32 right: bool }\nstruct Outer { pair: Pair value: i32 }\npub fn main() -> i32 { return 42 }',
  ),
  {
    label: 'ok · Imported nominal type',
    group: 'structs',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import model.Tree as Ast { Node }\nstruct Root { selected: Node qualified: Ast.Node }\npub fn main() -> i32 { return 42 }',
      'model/Tree': 'pub struct Node { value: i32 }',
    },
  },
  one(
    'structs',
    'fail · Private type exposure',
    'struct Hidden { value: i32 }\npub struct Visible { pub hidden: Hidden }\npub fn main() -> i32 { return 42 }',
  ),
  one(
    'structs',
    'fail · Damaged field',
    'struct Broken { value: Missing next: i32 }\npub fn main() -> i32 { return 42 }',
  ),
  one(
    'structs',
    'fail · Recursive structs',
    'struct Left { right: Right }\nstruct Right { left: Left }\npub fn main() -> i32 { return 42 }',
  ),
  one(
    'structs',
    'ok · Construct and project fields',
    `struct Pair { left: i32 right: i32 }
fn make() -> Pair { return Pair { right: 2, left: 1 } }
pub fn main() -> i32 { let pair = make() return pair.right }`,
  ),
  one(
    'structs',
    'ok · Nested projections',
    `struct Inner { value: i32 }
struct Outer { inner: Inner }
fn make() -> Outer { return Outer { inner: Inner { value: 42 } } }
pub fn main() -> i32 { let outer = make() return outer.inner.value }`,
  ),
  one(
    'structs',
    'ok · Zero-lane struct value',
    `struct Marker {}
fn marker() -> Marker { return Marker {} }
pub fn main() -> i32 { let value = marker() return 42 }`,
  ),
  one(
    'structs',
    'fail · Invalid struct literal',
    `struct Pair { left: i32 right: i32 }
fn broken() -> Pair { return Pair { left: true, left: 2, extra: 3 } }
pub fn main() -> i32 { return 42 }`,
  ),
  {
    label: 'ok · Public struct factory',
    group: 'structs',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import model.Pair { Pair, make }\npub fn main() -> i32 { let pair = make(20, 22) return pair.right }',
      'model/Pair':
        'pub struct Pair { pub left: i32 pub right: i32 }\npub fn make(left: i32, right: i32) -> Pair { return Pair { right: right, left: left } }',
    },
  },

  // ---- names ----------------------------------------------------------------------------
  // Namespaced imports, aliases, and selective member lists: every binding form the resolver
  // has to answer for, plus the ways one can fail to bind.
  {
    label: 'ok · Namespace import',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main': 'import compiler.Syntax\npub fn main() -> i32 { return Syntax.parse() }',
      'compiler/Syntax': 'pub fn parse() -> i32 { return 42 }',
    },
  },
  {
    label: 'ok · Selective alias',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import compiler.Syntax { parse as read }\npub fn main() -> i32 { return read() }',
      'compiler/Syntax': 'pub fn parse() -> i32 { return 42 }',
    },
  },
  {
    label: 'ok · Hybrid alias',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import compiler.Syntax as Tree { parse }\npub fn main() -> i32 { return Tree.parse() }',
      'compiler/Syntax': 'pub fn parse() -> i32 { return 42 }',
    },
  },
  {
    label: 'fail · Private member',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main': 'import compiler.Syntax { hidden }\npub fn main() -> i32 { return 0 }',
      'compiler/Syntax': 'fn hidden() -> i32 { return 42 }',
    },
  },
  {
    label: 'fail · Unknown member',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import compiler.Syntax { missing }\npub fn main() -> i32 { return missing() }',
      'compiler/Syntax': 'pub fn parse() -> i32 { return 42 }',
    },
  },
  {
    label: 'fail · Damaged alias',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main': 'import compiler.Syntax as\npub fn main() -> i32 { return 0 }',
      'compiler/Syntax': 'pub fn parse() -> i32 { return 42 }',
    },
  },
  {
    label: 'fail · Import collision',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import compiler.Syntax { parse }\npub fn parse() -> i32 { return 0 }\npub fn main() -> i32 { return parse() }',
      'compiler/Syntax': 'pub fn parse() -> i32 { return 42 }',
    },
  },
  {
    label: 'trap · Import cycle (names)',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main': 'import cycle.A { a }\npub fn main() -> i32 { return a() }',
      'cycle/A': 'import cycle.B { b }\npub fn a() -> i32 { return b() }',
      'cycle/B': 'import cycle.A { a }\npub fn b() -> i32 { return a() }',
    },
  },

  // ---- operators ------------------------------------------------------------------------
  one('operators', 'ok · Operator precedence', 'pub fn main() -> i32 { return 2 + 5 * 8 }'),
  one('operators', 'ok · Pipeline', 'pub fn main() -> i32 { return 2 |> i32.add(40) }'),
  one(
    'operators',
    'ok · Unary bool pipeline',
    'pub fn main() -> i32 { if true |> bool.not { return 0 } return 42 }',
  ),
  one('operators', 'ok · bool not', 'pub fn main() -> i32 { if !(1 == 2) { return 42 } return 0 }'),
  one('operators', 'trap · Negation overflow traps', 'pub fn main() -> i32 { return -(-2147483648) }'),
  one(
    'operators',
    'ok · Closed operator surface',
    `pub fn main() -> i32 {
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
  ),
  one(
    'backend',
    'ok · Target-sized usize boundary',
    `fn nativeBoundary() -> usize { return 4294967296 }
pub fn main() -> i32 {
  if nativeBoundary() > 4294967295 { return 42 }
  return 0
}`,
  ),

  // ---- arrays ---------------------------------------------------------------------------
  one('arrays', 'ok · Array inferred', 'pub fn main() -> i32 { let values = [10, 42] return values[1] }'),
  one(
    'arrays',
    'ok · Array contextual',
    `fn values() -> [i32; 2] { return [10, 42] }
pub fn main() -> i32 { return values()[1] }`,
  ),
  one(
    'arrays',
    'ok · Array empty',
    `fn empty() -> [i32; 0] { return [] }
fn consume(values: [i32; 0]) -> i32 { return 42 }
pub fn main() -> i32 { return consume(empty()) }`,
  ),
  one(
    'arrays',
    'ok · Array nested',
    `fn choose(values: [[i32; 2]; 2], outer: usize, inner: usize) -> i32 { return values[outer][inner] }
pub fn main() -> i32 { return choose([[10, 11], [42, 43]], 1, 0) }`,
  ),
  one(
    'arrays',
    'ok · Array struct elements',
    `struct Pair { left: i32 right: i32 }
pub fn main() -> i32 { let values = [Pair { left: 10, right: 11 }, Pair { left: 42, right: 43 }] return values[1].left }`,
  ),
  one(
    'arrays',
    'ok · Array evaluation order',
    `fn first() -> i32 { return 10 }
fn second() -> i32 { return 42 }
pub fn main() -> i32 { let values = [first(), second()] return values[1] }`,
  ),
  one(
    'arrays',
    'ok · Array whole moved',
    `struct Token { value: i32 }
pub fn main() -> i32 { let values = [Token { value: 10 }, Token { value: 42 }] let moved = move values return moved[1].value }`,
  ),
  one(
    'arrays',
    'ok · Array Copy read',
    `fn choose(values: [i32; 2], index: usize) -> i32 { let selected = values[index] return selected }
pub fn main() -> i32 { return choose([10, 42], 1) }`,
  ),
  one(
    'arrays',
    'ok · Array indexed field',
    `struct Pair { left: i32 right: i32 }
fn choose(values: [Pair; 2], index: usize) -> i32 { return values[index].left }
pub fn main() -> i32 { return choose([Pair { left: 10, right: 11 }, Pair { left: 42, right: 43 }], 1) }`,
  ),
  one(
    'arrays',
    'fail · Array constant out of bounds',
    'pub fn main() -> i32 { let values = [10, 42] return values[2] }',
  ),
  one(
    'arrays',
    'trap · Array dynamic trap',
    `fn choose(values: [i32; 2], index: usize) -> i32 { return values[index] }
pub fn main() -> i32 { return choose([10, 42], -1) }`,
  ),
  one(
    'arrays',
    'fail · Array type mismatch',
    'pub fn main() -> i32 { let values = [1, true] return 0 }',
  ),
  one(
    'arrays',
    'fail · Array length mismatch',
    `fn values() -> [i32; 3] { return [10, 42] }
pub fn main() -> i32 { return 0 }`,
  ),
  one(
    'arrays',
    'fail · Array partial move',
    `struct Token { value: i32 }
fn take(values: [Token; 2]) -> Token { return move values[0] }
pub fn main() -> i32 { return 42 }`,
  ),
  one(
    'arrays',
    'trap · Array unavailable layout',
    `fn consume(values: [[[i32; 2147483647]; 2147483647]; 0]) -> i32 { return 42 }
pub fn main() -> i32 { return consume([]) }`,
  ),

  // ---- ownership ------------------------------------------------------------------------
  one(
    'ownership',
    'ok · Let bindings',
    `${identity}
pub fn main() -> i32 { let value = identity(42) let extra = 1 return value }`,
  ),
  one(
    'ownership',
    'ok · Moved binding',
    `${identity}
pub fn main() -> i32 { let value = 42 return identity(move value) }`,
  ),
  one(
    'ownership',
    'fail · Use after move',
    `pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { let value = 42 return choose(move value, value) }`,
  ),
  one(
    'ownership',
    'ok · Two parameters (ownership)',
    `pub fn choose(left: i32, right: i32) -> i32 { return left }
pub fn main() -> i32 { return choose(1, 2) }`,
  ),
  one('ownership', 'fail · Damaged body', 'pub fn main() -> i32 { return missing() }'),
  one('ownership', 'fail · Unknown parameter type', 'pub fn puzzle(value: Mystery) -> i32 { return value }'),

  // ---- exhaustive matching --------------------------------------------------------------
  one(
    'matching',
    'ok · Guarded union match',
    `struct Left { value: i32 }
struct Right { value: i32 }
fn inspect(input: Left | Right) -> i32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => answer + 1
    Right { value } => value
  }
}
pub fn main() -> i32 { return inspect(Left { value: 41 }) }`,
  ),
  one(
    'matching',
    'ok · Nested renamed patterns',
    `struct Token { kind: i32 }
struct Box { token: Token extra: i32 }
pub fn main() -> i32 {
  let box = Box { token: Token { kind: 42 }, extra: 0 }
  return match move box { Box { token: Token { kind: answer }, .. } => answer }
}`,
  ),
  one(
    'matching',
    'ok · Universal fallback',
    `struct Left { value: i32 }
struct Right { value: i32 }
fn inspect(input: Left | Right) -> i32 { return match &input { Left { value } => value _ => 0 } }
pub fn main() -> i32 { return inspect(Right { value: 42 }) }`,
  ),
  one(
    'matching',
    'ok · Exclusive mutable match',
    `struct Token { kind: i32 }
pub fn main() -> i32 { let mut token = Token { kind: 42 } return match &mut token { Token { kind } => kind } }`,
  ),
  one(
    'matching',
    'fail · Incomplete match',
    `struct Left {}
struct Right {}
fn inspect(input: Left | Right) -> i32 { return match &input { Left {} => 1 } }
pub fn main() -> i32 { return 0 }`,
  ),
  one(
    'matching',
    'fail · Unreachable match arm',
    `struct Token { value: i32 }
fn inspect(input: Token) -> i32 { return match &input { Token { value } => value Token { value: other } => other } }
pub fn main() -> i32 { return 0 }`,
  ),
  one(
    'matching',
    'fail · Invalid guard and join',
    `struct Left {}
struct Right {}
fn inspect(input: Left | Right) -> i32 { return match &input { Left {} if 1 => 1 Left {} => 1 Right {} => true } }
pub fn main() -> i32 { return 0 }`,
  ),
  one(
    'matching',
    'fail · Borrow escape and immutable exclusive',
    `struct Token { value: Token }
fn escape(input: Token) -> Token { return match &input { Token { value } => value } }
fn exclusive(input: Token) -> i32 { return match &mut input { Token { .. } => 0 } }
pub fn main() -> i32 { return 0 }`,
  ),

  // ---- mutation and structured loops ----------------------------------------------------
  one(
    'control',
    'fail · Immutable write rejection',
    `pub fn main() -> i32 { let value = 1 value = 2 return value }`,
  ),
  one(
    'control',
    'ok · Scalar mutation',
    `pub fn main() -> i32 { let mut value = 40 value = value + 2 return value }`,
  ),
  one(
    'control',
    'ok · Field mutation',
    `struct Pair { left: i32 right: i32 }
pub fn main() -> i32 { let mut pair = Pair { left: 1, right: 42 } pair.left = 40 return pair.left + 2 }`,
  ),
  one(
    'control',
    'ok · Indexed mutation',
    `pub fn main() -> i32 { let mut values = [1, 2, 3] let index = usize.add(1, 0) values[index] = 42 return values[1] }`,
  ),
  one(
    'control',
    'ok · Move-only replacement',
    `struct Token { value: i32 }
pub fn main() -> i32 { let mut token = Token { value: 1 } token = Token { value: 42 } return token.value }`,
  ),
  one(
    'control',
    'ok · Zero-iteration loop',
    `pub fn main() -> i32 { let mut value = 42 while false { value = 0 } return value }`,
  ),
  one(
    'control',
    'ok · Counting loop',
    `pub fn main() -> i32 { let mut value = 0 while value < 42 { value = value + 1 } return value }`,
  ),
  one(
    'control',
    'ok · Nested loops',
    `pub fn main() -> i32 {
  let mut outer = 0
  let mut total = 0
  while outer < 6 {
    let mut inner = 0
    while inner < 7 { total = total + 1 inner = inner + 1 }
    outer = outer + 1
  }
  return total
}`,
  ),
  one(
    'control',
    'ok · Conditional break',
    `pub fn main() -> i32 { let mut value = 0 while true { if value == 42 { break } value = value + 1 } return value }`,
  ),
  one(
    'control',
    'ok · Continue',
    `pub fn main() -> i32 { let mut value = 0 while value < 42 { value = value + 1 if value < 42 { continue } } return value }`,
  ),
  one(
    'control',
    'ok · Early loop return',
    `pub fn main() -> i32 { while true { return 42 } return 0 }`,
  ),
  one(
    'control',
    'trap · Write bounds trap',
    `pub fn main() -> i32 { let mut values = [1, 2] let index = usize.add(2, 0) values[index] = 42 return 0 }`,
  ),
  one('control', 'fail · Invalid loop condition', 'pub fn main() -> i32 { while 1 { break } return 42 }'),
  one('control', 'fail · Invalid transfer', 'pub fn main() -> i32 { continue return 42 }'),
  one(
    'control',
    'fail · Incompatible loop owner',
    `struct Token { value: i32 }
pub fn main() -> i32 {
  let mut token = Token { value: 1 }
  let mut iteration = 0
  while iteration < 1 {
    if iteration == 0 { let old = move token continue }
    iteration = iteration + 1
  }
  return 42
}`,
  ),

  // ---- discovery ------------------------------------------------------------------------
  one(
    'discovery',
    'ok · Nested calls (discovery)',
    `${identity}
pub fn main() -> i32 { return identity(identity(42)) }`,
  ),
  one(
    'discovery',
    'trap · Mutual recursion',
    `pub fn main() -> i32 { return other() }
pub fn other() -> i32 { return main() }`,
  ),
  one(
    'discovery',
    'ok · Unreachable declaration',
    `pub fn unused() -> i32 { return 1 }
pub fn main() -> i32 { return 42 }`,
  ),
  one('discovery', 'trap · Missing entry (discovery)', 'pub fn answer() -> i32 { return 42 }'),

  // ---- backend --------------------------------------------------------------------------
  one(
    'backend',
    'ok · Nested calls',
    `${identity}
pub fn main() -> i32 { return identity(identity(42)) }`,
  ),
  one(
    'backend',
    'ok · Two parameters (backend)',
    `pub fn choose(left: i32, right: i32) -> i32 { return right }
pub fn main() -> i32 { return choose(1, 42) }`,
  ),
  one('backend', 'fail · Trap body', 'pub fn main() -> i32 { return missing() }'),
  one(
    'backend',
    'ok · Branch diamond',
    'pub fn main() -> i32 { if i32.equals(1, 1) { return 42 } return 0 }',
  ),
  one('backend', 'ok · Checked arithmetic', 'pub fn main() -> i32 { return i32.divide(i32.add(40, 2), 1) }'),
  one('backend', 'trap · Overflow traps', 'pub fn main() -> i32 { return i32.add(2147483647, 1) }'),
  one('backend', 'trap · Divide by zero traps', 'pub fn main() -> i32 { return i32.divide(1, 0) }'),
  one(
    'syntax',
    'fail · Union normalization + never',
    `struct A {}
struct B {}
fn normalized(value: B | (A | B)) -> A | B { return value }
fn impossible(value: never) -> never { return value }
pub fn main() -> i32 { return 42 }`,
  ),
  one(
    'structs',
    'ok · Union injection + widening',
    `struct A {}
struct B { value: i32 }
struct C { left: i32 right: i32 }
fn accept(value: A | B | C) -> i32 { return 42 }
fn widen(value: A | B) -> i32 { return accept(move value) }
pub fn main() -> i32 { return widen(A {}) }`,
  ),
  one(
    'arrays',
    'ok · Union array containment',
    `struct A {}
struct B {}
fn accept(values: [A | B; 2]) -> i32 { return 42 }
pub fn main() -> i32 { return accept([A {}, B {}]) }`,
  ),
  one(
    'ownership',
    'ok · Union field replacement',
    `struct A {}
struct B { value: i32 }
struct Box { value: A | B }
pub fn main() -> i32 {
  let mut box = Box { value: A {} }
  box.value = B { value: 42 }
  return 42
}`,
  ),
  one(
    'syntax',
    'fail · Invalid union member',
    'fn broken(value: i32 | never) -> i32 { return 0 }\npub fn main() -> i32 { return 42 }',
  ),
  one(
    'syntax',
    'fail · Unavailable union member',
    'fn broken(value: Missing | never) -> i32 { return 0 }\npub fn main() -> i32 { return 42 }',
  ),
]

/** Presets in catalog order, grouped by phase, for the picker. */
export const presetGroups: ReadonlyArray<readonly [string, ReadonlyArray<Preset>]> = [
  ...presets
    .reduce((groups, preset) => {
      const existing = groups.get(preset.group)
      if (existing === undefined) groups.set(preset.group, [preset])
      else existing.push(preset)
      return groups
    }, new Map<string, Array<Preset>>())
    .entries(),
]
