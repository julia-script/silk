/**
 * Every program the per-phase labs shipped as a preset, in one catalog.
 *
 * These are the cases each phase was built against — the recovery paths, the ambiguity, the
 * cycles, the traps. They are the fastest way to put the compiler in a specific state, so they
 * are worth keeping complete rather than trimming to a handful that fit in a button bar.
 *
 * Everything is modelled as a module map even when there is only one module, so the multi-module
 * presets are not a special case: `modules` plus a `root` is what the driver takes.
 */

export interface Preset {
  readonly label: string
  /** Which phase this program was written to exercise; used to group the picker. */
  readonly group: string
  readonly root: string
  readonly modules: Readonly<Record<string, string>>
}

/** Single-module presets are the common case; this keeps the table readable. */
const one = (group: string, label: string, source: string): Preset => ({
  label,
  group,
  root: 'main',
  modules: { main: source },
})

const mainFn = 'pub fn main() -> I32 { return 42 }'

const identity = 'pub fn identity(value: I32) -> I32 { return value }'

export const presets: ReadonlyArray<Preset> = [
  // ---- syntax ---------------------------------------------------------------------------
  one('syntax', 'Literal result', 'pub fn main() -> I32 { return 42 }'),
  one(
    'syntax',
    'Two functions',
    `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return 0 }`,
  ),
  one(
    'syntax',
    'Three functions',
    `pub fn one() -> I32 { return 1 }
pub fn two() -> I32 { return 2 }
pub fn three() -> I32 { return 3 }`,
  ),
  one(
    'syntax',
    'Resolved backward',
    `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return answer() }`,
  ),
  one(
    'syntax',
    'Resolved forward',
    `pub fn main() -> I32 { return answer() }
pub fn answer() -> I32 { return 42 }`,
  ),
  one('syntax', 'Direct cycle', 'pub fn main() -> I32 { return main() }'),
  one('syntax', 'Unknown call', 'pub fn main() -> I32 { return missing() }'),
  one(
    'syntax',
    'Ambiguous call',
    `pub fn same() -> I32 { return 1 }
pub fn same() -> I32 { return 2 }
pub fn main() -> I32 { return same() }`,
  ),
  one('syntax', 'Missing callee', 'pub fn main() -> I32 { return () }'),
  one(
    'syntax',
    'Missing call )',
    `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return answer( }`,
  ),
  one(
    'syntax',
    'Resolved parameter',
    `${identity}
pub fn main() -> I32 { return identity(42) }`,
  ),
  one(
    'syntax',
    'Flow unknown reference',
    `pub fn identity(value: I32) -> I32 { return missing }
pub fn main() -> I32 { return identity(42) }`,
  ),
  one(
    'syntax',
    'Flow ambiguous reference',
    `pub fn choose(value: I32, value: I32) -> I32 { return value }
pub fn main() -> I32 { return choose(1, 2) }`,
  ),
  one(
    'syntax',
    'Flow damaged syntax',
    `${identity}
pub fn main() -> I32 { return identity(@) }`,
  ),
  one(
    'syntax',
    'Wrong arity',
    `pub fn choose(left: I32, right: I32) -> I32 { return left }
pub fn main() -> I32 { return choose(1) }`,
  ),
  one(
    'syntax',
    'Too many arguments',
    `${identity}
pub fn main() -> I32 { return identity(1, 2) }`,
  ),
  one(
    'syntax',
    'Unavailable evaluation',
    `pub fn identity(value: Mystery) -> I32 { return 0 }
pub fn main() -> I32 { return identity(42) }`,
  ),
  one(
    'syntax',
    'Second argument result',
    `pub fn second(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return second(10, 42) }`,
  ),
  one('syntax', 'Missing entry', 'pub fn answer() -> I32 { return 42 }'),
  one(
    'syntax',
    'Mutual cycle',
    `pub fn main() -> I32 { return other() }
pub fn other() -> I32 { return main() }`,
  ),
  one('syntax', 'Unresolved contract call', 'pub fn main() -> I32 { return missing(42) }'),
  one('syntax', 'Unknown parameter', 'pub fn main() -> I32 { return missing }'),
  one(
    'syntax',
    'Duplicate parameter',
    'pub fn choose(value: I32, value: I32) -> I32 { return value }',
  ),
  one(
    'syntax',
    'Cross-function parameter',
    `pub fn owner(value: I32) -> I32 { return value }
pub fn other() -> I32 { return value }`,
  ),
  one('syntax', 'Recovered reference', 'pub fn identity(value: I32) -> I32 { return @ value }'),
  one('syntax', 'Two parameters', 'pub fn choose(left: I32, right: I32) -> I32 { return left }'),
  one(
    'syntax',
    'Identifier argument',
    `${identity}
pub fn forward(value: I32) -> I32 { return identity(value) }`,
  ),
  one(
    'syntax',
    'Nested flow · complete',
    `${identity}
pub fn main() -> I32 { return identity(identity(42)) }`,
  ),
  one(
    'syntax',
    'Nested flow · siblings',
    `${identity}
pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(identity(1), identity(2)) }`,
  ),
  one(
    'syntax',
    'Nested flow · unavailable',
    `${identity}
pub fn uncertain(value: Mystery) -> I32 { return 0 }
pub fn main() -> I32 { return identity(uncertain(42)) }`,
  ),
  one(
    'syntax',
    'Nested flow · wrong arity',
    `${identity}
pub fn main() -> I32 { return identity(identity()) }`,
  ),
  one(
    'syntax',
    'Damaged nested call',
    `${identity}
pub fn main() -> I32 { return identity(identity(@)) }`,
  ),
  one(
    'syntax',
    'Nested flow · inner blocked',
    `${identity}
pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(identity(1), missing(2)) }`,
  ),
  one(
    'syntax',
    'Nested flow · cycle',
    `${identity}
pub fn main() -> I32 { return identity(main()) }`,
  ),
  one('syntax', 'Missing parameter type', 'pub fn identity(value:) -> I32 { return value }'),
  one(
    'syntax',
    'Missing parameter comma',
    'pub fn choose(left: I32 right: I32) -> I32 { return left }',
  ),
  one('syntax', 'Malformed argument', 'pub fn main(value: I32) -> I32 { return missing(@, value) }'),
  one(
    'syntax',
    'Missing name',
    `pub fn answer() -> I32 { return 42 }
pub fn () -> I32 { return 0 }`,
  ),
  one(
    'syntax',
    'Duplicate names',
    `pub fn same() -> I32 { return 1 }
pub fn same() -> I32 { return 2 }`,
  ),
  one(
    'syntax',
    'Mixed damage',
    `pub fn main() -> I32 { return 42 }
pub fn damaged() -> Mystery { return 2147483648 }`,
  ),
  one(
    'syntax',
    'Missing first }',
    `pub fn answer() -> I32 { return 42
pub fn main() -> I32 { return 0 }`,
  ),
  one('syntax', 'Missing }', 'pub fn main() -> I32 { return 42'),
  one('syntax', 'Unexpected @', 'pub fn @ main() -> I32 { return 42 }'),
  one('syntax', 'Unknown type', 'pub fn main() -> Mystery { return 42 }'),
  one('syntax', 'I32 overflow', 'pub fn main() -> I32 { return 2147483648 }'),
  one('syntax', 'UTF-8', 'pub fn café() -> I32 { return 42 }'),

  // ---- modules --------------------------------------------------------------------------
  {
    label: 'Diamond',
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
    label: 'Mutual cycle (imports)',
    group: 'modules',
    root: 'root',
    modules: {
      root: `import beta\n${mainFn}`,
      beta: `import gamma\n${mainFn}`,
      gamma: `import beta\n${mainFn}`,
    },
  },
  {
    label: 'Unknown import',
    group: 'modules',
    root: 'root',
    modules: { root: `import missing\n${mainFn}` },
  },
  {
    label: 'Self import',
    group: 'modules',
    root: 'root',
    modules: { root: `import root\n${mainFn}` },
  },
  {
    label: 'Unreachable island',
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
    label: 'Two modules',
    group: 'headers',
    root: 'root',
    modules: {
      root: 'import lib\npub fn main() -> I32 { return 42 }',
      lib: 'pub fn answer() -> I32 { return 1 }\npub fn choose(left: I32, right: I32) -> I32 { return left }',
    },
  },
  {
    label: 'Same name twice across modules',
    group: 'headers',
    root: 'root',
    modules: {
      root: 'import lib\npub fn answer() -> I32 { return 1 }',
      lib: 'pub fn answer() -> I32 { return 2 }',
    },
  },
  {
    label: 'Duplicate in one module',
    group: 'headers',
    root: 'root',
    modules: { root: 'pub fn same() -> I32 { return 1 }\npub fn same() -> I32 { return 2 }' },
  },
  {
    label: 'Missing name (header)',
    group: 'headers',
    root: 'root',
    modules: { root: 'pub fn () -> I32 { return 0 }' },
  },
  {
    label: 'Unknown types',
    group: 'headers',
    root: 'root',
    modules: { root: 'pub fn puzzle(value: Mystery) -> Enigma { return 0 }' },
  },

  // ---- structs --------------------------------------------------------------------------
  one('structs', 'Empty struct', 'struct Marker {}\npub fn main() -> I32 { return 42 }'),
  one(
    'structs',
    'Nested physical layout',
    'struct Pair { left: I32 right: Bool }\nstruct Outer { pair: Pair value: I32 }\npub fn main() -> I32 { return 42 }',
  ),
  {
    label: 'Imported nominal type',
    group: 'structs',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import model.Tree as Ast { Node }\nstruct Root { selected: Node qualified: Ast.Node }\npub fn main() -> I32 { return 42 }',
      'model/Tree': 'pub struct Node { value: I32 }',
    },
  },
  one(
    'structs',
    'Private type exposure',
    'struct Hidden { value: I32 }\npub struct Visible { pub hidden: Hidden }\npub fn main() -> I32 { return 42 }',
  ),
  one(
    'structs',
    'Damaged field',
    'struct Broken { value: Missing next: I32 }\npub fn main() -> I32 { return 42 }',
  ),
  one(
    'structs',
    'Recursive structs',
    'struct Left { right: Right }\nstruct Right { left: Left }\npub fn main() -> I32 { return 42 }',
  ),
  one(
    'structs',
    'Construct and project fields',
    `struct Pair { left: I32 right: I32 }
fn make() -> Pair { return Pair { right: 2, left: 1 } }
pub fn main() -> I32 { let pair = make() return pair.right }`,
  ),
  one(
    'structs',
    'Nested projections',
    `struct Inner { value: I32 }
struct Outer { inner: Inner }
fn make() -> Outer { return Outer { inner: Inner { value: 42 } } }
pub fn main() -> I32 { let outer = make() return outer.inner.value }`,
  ),
  one(
    'structs',
    'Zero-lane struct value',
    `struct Marker {}
fn marker() -> Marker { return Marker {} }
pub fn main() -> I32 { let value = marker() return 42 }`,
  ),
  one(
    'structs',
    'Invalid struct literal',
    `struct Pair { left: I32 right: I32 }
fn broken() -> Pair { return Pair { left: true, left: 2, extra: 3 } }
pub fn main() -> I32 { return 42 }`,
  ),
  {
    label: 'Public struct factory',
    group: 'structs',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import model.Pair { Pair, make }\npub fn main() -> I32 { let pair = make(20, 22) return pair.right }',
      'model/Pair':
        'pub struct Pair { pub left: I32 pub right: I32 }\npub fn make(left: I32, right: I32) -> Pair { return Pair { right: right, left: left } }',
    },
  },

  // ---- names ----------------------------------------------------------------------------
  // Namespaced imports, aliases, and selective member lists: every binding form the resolver
  // has to answer for, plus the ways one can fail to bind.
  {
    label: 'Namespace import',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main': 'import compiler.Syntax\npub fn main() -> I32 { return Syntax.parse() }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }',
    },
  },
  {
    label: 'Selective alias',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import compiler.Syntax { parse as read }\npub fn main() -> I32 { return read() }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }',
    },
  },
  {
    label: 'Hybrid alias',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import compiler.Syntax as Tree { parse }\npub fn main() -> I32 { return Tree.parse() }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }',
    },
  },
  {
    label: 'Private member',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main': 'import compiler.Syntax { hidden }\npub fn main() -> I32 { return 0 }',
      'compiler/Syntax': 'fn hidden() -> I32 { return 42 }',
    },
  },
  {
    label: 'Unknown member',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import compiler.Syntax { missing }\npub fn main() -> I32 { return missing() }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }',
    },
  },
  {
    label: 'Damaged alias',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main': 'import compiler.Syntax as\npub fn main() -> I32 { return 0 }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }',
    },
  },
  {
    label: 'Import collision',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main':
        'import compiler.Syntax { parse }\npub fn parse() -> I32 { return 0 }\npub fn main() -> I32 { return parse() }',
      'compiler/Syntax': 'pub fn parse() -> I32 { return 42 }',
    },
  },
  {
    label: 'Import cycle (names)',
    group: 'names',
    root: 'app/Main',
    modules: {
      'app/Main': 'import cycle.A { a }\npub fn main() -> I32 { return a() }',
      'cycle/A': 'import cycle.B { b }\npub fn a() -> I32 { return b() }',
      'cycle/B': 'import cycle.A { a }\npub fn b() -> I32 { return a() }',
    },
  },

  // ---- operators ------------------------------------------------------------------------
  one('operators', 'Operator precedence', 'pub fn main() -> I32 { return 2 + 5 * 8 }'),
  one('operators', 'Pipeline', 'pub fn main() -> I32 { return 2 |> I32.add(40) }'),
  one(
    'operators',
    'Unary bool pipeline',
    'pub fn main() -> I32 { if true |> Bool.not { return 0 } return 42 }',
  ),
  one('operators', 'Bool not', 'pub fn main() -> I32 { if !(1 == 2) { return 42 } return 0 }'),
  one('operators', 'Negation overflow traps', 'pub fn main() -> I32 { return -(-2147483648) }'),
  one(
    'operators',
    'Closed operator surface',
    `pub fn main() -> I32 {
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

  // ---- arrays ---------------------------------------------------------------------------
  one('arrays', 'Array inferred', 'pub fn main() -> I32 { let values = [10, 42] return values[1] }'),
  one(
    'arrays',
    'Array contextual',
    `fn values() -> Array<I32, 2> { return [10, 42] }
pub fn main() -> I32 { return values()[1] }`,
  ),
  one(
    'arrays',
    'Array empty',
    `fn empty() -> Array<I32, 0> { return [] }
fn consume(values: Array<I32, 0>) -> I32 { return 42 }
pub fn main() -> I32 { return consume(empty()) }`,
  ),
  one(
    'arrays',
    'Array nested',
    `fn choose(values: Array<Array<I32, 2>, 2>, outer: I32, inner: I32) -> I32 { return values[outer][inner] }
pub fn main() -> I32 { return choose([[10, 11], [42, 43]], 1, 0) }`,
  ),
  one(
    'arrays',
    'Array struct elements',
    `struct Pair { left: I32 right: I32 }
pub fn main() -> I32 { let values = [Pair { left: 10, right: 11 }, Pair { left: 42, right: 43 }] return values[1].left }`,
  ),
  one(
    'arrays',
    'Array evaluation order',
    `fn first() -> I32 { return 10 }
fn second() -> I32 { return 42 }
pub fn main() -> I32 { let values = [first(), second()] return values[1] }`,
  ),
  one(
    'arrays',
    'Array whole moved',
    `struct Token { value: I32 }
pub fn main() -> I32 { let values = [Token { value: 10 }, Token { value: 42 }] let moved = move values return moved[1].value }`,
  ),
  one(
    'arrays',
    'Array Copy read',
    `fn choose(values: Array<I32, 2>, index: I32) -> I32 { let selected = values[index] return selected }
pub fn main() -> I32 { return choose([10, 42], 1) }`,
  ),
  one(
    'arrays',
    'Array indexed field',
    `struct Pair { left: I32 right: I32 }
fn choose(values: Array<Pair, 2>, index: I32) -> I32 { return values[index].left }
pub fn main() -> I32 { return choose([Pair { left: 10, right: 11 }, Pair { left: 42, right: 43 }], 1) }`,
  ),
  one(
    'arrays',
    'Array constant out of bounds',
    'pub fn main() -> I32 { let values = [10, 42] return values[2] }',
  ),
  one(
    'arrays',
    'Array dynamic trap',
    `fn choose(values: Array<I32, 2>, index: I32) -> I32 { return values[index] }
pub fn main() -> I32 { return choose([10, 42], -1) }`,
  ),
  one(
    'arrays',
    'Array type mismatch',
    'pub fn main() -> I32 { let values = [1, true] return 0 }',
  ),
  one(
    'arrays',
    'Array length mismatch',
    `fn values() -> Array<I32, 3> { return [10, 42] }
pub fn main() -> I32 { return 0 }`,
  ),
  one(
    'arrays',
    'Array partial move',
    `struct Token { value: I32 }
fn take(values: Array<Token, 2>) -> Token { return move values[0] }
pub fn main() -> I32 { return 42 }`,
  ),
  one(
    'arrays',
    'Array unavailable layout',
    `fn consume(values: Array<Array<Array<I32, 2147483647>, 2147483647>, 0>) -> I32 { return 42 }
pub fn main() -> I32 { return consume([]) }`,
  ),

  // ---- ownership ------------------------------------------------------------------------
  one(
    'ownership',
    'Let bindings',
    `${identity}
pub fn main() -> I32 { let value = identity(42) let extra = 1 return value }`,
  ),
  one(
    'ownership',
    'Moved binding',
    `${identity}
pub fn main() -> I32 { let value = 42 return identity(move value) }`,
  ),
  one(
    'ownership',
    'Use after move',
    `pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { let value = 42 return choose(move value, value) }`,
  ),
  one(
    'ownership',
    'Two parameters (ownership)',
    `pub fn choose(left: I32, right: I32) -> I32 { return left }
pub fn main() -> I32 { return choose(1, 2) }`,
  ),
  one('ownership', 'Damaged body', 'pub fn main() -> I32 { return missing() }'),
  one('ownership', 'Unknown parameter type', 'pub fn puzzle(value: Mystery) -> I32 { return value }'),

  // ---- exhaustive matching --------------------------------------------------------------
  one(
    'matching',
    'Guarded union match',
    `struct Left { value: I32 }
struct Right { value: I32 }
fn inspect(input: Left | Right) -> I32 {
  return match &input {
    Left { value } if false => 0
    Left { value: answer } => answer + 1
    Right { value } => value
  }
}
pub fn main() -> I32 { return inspect(Left { value: 41 }) }`,
  ),
  one(
    'matching',
    'Nested renamed patterns',
    `struct Token { kind: I32 }
struct Box { token: Token extra: I32 }
pub fn main() -> I32 {
  let box = Box { token: Token { kind: 42 }, extra: 0 }
  return match move box { Box { token: Token { kind: answer }, .. } => answer }
}`,
  ),
  one(
    'matching',
    'Universal fallback',
    `struct Left { value: I32 }
struct Right { value: I32 }
fn inspect(input: Left | Right) -> I32 { return match &input { Left { value } => value _ => 0 } }
pub fn main() -> I32 { return inspect(Right { value: 42 }) }`,
  ),
  one(
    'matching',
    'Exclusive mutable match',
    `struct Token { kind: I32 }
pub fn main() -> I32 { let mut token = Token { kind: 42 } return match &mut token { Token { kind } => kind } }`,
  ),
  one(
    'matching',
    'Incomplete match',
    `struct Left {}
struct Right {}
fn inspect(input: Left | Right) -> I32 { return match &input { Left {} => 1 } }
pub fn main() -> I32 { return 0 }`,
  ),
  one(
    'matching',
    'Unreachable match arm',
    `struct Token { value: I32 }
fn inspect(input: Token) -> I32 { return match &input { Token { value } => value Token { value: other } => other } }
pub fn main() -> I32 { return 0 }`,
  ),
  one(
    'matching',
    'Invalid guard and join',
    `struct Left {}
struct Right {}
fn inspect(input: Left | Right) -> I32 { return match &input { Left {} if 1 => 1 Left {} => 1 Right {} => true } }
pub fn main() -> I32 { return 0 }`,
  ),
  one(
    'matching',
    'Borrow escape and immutable exclusive',
    `struct Token { value: Token }
fn escape(input: Token) -> Token { return match &input { Token { value } => value } }
fn exclusive(input: Token) -> I32 { return match &mut input { Token { .. } => 0 } }
pub fn main() -> I32 { return 0 }`,
  ),

  // ---- mutation and structured loops ----------------------------------------------------
  one(
    'control',
    'Immutable write rejection',
    `pub fn main() -> I32 { let value = 1 value = 2 return value }`,
  ),
  one(
    'control',
    'Scalar mutation',
    `pub fn main() -> I32 { let mut value = 40 value = value + 2 return value }`,
  ),
  one(
    'control',
    'Field mutation',
    `struct Pair { left: I32 right: I32 }
pub fn main() -> I32 { let mut pair = Pair { left: 1, right: 42 } pair.left = 40 return pair.left + 2 }`,
  ),
  one(
    'control',
    'Indexed mutation',
    `pub fn main() -> I32 { let mut values = [1, 2, 3] let index = 1 values[index] = 42 return values[1] }`,
  ),
  one(
    'control',
    'Move-only replacement',
    `struct Token { value: I32 }
pub fn main() -> I32 { let mut token = Token { value: 1 } token = Token { value: 42 } return token.value }`,
  ),
  one(
    'control',
    'Zero-iteration loop',
    `pub fn main() -> I32 { let mut value = 42 while false { value = 0 } return value }`,
  ),
  one(
    'control',
    'Counting loop',
    `pub fn main() -> I32 { let mut value = 0 while value < 42 { value = value + 1 } return value }`,
  ),
  one(
    'control',
    'Nested loops',
    `pub fn main() -> I32 {
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
    'Conditional break',
    `pub fn main() -> I32 { let mut value = 0 while true { if value == 42 { break } value = value + 1 } return value }`,
  ),
  one(
    'control',
    'Continue',
    `pub fn main() -> I32 { let mut value = 0 while value < 42 { value = value + 1 if value < 42 { continue } } return value }`,
  ),
  one(
    'control',
    'Early loop return',
    `pub fn main() -> I32 { while true { return 42 } return 0 }`,
  ),
  one(
    'control',
    'Write bounds trap',
    `pub fn main() -> I32 { let mut values = [1, 2] let index = 2 values[index] = 42 return 0 }`,
  ),
  one('control', 'Invalid loop condition', 'pub fn main() -> I32 { while 1 { break } return 42 }'),
  one('control', 'Invalid transfer', 'pub fn main() -> I32 { continue return 42 }'),
  one(
    'control',
    'Incompatible loop owner',
    `struct Token { value: I32 }
pub fn main() -> I32 {
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
    'Nested calls (discovery)',
    `${identity}
pub fn main() -> I32 { return identity(identity(42)) }`,
  ),
  one(
    'discovery',
    'Mutual recursion',
    `pub fn main() -> I32 { return other() }
pub fn other() -> I32 { return main() }`,
  ),
  one(
    'discovery',
    'Unreachable declaration',
    `pub fn unused() -> I32 { return 1 }
pub fn main() -> I32 { return 42 }`,
  ),
  one('discovery', 'Missing entry (discovery)', 'pub fn answer() -> I32 { return 42 }'),

  // ---- backend --------------------------------------------------------------------------
  one(
    'backend',
    'Nested calls',
    `${identity}
pub fn main() -> I32 { return identity(identity(42)) }`,
  ),
  one(
    'backend',
    'Two parameters (backend)',
    `pub fn choose(left: I32, right: I32) -> I32 { return right }
pub fn main() -> I32 { return choose(1, 42) }`,
  ),
  one('backend', 'Trap body', 'pub fn main() -> I32 { return missing() }'),
  one(
    'backend',
    'Branch diamond',
    'pub fn main() -> I32 { if I32.equals(1, 1) { return 42 } return 0 }',
  ),
  one('backend', 'Checked arithmetic', 'pub fn main() -> I32 { return I32.divide(I32.add(40, 2), 1) }'),
  one('backend', 'Overflow traps', 'pub fn main() -> I32 { return I32.add(2147483647, 1) }'),
  one('backend', 'Divide by zero traps', 'pub fn main() -> I32 { return I32.divide(1, 0) }'),
  one(
    'syntax',
    'Union normalization + Never',
    `struct A {}
struct B {}
fn normalized(value: B | (A | B)) -> A | B { return value }
fn impossible(value: Never) -> Never { return value }
pub fn main() -> I32 { return 42 }`,
  ),
  one(
    'structs',
    'Union injection + widening',
    `struct A {}
struct B { value: I32 }
struct C { left: I32 right: I32 }
fn accept(value: A | B | C) -> I32 { return 42 }
fn widen(value: A | B) -> I32 { return accept(move value) }
pub fn main() -> I32 { return widen(A {}) }`,
  ),
  one(
    'arrays',
    'Union array containment',
    `struct A {}
struct B {}
fn accept(values: Array<A | B, 2>) -> I32 { return 42 }
pub fn main() -> I32 { return accept([A {}, B {}]) }`,
  ),
  one(
    'ownership',
    'Union field replacement',
    `struct A {}
struct B { value: I32 }
struct Box { value: A | B }
pub fn main() -> I32 {
  let mut box = Box { value: A {} }
  box.value = B { value: 42 }
  return 42
}`,
  ),
  one(
    'syntax',
    'Invalid union member',
    'fn broken(value: I32 | Never) -> I32 { return 0 }\npub fn main() -> I32 { return 42 }',
  ),
  one(
    'syntax',
    'Unavailable union member',
    'fn broken(value: Missing | Never) -> I32 { return 0 }\npub fn main() -> I32 { return 42 }',
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
