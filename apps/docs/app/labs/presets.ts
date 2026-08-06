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
