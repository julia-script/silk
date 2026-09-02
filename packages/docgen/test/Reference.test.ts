import { assert, it } from '@effect/vitest'
import * as Analysis from '@silklang/compiler/Analysis'
import * as Effect from 'effect/Effect'
import * as Project from '../src/Project.js'
import * as Reference from '../src/Reference.js'

const encoder = new TextEncoder()

const source = `//! Coordinates recoverable work.
//!
//! # Details
//! - Operations preserve source order.
//! - See [\`Recovery\`] for the provider contract.

/// Recovers one numeric problem.
///
/// # Details
/// The provider receives the caller's value unchanged.
pub service Recovery {
  /// Recovers one problem code.
  effect fn recover(
    /// The problem code interpreted by the selected provider.
    problem: i32,
    fallback: i32,
  ) -> i32 ? &mut Recovery
}

/// A recovery provider used by examples.
pub struct Provider {}

effect fn recoverWithProvider(self: &mut Provider, problem: i32, fallback: i32) -> i32 {
  return problem
}

/// Selects Provider as a Recovery implementation.
impl Recovery for Provider {
  /// Routes Recovery.recover to Provider's implementation.
  recover: Provider.recoverWithProvider
}

/// Returns a value without changing it.
///
/// # Examples
/// ## Preserve an integer
/// \`\`\`silk
/// pub fn main() -> i32 { return 1 }
/// \`\`\`
/// ## Preserve another integer
/// \`\`\`silk
/// pub fn main() -> i32 { return 2 }
/// \`\`\`
///
/// # See also
/// - [\`Recovery\`] can interpret a value.
/// - [\`Missing\`] remains readable when unresolved.
pub fn identity<
  /// The owned value type preserved by the function.
  T,
>(
  /// The owned value returned to the caller.
  value: T,
) -> T { return move value }

/// Private implementation helper.
fn helper() -> i32 { return 0 }

/// Computes an absolute value through libc.
pub unsafe extern "C" fn cAbs(value: i32) -> i32 as "abs"
`

const rendered = Effect.fnUntraced(function* () {
  const snapshot = yield* Analysis.ofSource('test/reference', encoder.encode(source))
  const project = Project.make(snapshot)
  return Reference.make([{ module: 'test/reference', namespace: 'Recovery' }], project)
})

it.effect('renders the complete public hierarchy in source order with accurate counts', () =>
  Effect.gen(function* () {
    const result = yield* rendered()
    assert.strictEqual(result._tag, 'Success')
    if (result._tag !== 'Success') return
    const index = result.reference.files.find((file) => file.path === 'index.md')?.contents
    assert.isDefined(index)
    assert.include(index, '[Language reference](../../reference/)')
    const page = result.reference.files.find((file) => file.path === 'reference.md')?.contents
    assert.isDefined(page)
    assert.include(page, 'Import as `Recovery` with `import test.reference { Recovery }`.')
    assert.include(page, 'Public declarations: 4.')
    assert.include(page, '## `cAbs`')
    assert.include(page, 'pub unsafe extern "C" fn cAbs(value: i32) -> i32 as "abs"')
    assert.include(page, 'Computes an absolute value through libc.')
    assert.include(page, '### Operation `recover`')
    assert.include(page, '#### Parameter `problem`')
    assert.notInclude(page, 'Parameter `fallback`')
    assert.include(page, '### Type parameter `T`')
    assert.include(page, '### Parameter `value`')
    assert.include(page, '## Implementation `Recovery for Provider`')
    assert.notInclude(page, 'helper')
    assert.isBelow(page.indexOf('`Recovery`'), page.indexOf('`Provider`'))
    assert.isBelow(page.indexOf('`Provider`'), page.indexOf('Implementation'))
    assert.isBelow(page.indexOf('Implementation'), page.indexOf('`identity`'))
  }),
)

it.effect('keeps primitive module imports unscoped', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource('test/reference', encoder.encode(source))
    const project = Project.make(snapshot)
    const result = Reference.make([{ module: 'test/reference', namespace: 'i32' }], project)
    assert.strictEqual(result._tag, 'Success')
    if (result._tag !== 'Success') return
    const page = result.reference.files.find((file) => file.path === 'reference.md')?.contents
    assert.isDefined(page)
    assert.include(page, 'Import as `i32` with `import test.reference`.')
  }),
)

it.effect('rebases headings, preserves lists and titled fences, and renders semantic links', () =>
  Effect.gen(function* () {
    const result = yield* rendered()
    assert.strictEqual(result._tag, 'Success')
    if (result._tag !== 'Success') return
    const page = result.reference.files.find((file) => file.path === 'reference.md')?.contents
    assert.isDefined(page)
    assert.include(page, '## Details\n\n- Operations preserve source order.')
    assert.include(page, '### Details\n\nThe provider receives')
    assert.include(page, '### Examples')
    assert.include(page, '#### Preserve an integer')
    assert.include(page, '#### Preserve another integer')
    assert.include(page, 'pub fn main() -> i32 { return 1 }')
    assert.include(page, 'pub fn main() -> i32 { return 2 }')
    assert.include(
      page,
      `[\`Recovery\`](#${Reference.declarationAnchor('test/reference::Recovery')})`,
    )
    assert.include(page, '`Missing` remains readable')
  }),
)

it.effect('produces byte-identical files for repeated input', () =>
  Effect.gen(function* () {
    const first = yield* rendered()
    const second = yield* rendered()
    assert.deepStrictEqual(first, second)
  }),
)

it.effect('groups inherent members under their owner and resolves member links', () =>
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSource(
      'test/option',
      encoder.encode(`//! Optional values; see [\`map\`] and [\`none\`].

/// An optional value.
pub union Option<T> { None, Some { pub value: T } }

impl<T> Option<T> {
  /// Transform.
  pub fn map<U>(self: Self, transform: once fn(T) -> U) -> Option<U> {
    return match move self {
      Option<T>.Some { value } => Option<U>.Some { value: transform(move value) }
      Option<T>.None => Option<U>.None
    }
  }
  /// Nothing.
  pub fn none() -> Self { return Option<T>.None }
  fn hidden() -> i32 { return 0 }
}
`),
    )
    const project = Project.make(snapshot)
    const module = project.modules.at(0)
    assert.deepStrictEqual(
      module?.items.map((item) => item.name),
      ['Option'],
    )
    const option = module?.items.at(0)
    assert.deepStrictEqual(
      option?.children
        .filter((item) => item.kind === 'Method' || item.kind === 'AssociatedFunction')
        .map((item) => [item.kind, item.name, item.documentation?.markdown]),
      [
        ['Method', 'Option.map', 'Transform.'],
        ['AssociatedFunction', 'Option.none', 'Nothing.'],
      ],
    )
    const links = module?.documentation?.blocks
      .flatMap((block) => (block._tag === 'Paragraph' ? block.children : []))
      .flatMap((inline) => (inline._tag === 'SymbolLink' ? [inline.target?.id] : []))
    assert.deepStrictEqual(links, ['test/option::Option.map', 'test/option::Option.none'])

    const result = Reference.make([{ module: 'test/option', namespace: 'Option' }], project)
    assert.strictEqual(result._tag, 'Success')
    if (result._tag !== 'Success') return
    const page = result.reference.files.find((file) => file.path === 'option.md')?.contents
    assert.isDefined(page)
    assert.include(page, 'Public declarations: 1.')
    assert.include(page, '### Method `Option.map`')
    assert.include(page, '### Associated function `Option.none`')
    assert.notInclude(page, 'hidden')
    assert.include(page, `[\`map\`](#${Reference.declarationAnchor('test/option::Option.map')})`)
    assert.isBelow(page.indexOf('## `Option`'), page.indexOf('### Method `Option.map`'))
  }),
)

const item = (id: string, start: number): Project.Item =>
  Object.freeze({
    id,
    kind: 'Struct',
    name: id,
    visibility: 'Public',
    signature: Object.freeze({ text: `pub struct ${id}` }),
    source: Object.freeze({ sourceId: 'collision', start, end: start + 1 }),
    children: Object.freeze([]),
  })

it('rejects deterministic module-path and declaration-anchor collisions', () => {
  const project: Project.Project = Object.freeze({
    schema: 'silk-documentation',
    experimental: true,
    modules: Object.freeze([
      Object.freeze({
        name: 'test/a_b',
        sourceId: 'test/a_b',
        items: Object.freeze([item('test/a_b::duplicate', 1), item('test/a_b::duplicate', 2)]),
      }),
      Object.freeze({ name: 'test/a-b', sourceId: 'test/a-b', items: Object.freeze([]) }),
    ]),
  })
  const result = Reference.make(
    [
      { module: 'test/a_b', namespace: 'First' },
      { module: 'test/a-b', namespace: 'Second' },
    ],
    project,
  )
  assert.strictEqual(result._tag, 'Failure')
  if (result._tag !== 'Failure') return
  assert.isTrue(
    result.errors.some((error) => error._tag === 'Collision' && error.kind === 'ModulePath'),
  )
  assert.isTrue(
    result.errors.some((error) => error._tag === 'Collision' && error.kind === 'DeclarationAnchor'),
  )
})
