## Why

Silk can spell a structural union at every type position, but it cannot name one. A failure row
such as `! HttpError | JsonParsingError` must be repeated in full at every signature that shares it,
and a generic application such as `Point<f32>` has no short spelling either. `alpha-status.md`
lists type aliases as a known gap. The structural-union specification already reserves the rule an
alias must obey: "transparent alias spelling MUST NOT affect normalized identity when aliases become
available." This change makes them available under exactly that rule.

## What Changes

- Add the declaration `[pub] type Name = <type>`. `type` becomes a complete-identifier keyword, as
  `enum` and `union` are. No `.silk` source in the repository uses `type` as an identifier.
- An alias is transparent and erased. It introduces no new canonical type. Every use of the name
  resolves to the target's canonical type, so `type Meters = u32` and `type Kilograms = u32` are
  both `u32` and interchangeable, as in Rust and TypeScript. Signatures, module surfaces, and
  diagnostics show the expanded type, never the alias name.
- A structural-union member of a declared failure row flattens to its member types. Today
  `! A | B` yields two row members because the parser supplies two operands; with an alias the row
  receives one member whose type is a structural union, and it must yield the same two members.
  `Effect.catch<FetchError>` is therefore the same call as `Effect.catch<HttpError | JsonParsingError>`.
  A nominal `union` remains one atomic member in every position.
- An alias may name any type admitted at that position: scalars, `string`, nominal types, generic
  applications, structural unions, arrays, and callable types. An alias whose target is itself an
  alias resolves through it. A cycle among aliases is rejected at the declaration.
- A `pub` alias is exported through the module surface and resolves through imports like a nominal
  type. A `pub` alias whose target exposes a private nominal type is rejected with the ordinary
  exposure diagnostic.
- Aliases declare no type parameters in this change. `type PointF32 = Point<f32>` is accepted;
  `type Pair<T> = Point<T>` is rejected with a diagnostic naming the restriction.

## Non-goals

- **No nominal newtype.** `type Meters = u32` does not make `Meters` distinct from `u32`. The
  existing `tuple Meters(u32)` declaration is the nominal wrapper.
- **No generic aliases.** Parameterized aliases need use-site substitution, arity checks, and
  surface encoding of parameters. They are a follow-up once the transparent form is in use.
- **No alias name retention.** Hover, diagnostics, and surfaces print the expanded type. Retaining
  the spelling for presentation is a separate tooling concern.

## Capabilities

### New Capabilities

- `bootstrap-type-aliases`: the `type` declaration, its transparency and erasure, alias-through-alias
  resolution, cycle rejection, visibility and exposure rules, and the parameter restriction.

### Modified Capabilities

- `bootstrap-lexer`: `type` joins the complete-identifier keyword vocabulary.
- `bootstrap-syntax`: the type alias declaration parses losslessly and recovers locally.
- `bootstrap-name-resolution`: a type path that names an alias resolves to the alias target's
  canonical type through the same module scopes and visibility gate as nominal types.
- `bootstrap-structural-unions`: the normalization requirement's conditional alias wording becomes
  unconditional now that aliases exist.
- `bootstrap-flow-functions`: a declared failure-row member that resolves to a structural union
  contributes each union member to the row rather than one union-typed member.
- `bootstrap-module-semantic-surface`: module surfaces encode public alias declarations with their
  erased target type.
- `silk-source-formatting`: the formatter prints a type alias declaration in canonical form.

## Impact

- **Lexer and parser.** New keyword token, new `TypeAliasDeclaration` node kind, declaration-start
  and `pub`-following tables in `Parser/Declaration.ts`.
- **Declaration pipeline.** `DeclarationCollection` collects the alias header; `DeclarationCompletion`
  resolves its target with cycle detection; `NameResolution.resolveType` returns the erased target
  when a lookup hits an alias. No new `Type` variant, so layout, MIR, ownership, and all three
  backends are untouched.
- **Failure rows.** `semanticFailureRow` and `resolveFailureRow` in `DeclarationResolution.ts`
  flatten a structural-union member the way `Type.effect` already does.
- **Module surface.** `ModuleSummary.DeclarationKind` and the surface encoder gain an alias kind
  carrying the encoded target.
- **Tooling.** Formatter node-kind switch, syntax inspector, LSP structure and hover, TextMate and
  CodeMirror keyword tables, and docgen each need the new declaration kind. These follow existing
  patterns for `union` and are listed as tasks, not as new requirements.
- **Documentation.** `alpha-status.md` drops type aliases from the gap list; the tutorial or
  typed-failures reference gains the alias form.
