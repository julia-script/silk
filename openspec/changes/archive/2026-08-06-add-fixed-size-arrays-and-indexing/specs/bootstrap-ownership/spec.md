## ADDED Requirements

### Requirement: Array ownership is recursively element-derived

Ownership checking SHALL classify an array as Copy only when its element type is Copy and otherwise
as a move-only whole owner. A whole-array move SHALL end the source liveness and transfer cleanup;
ordinary use of a Copy array SHALL leave the source live.

#### Scenario: Move a struct array

- **WHEN** `let next = move current` transfers an `Array<Token, 4>`
- **THEN** only `next` remains live and owns the complete index-ordered cleanup obligation

### Requirement: Indexed non-Copy extraction is a partial move

Ownership SHALL allow a non-consuming read of a Copy leaf through any valid index/field place chain
without consuming the root owner. It SHALL reject consuming access whose selected indexed value is
not Copy, because this slice has no replacement or complete array destructuring.

#### Scenario: Read then move the complete array

- **WHEN** code reads `tokens[index].kind` and later moves the complete `tokens` array
- **THEN** the field read leaves `tokens` live and the later whole move succeeds

### Requirement: Array cleanup is index-ordered and exact

Cleanup plans SHALL retain one whole-array release with recursive element cleanup in ascending index
order. Zero-length and Copy-only arrays SHALL still produce explicit complete cleanup facts even when
they emit no runtime release action.

#### Scenario: Plan zero-length cleanup

- **WHEN** a live `Array<Token, 0>` reaches a structured exit
- **THEN** its cleanup fact is complete and contains zero element actions
