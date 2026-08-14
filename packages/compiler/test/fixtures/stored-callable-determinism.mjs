import * as NodeRuntime from '@effect/platform-node/NodeRuntime'
import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'

/**
 * Nested and multiply-specialized stored-callable constructions: `Parser`/`Nested` violate through
 * their declared fields, and `Holder<T>` violates twice through two distinct specializations, so
 * the printed report exercises both provenance modes and the deterministic ordering between them.
 */
const source = `struct Parser<A> { decode: fn(i32) -> A }
struct Nested<A> { parser: Parser<A> }
struct Holder<T> { value: T }

fn double(value: i32) -> i32 { return value * 2 }
fn flag(value: i32) -> bool { return value > 0 }

fn make<A>(decoder: fn(i32) -> A) -> Parser<A> {
  return Parser<A> { decode: decoder }
}

fn nest<A>(parser: Parser<A>) -> Nested<A> {
  return Nested<A> { parser: move parser }
}

fn wrap<T>(value: T) -> Holder<T> { return Holder<T> { value: move value } }

pub fn main() -> i32 {
  let parser = make<i32>(i32.add(1))
  let nested = nest<i32>(move parser)
  let doubled = wrap(double)
  let flagged = wrap(flag)
  return 42
}`

NodeRuntime.runMain(
  Effect.gen(function* () {
    const snapshot = yield* Analysis.ofSourceRealized(
      'fixture/StoredCallable',
      new TextEncoder().encode(source),
      'wasm32-unknown-unknown',
    )
    yield* Console.log(
      JSON.stringify({
        diagnostics: Analysis.diagnostics(snapshot).map((diagnostic) => ({
          code: diagnostic.code,
          message: diagnostic.message,
          span: {
            sourceId: diagnostic.span.sourceId,
            start: diagnostic.span.start,
            end: diagnostic.span.end,
          },
          related: (diagnostic.relatedSpans ?? []).map((related) => ({
            label: related.label,
            sourceId: related.span.sourceId,
            start: related.span.start,
            end: related.span.end,
          })),
        })),
        layout: snapshot.layout._tag,
        mir: snapshot.mir._tag,
      }),
    )
  }),
)
