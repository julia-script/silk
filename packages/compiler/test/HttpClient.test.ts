import { assert, it } from '@effect/vitest'
import * as Effect from 'effect/Effect'
import * as Analysis from '../src/Analysis.js'
import * as AnalysisFixture from './support/AnalysisFixture.js'

const ownershipSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.http_client as Client {Connection, ConnectionHandler, Exchange, ClientError, RequestOptions}
import silk.http_request {PreparedRequest}
import silk.http_transport {HttpTransport}
import silk.monotonic_clock {MonotonicClock}
import silk.random {Random}

effect fn invalidHead<'exchange, P>(exchange: &mut Exchange<'exchange, P>) -> ()
! ClientError | OutOfMemoryError
? &mut Allocator | &mut MonotonicClock | &mut Random
where &mut P provides &HttpTransport from &mut HttpTransport | &mut MonotonicClock | &mut Allocator | &mut Random {
  let head = run Exchange.head(&exchange.*)
  let mut bytes: [u8; 1] = [0]
  let count = run Client.readSome(&mut exchange.*, &mut bytes)
  drop count
  drop head
  return ()
}

effect<'call> fn ignoreExchange<'call, 'exchange: 'call>(
  exchange: &'call mut Exchange<'exchange, Marker>,
) -> () {
  drop exchange
  return ()
}

struct Marker {}

effect fn simultaneous<'loan>(
  connection: &'loan mut Connection<Marker>, request: &PreparedRequest,
) -> () ! ClientError | OutOfMemoryError ? &mut Allocator {
  let first = Client.withExchange<'loan, 'loan, Marker, (), never>(
    &mut connection.*, request, RequestOptions.defaults(), ignoreExchange,
  )
  let second = Client.withExchange<'loan, 'loan, Marker, (), never>(
    &mut connection.*, request, RequestOptions.defaults(), ignoreExchange,
  )
  run first
  run second
  return ()
}


struct Escaped<'escape> { connection: &'escape mut Connection<Marker> }
struct Escape<'escape> { marker: &'escape i32 }
impl<'escape> ConnectionHandler<Marker, Escaped<'escape>, never ? &mut Allocator> for Escape<'escape> {
  effect<'call> fn handle<'call>(handler: Self, connection: &'call mut Connection<Marker>) -> Escaped<'escape>
  ? &mut Allocator
  where &mut Marker provides &HttpTransport from &mut HttpTransport | &mut MonotonicClock | &mut Allocator | &mut Random,
    &mut Marker provides &HttpTransport from &mut HttpTransport {
    drop handler
    return Escaped<'escape> {connection: move connection}
  }
}

`

it.effect(
  'rejects escaping session loans, simultaneous exchanges and invalidated head views',
  () =>
    Effect.gen(function* () {
      const snapshot = yield* AnalysisFixture.declarations(
        'http-client/ownership',
        new TextEncoder().encode(ownershipSource),
      )
      const diagnostics = Analysis.diagnostics(snapshot).map((diagnostic) => ({
        code: diagnostic.code,
        span: ownershipSource.slice(diagnostic.span.start, diagnostic.span.end).trim(),
      }))
      assert.deepEqual(diagnostics, [
        { code: 'OWN0010', span: '&mut exchange.*' },
        { code: 'OWN0010', span: '&mut connection.*' },
        { code: 'SEM0025', span: 'move connection' },
      ])
    }),
  180_000,
)
