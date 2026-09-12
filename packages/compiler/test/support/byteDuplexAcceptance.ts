/** Shared native/Wasm corpus source for bounded partial-byte transport behavior. */
export const byteDuplexAcceptanceSource = `import silk.allocator {Allocator, OutOfMemoryError}
import silk.byte_duplex {ByteDuplex, ByteIoError, ByteIoOperation, ReadTransfer}
import silk.bytes {Bytes}
import silk.effect {Effect}
import silk.memory_byte_duplex {MemoryByteDuplex, MemoryByteDuplexPhase, MemoryReadEvent, MemoryWriteAction, MemoryWriteEvent}
import silk.monotonic_clock {MonotonicClock}
import silk.option {Option}
import silk.result {Result}
import silk.system_clock {Instant, SystemClock}
import silk.u64
import silk.usize
import silk.vector {Vector}

struct VirtualClock { mark: Instant }
impl MonotonicClock for VirtualClock {
  effect fn now(self: &mut Self) -> Instant {
    return SystemClock.make(SystemClock.seconds(&self.mark), SystemClock.nanoseconds(&self.mark))
  }
  effect fn getResolution(self: &mut Self) -> u64 { return u64.ONE }
  effect fn waitUntil(self: &mut Self, when: Instant) -> () { self.mark = move when return () }
  effect fn waitFor(self: &mut Self, howLong: u64) -> () {
    self.mark = MonotonicClock.deadlineAfter(&self.mark, howLong)
    return ()
  }
}

effect fn program() -> i32 ! ByteIoError | OutOfMemoryError ? &mut Allocator | &mut MonotonicClock {
  let first = run Bytes.copy(&b"abc")
  let second = run Bytes.copy(&b"de")
  let mut reads = Vector.make<MemoryReadEvent>()
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Data {
    readyAt: SystemClock.make(0, 5), bytes: move first,
  })
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Data {
    readyAt: SystemClock.make(0, 6), bytes: move second,
  })
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.End {
    readyAt: SystemClock.make(0, 7),
  })
  run Vector.append<MemoryReadEvent>(&mut reads, MemoryReadEvent.Data {
    readyAt: SystemClock.make(0, 10), bytes: run Bytes.copy(&b"late"),
  })
  let mut writes = Vector.make<MemoryWriteEvent>()
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(0, 1), action: MemoryWriteAction.Accept {count: 2},
  })
  run Vector.append<MemoryWriteEvent>(&mut writes, MemoryWriteEvent {
    readyAt: SystemClock.make(0, 2), action: MemoryWriteAction.Accept {count: 3},
  })
  let mut duplex = run MemoryByteDuplex.make(
    move reads, move writes, 16, 16, Option.none<i32>(),
  )
  let mut input = [0, 0]
  let firstRead = run ByteDuplex.readSome(&mut input, Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut duplex)
  if input[0] != 97 || input[1] != 98 { return 1 }
  match move firstRead {
    ReadTransfer.Data {count} => { if count != 2 { return 2 } }
    ReadTransfer.End => { return 3 }
  }
  let secondRead = run ByteDuplex.readSome(&mut input, Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut duplex)
  if input[0] != 99 || input[1] != 100 { return 4 }
  match move secondRead {
    ReadTransfer.Data {count} => { if count != 2 { return 5 } }
    ReadTransfer.End => { return 6 }
  }
  let thirdRead = run ByteDuplex.readSome(&mut input, Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut duplex)
  if input[0] != 101 { return 7 }
  match move thirdRead {
    ReadTransfer.Data {count} => { if count != 1 { return 8 } }
    ReadTransfer.End => { return 9 }
  }
  let ended = run ByteDuplex.readSome(&mut input, Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut duplex)
  match move ended {
    ReadTransfer.Data {count} => { return 10 }
    ReadTransfer.End => {}
  }
  let timed = run Effect.result(
    ByteDuplex.readSome(&mut input, Option.some<Instant>(SystemClock.make(0, 10)))
      |> Effect.provideMut<ByteDuplex>(&mut duplex)
  )
  match move timed {
    Result<ReadTransfer, ByteIoError>.Success {value} => { return 11 }
    Result<ReadTransfer, ByteIoError>.Failure {error} => match move error {
      ByteIoError.Timeout {operation} => { if operation != ByteIoOperation.Read { return 12 } }
      ByteIoError.InvalidTransferCount {operation, count, limit} => { return 13 }
      ByteIoError.Closed {operation} => { return 14 }
      ByteIoError.Provider {operation, code} => { return 15 }
    }
  }
  let sentFirst = run ByteDuplex.writeSome(&b"hello", Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut duplex)
  if sentFirst != 2 { return 16 }
  let sentSecond = run ByteDuplex.writeSome(&b"llo", Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut duplex)
  if sentSecond != 3 { return 17 }
  let empty = run ByteDuplex.writeSome(&b"", Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut duplex)
  if empty != 0 { return 18 }
  run ByteDuplex.flush(Option.none<Instant>()) |> Effect.provideMut<ByteDuplex>(&mut duplex)
  run ByteDuplex.shutdownWrite(Option.none<Instant>())
    |> Effect.provideMut<ByteDuplex>(&mut duplex)
  let outbound = MemoryByteDuplex.outbound(&duplex)
  if outbound.length != 5 || outbound[0] != 104 || outbound[4] != 111 { return 19 }
  let audit = MemoryByteDuplex.audit(&duplex)
  if audit.length != 8 { return 20 }
  if audit[6].operation != ByteIoOperation.Flush { return 21 }
  if audit[7].operation != ByteIoOperation.ShutdownWrite { return 22 }
  run ByteDuplex.close() |> Effect.provideMut<ByteDuplex>(&mut duplex)
  run ByteDuplex.close() |> Effect.provideMut<ByteDuplex>(&mut duplex)
  if MemoryByteDuplex.closeAttempts(&duplex) != 2 { return 23 }
  if MemoryByteDuplex.phase(&duplex) != MemoryByteDuplexPhase.Closed { return 24 }
  return 42
}

effect fn failed<E>(error: E) -> i32 { drop error return -1 }
pub fn main() -> i32 {
  let mut allocator = Allocator.systemAllocatorProvider()
  let mut clock = VirtualClock {mark: SystemClock.make(0, 0)}
  return run Effect.catchAll(
    program()
      |> Effect.provideMut<Allocator>(&mut allocator)
      |> Effect.provideMut<MonotonicClock>(&mut clock),
    failed,
  )
}`
