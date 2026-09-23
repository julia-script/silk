const zeros = (count: number): string => `[${Array(count).fill(0).join(', ')}]`

/** One native program exercises owned JSON parsing, escapes, accessors, and writer round trips. */
export const jsonValueAcceptanceSource = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.json_scanner { JsonError }
import silk.json_value { Json }
import silk.option { Option }
import silk.slice { Slice }
import silk.u8
import silk.usize
import silk.writer { Writer, WriterError }

struct Capture {
  bytes: [u8; 256]
  count: usize
}

effect fn writeAll(self: &mut Capture, values: &[u8]) -> () ! WriterError {
  if self.count + values.length > 256 { fail Writer.failure() }
  let mut index = usize.ZERO
  while index < values.length {
    self.bytes[self.count] = values[index]
    self.count = self.count + usize.ONE
    index = index + usize.ONE
  }
}

effect fn flush(self: &mut Capture) -> () ! WriterError { return () }
impl Writer for Capture { writeAll: Capture.writeAll flush: Capture.flush }

fn capture() -> Capture { return Capture { bytes: ${zeros(256)}, count: usize.ZERO } }

fn equal(left: &[u8], right: &[u8]) -> bool {
  if left.length != right.length { return false }
  let mut index = usize.ZERO
  while index < left.length {
    if left[index] != right[index] { return false }
    index = index + usize.ONE
  }
  return true
}

effect fn roundTrip(input: &[u8]) -> bool
! JsonError | OutOfMemoryError | WriterError ? &mut Allocator {
  let first = run Json.parse(input)
  let mut output = capture()
  run Json.write(&first) |> Effect.provideMut<Writer>(&mut output)
  let serialized = Slice.view<u8>(&output.bytes, usize.ZERO, output.count)
  let second = run Json.parse(serialized)
  let mut again = capture()
  run Json.write(&second) |> Effect.provideMut<Writer>(&mut again)
  return equal(serialized, Slice.view<u8>(&again.bytes, usize.ZERO, again.count))
}

effect fn check() -> i32 ! JsonError | OutOfMemoryError | WriterError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let source = b"{\\\"id\\\":42,\\\"name\\\":\\\"Silk\\\",\\\"music\\\":\\\"\\\\uD834\\\\uDD1E\\\",\\\"list\\\":[null,true,false,1.25e+2]}"
  let document = run Json.parse(source) |> Effect.provideMut<Allocator>(&mut allocator)
  let id = Json.field(&document, "id") |> Json.asI32 |> Option.unwrapOr<i32>(0)
  if id != 42 { return 1 }
  let name = Json.field(&document, "name") |> Json.asString |> Option.unwrapOr<string>("")
  if name != "Silk" { return 2 }
  let music = Json.field(&document, "music") |> Json.asString |> Option.unwrapOr<string>("")
  if music != "𝄞" { return 3 }
  let missing = Json.field(&document, "missing") |> Json.fieldAt("child") |> Json.asI32
  if Option.unwrapOr<i32>(move missing, -1) != -1 { return 4 }
  let checked = run roundTrip(source) |> Effect.provideMut<Allocator>(&mut allocator)
  if !checked { return 5 }
  if !(run roundTrip(b"null") |> Effect.provideMut<Allocator>(&mut allocator)) { return 6 }
  if !(run roundTrip(b"true") |> Effect.provideMut<Allocator>(&mut allocator)) { return 7 }
  if !(run roundTrip(b"-0") |> Effect.provideMut<Allocator>(&mut allocator)) { return 8 }
  if !(run roundTrip(b"1E-999") |> Effect.provideMut<Allocator>(&mut allocator)) { return 9 }
  if !(run roundTrip(b"[]") |> Effect.provideMut<Allocator>(&mut allocator)) { return 10 }
  if !(run roundTrip(b"{}") |> Effect.provideMut<Allocator>(&mut allocator)) { return 11 }
  if !(run roundTrip(b"[\\\"\\\\n\\\",{\\\"x\\\":[0,1]}]") |> Effect.provideMut<Allocator>(&mut allocator)) { return 12 }
  return 0
}

effect fn recover(error: JsonError | OutOfMemoryError | WriterError) -> i32 { return 90 }
pub fn main() -> i32 { return run Effect.catchAll(check(), recover) }
`
