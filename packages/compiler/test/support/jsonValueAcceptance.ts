const zeros = (count: number): string => `[${Array(count).fill(0).join(', ')}]`
const deepestDocument = `${'['.repeat(64)}0${']'.repeat(64)}`

/** One native program exercises owned JSON parsing, escapes, accessors, and writer round trips. */
export const jsonValueAcceptanceSource = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.json_output { JsonOptions }
import silk.json_scanner { JsonError, JsonReason }
import silk.json_value { Json, Value }
import silk.layout { Layout }
import silk.option { Option }
import silk.result { Result }
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

effect fn rejectsDuplicate() -> bool ! OutOfMemoryError ? &mut Allocator {
  let result = run Effect.result(Json.parse(b"{\\\"a\\\":1,\\\"\\\\u0061\\\":2}"))
  return match move result {
    Result<Value, JsonError | OutOfMemoryError>.Success { value: _ } => false
    Result<Value, JsonError | OutOfMemoryError>.Failure { error } => match move error {
      JsonError problem => {
        let atSecondKey = match move problem.offset {
          Option<usize>.Some { value } => value == 7
          Option<usize>.None => false
        }
        return problem.reason == JsonReason.DuplicateKey && atSecondKey
      }
      OutOfMemoryError other => false
    }
  }
}

struct RejectAllocator {
  calls: i32
  rejectAt: i32
}
effect fn allocate(self: &mut RejectAllocator, layout: Layout) -> Allocation ! OutOfMemoryError {
  let ordinal = self.calls
  self.calls = self.calls + 1
  if ordinal == self.rejectAt { return run Allocator.outOfMemory() }
  let mut inner = Allocator.systemAllocatorProvider()
  return run Allocator.allocate(move layout) |> Effect.provideMut<Allocator>(&mut inner)
}
impl Allocator for RejectAllocator { allocate: RejectAllocator.allocate }

effect fn allocationFailures() -> bool {
  let input = b"{\\\"outer\\\":[\\\"first\\\",{\\\"number\\\":12.5},\\\"last\\\"]}"
  let mut complete = RejectAllocator { calls: 0, rejectAt: 1000000 }
  let initial = run Effect.result(Json.parse(input) |> Effect.provideMut<Allocator>(&mut complete))
  match move initial {
    Result<Value, JsonError | OutOfMemoryError>.Success { value: _ } => {}
    Result<Value, JsonError | OutOfMemoryError>.Failure { error: _ } => { return false }
  }
  let mut ordinal = 0
  while ordinal < complete.calls {
    let mut rejected = RejectAllocator { calls: 0, rejectAt: ordinal }
    let attempted = run Effect.result(Json.parse(input) |> Effect.provideMut<Allocator>(&mut rejected))
    match move attempted {
      Result<Value, JsonError | OutOfMemoryError>.Success { value: _ } => { return false }
      Result<Value, JsonError | OutOfMemoryError>.Failure { error } => match move error {
        OutOfMemoryError out => {}
        JsonError problem => { return false }
      }
    }
    if rejected.calls != ordinal + 1 { return false }
    ordinal = ordinal + 1
  }
  return true
}

effect fn rejectsDepth(document: &Value) -> bool {
  let options = JsonOptions { indent: 0, depth: 0, depthLimit: 0 }
  let mut output = capture()
  let result = run Effect.result(Json.writeWith(document, &options) |> Effect.provideMut<Writer>(&mut output))
  return match move result {
    Result<(), JsonError | WriterError>.Success { value: _ } => false
    Result<(), JsonError | WriterError>.Failure { error } => match move error {
      JsonError problem => problem.reason == JsonReason.DepthExceeded && output.count == 0
      WriterError problem => false
    }
  }
}

effect fn check() -> i32 ! JsonError | OutOfMemoryError | WriterError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let source = b"{\\\"id\\\":42,\\\"name\\\":\\\"Silk\\\",\\\"music\\\":\\\"\\\\uD834\\\\uDD1E\\\",\\\"list\\\":[null,true,false,1.25e+2]}"
  let document = run Json.parse(source) |> Effect.provideMut<Allocator>(&mut allocator)
  let id = Json.field(&document, "id") |> Json.asI32 |> Option.unwrapOr<i32>(0)
  if id != 42 { return 1 }
  let signed = Json.field(&document, "id") |> Json.asI64 |> Option.unwrapOr<i64>(0)
  if signed != 42 { return 16 }
  let unsigned = Json.field(&document, "id") |> Json.asU64 |> Option.unwrapOr<u64>(0)
  if unsigned != 42 { return 17 }
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
  if !(run roundTrip(b"[null,true,false,0,\\\"x\\\",[],{}]") |> Effect.provideMut<Allocator>(&mut allocator)) { return 19 }
  if !(run roundTrip(b" \\t\\r\\n [ true ] \\t") |> Effect.provideMut<Allocator>(&mut allocator)) { return 20 }
  if !(run roundTrip(b"${deepestDocument}") |> Effect.provideMut<Allocator>(&mut allocator)) { return 21 }
  if !(run rejectsDuplicate() |> Effect.provideMut<Allocator>(&mut allocator)) { return 13 }
  if !(run allocationFailures()) { return 14 }
  if !(run rejectsDepth(&document)) { return 18 }
  let options = JsonOptions.indented(2)
  let mut indented = capture()
  run Json.writeWith(&document, &options) |> Effect.provideMut<Writer>(&mut indented)
  if indented.count <= source.length { return 15 }
  return 0
}

effect fn recover(error: JsonError | OutOfMemoryError | WriterError) -> i32 { return 90 }
pub fn main() -> i32 { return run Effect.catchAll(check(), recover) }
`
