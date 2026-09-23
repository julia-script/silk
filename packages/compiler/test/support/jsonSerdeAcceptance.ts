/** One native program checks typed JSON builder lowering and round trips. */
export const jsonSerdeAcceptanceSource = `import silk.effect { Effect }
import silk.json_array { JsonArray }
import silk.json_object { JsonObject }
import silk.json_output { JsonOptions }
import silk.json_scanner { JsonError }
import silk.writer { Writer, WriterError }

struct Sink {}
effect fn writeAll(self: &mut Sink, values: &[u8]) -> () ! WriterError { return () }
effect fn flush(self: &mut Sink) -> () ! WriterError { return () }
impl Writer for Sink { writeAll: Sink.writeAll flush: Sink.flush }

effect fn writeObject(value: &i32, options: &JsonOptions)
  -> () ! JsonError | WriterError ? &mut Writer {
  return run JsonObject.begin(options)
    |> JsonObject.field("x", value)
    |> JsonObject.end
}

effect fn writeArray(values: &[i32], options: &JsonOptions)
  -> () ! JsonError | WriterError ? &mut Writer {
  return run JsonArray.begin(options)
    |> JsonArray.items(values)
    |> JsonArray.end
}

effect fn check() -> i32 ! JsonError | WriterError {
  let options = JsonOptions.compact()
  let value: i32 = 1
  let values = [value]
  let mut sink = Sink {}
  run writeObject(&value, &options) |> Effect.provideMut<Writer>(&mut sink)
  run writeArray(&values, &options) |> Effect.provideMut<Writer>(&mut sink)
  return 0
}

effect fn recover(error: JsonError | WriterError) -> i32 { return 90 }
pub fn main() -> i32 { return run Effect.catchAll(check(), recover) }
`
