export const recoveredWriterModule = `import silk.native_descriptor { NativeDescriptor }
import silk.effect { Effect }
import silk.result { Result }
import silk.writer { Writer as NativeWriter, WriterError as NativeWriterError }

effect fn writeNative(bytes: &[u8]) -> () ! NativeWriterError {
  let mut error = 0
  let complete = run NativeDescriptor.writeAll(1, bytes, &mut error)
  if complete == false { fail NativeWriter.failure() }
  return ()
}

pub struct WriterError {}

service Writer {
  effect fn writeAll(bytes: &[u8]) -> () ! WriterError ? &mut Writer
}

pub struct StdoutWriter {}

effect fn translate<A, E, ?R>(self: once Effect<A ! E ? R>) -> A ! WriterError ? R {
  let completed = run Effect.result(move self)
  if let Result<A, E>.Success { value } = move completed {
    return move value
  }
  fail WriterError {}
}

impl Writer for StdoutWriter {
  effect fn writeAll(
    self: &mut StdoutWriter,
    bytes: &[u8]
  ) -> () ! WriterError ? &mut Writer {
    return run writeNative(bytes) |> translate
  }
}

pub effect fn program() -> () ! WriterError ? &mut Writer {
  return run Writer.writeAll(b"Hello")
}`

export const recoveredProvidedWrite = `import silk.effect { Effect }
import recovered_writer { WriterError, StdoutWriter, program }

pub effect fn main() -> () ! WriterError {
  let mut writer = StdoutWriter {}
  return run program() |> Effect.provideMut(&mut writer)
}`

export const recoveredDirectWrite = `import silk.native_descriptor { NativeDescriptor }
import silk.effect { Effect }
import silk.result { Result }
import silk.writer { Writer as NativeWriter, WriterError as NativeWriterError }

effect fn writeNative(bytes: &[u8]) -> () ! NativeWriterError {
  let mut error = 0
  let complete = run NativeDescriptor.writeAll(1, bytes, &mut error)
  if complete == false { fail NativeWriter.failure() }
  return ()
}

pub struct WriterError {}

effect fn translate<A, E, ?R>(self: once Effect<A ! E ? R>) -> A ! WriterError ? R {
  let completed = run Effect.result(move self)
  if let Result<A, E>.Success { value } = move completed {
    return move value
  }
  fail WriterError {}
}

pub effect fn main() -> () ! WriterError {
  return run writeNative(b"Hello") |> translate
}`
