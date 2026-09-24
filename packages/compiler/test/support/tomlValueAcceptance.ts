export const tomlValueAcceptanceSource = `import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.option { Option }
import silk.toml_scanner { TomlError }
import silk.toml_value { Toml }

effect fn check() -> i32 ! TomlError | OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  let document = run Toml.parse(b"title = \\"example\\"\\n[owner]\\nname = \\"Ada\\"\\n")
    |> Effect.provideMut<Allocator>(&mut allocator)
  let title = Toml.field(&document, "title") |> Toml.asString
  match move title {
    Option<string>.Some { value: text } => { if text != "example" { return 2 } }
    Option<string>.None => { return 3 }
  }
  return 42
}

effect fn recover(error: TomlError | OutOfMemoryError) -> i32 { return 1 }
pub fn main() -> i32 { return run Effect.catchAll(check(), recover) }`
