const valid = [
  '',
  '# comment\n',
  'name = "TOML"\n',
  'a.b = +1_000\n',
  'hex = 0xDEAD_beef\noct = 0o755\nbin = 0b1010\n',
  'floats = [+1.0, -2E-2, inf, -nan]\n',
  'times = [07:32, 07:32:00.999999, 1979-05-27, 1979-05-27T07:32, 1979-05-27 07:32Z]\n',
  'quoted."utf8-€" = "hello\\e\\x1B\\u20AC\\U0001F4A9"\n',
  "literal = '''\nhello\nworld'''\n",
  'array = [1, # comment\n 2,]\n',
  'inline = {\n a = 1,\n b = {c = true,},\n}\n',
  '[[products]]\nname = "one"\n[[products]]\nname = "two"\n',
  'text = """\nLine one \\\n line two"""\n',
  'leap = 2000-02-29\n',
] as const

const invalid = [
  ['key =', 'UnexpectedEnd', 5],
  ['x = 01', 'InvalidNumber', 4],
  ['x = 1__0', 'InvalidNumber', 4],
  ['x = 0x', 'InvalidNumber', 4],
  ['x = .7', 'InvalidNumber', 4],
  ['x = 7.', 'InvalidNumber', 4],
  ['x = 2023-02-29', 'InvalidDateTime', 4],
  ['x = 25:00', 'InvalidDateTime', 4],
  ['x = 1979-05-27T07:32+24:00', 'InvalidDateTime', 4],
  ['x = "\\q"', 'InvalidEscape', 6],
  ['x = "\\uD800"', 'InvalidEscape', 7],
  ['x = [1,,2]', 'UnexpectedByte', 7],
  ['x = {a = 1,,}', 'UnexpectedByte', 11],
  ['x = 1 y = 2', 'UnexpectedByte', 6],
  ['x = "unterminated', 'UnexpectedEnd', 17],
  ['x = ' + '['.repeat(65) + ']'.repeat(65), 'DepthExceeded', 68],
] as const

const byteList = (value: string): string => [...new TextEncoder().encode(value)].join(', ')
const silkBytes = (name: string, value: string): string => {
  const bytes = new TextEncoder().encode(value)
  return `  let ${name}: [u8; ${bytes.length}] = [${byteList(value)}]`
}

export const tomlScannerAcceptanceSource = `import silk.toml_scanner { TomlScanner, TomlToken, TomlError, TomlReason, TomlScalarKind, TomlSpan, MAX_DEPTH }
import silk.option { Option }
import silk.result { Result }
import silk.usize

fn accepts(input: &[u8]) -> bool {
  let mut scanner = TomlScanner.make(input)
  return match move TomlScanner.validate(&mut scanner) {
    Result<bool, TomlError>.Success { value } => value
    Result<bool, TomlError>.Failure { error: _ } => false
  }
}
fn rejects(input: &[u8], reason: TomlReason, offset: usize) -> bool {
  let mut scanner = TomlScanner.make(input)
  return match move TomlScanner.validate(&mut scanner) {
    Result<bool, TomlError>.Success { value: _ } => false
    Result<bool, TomlError>.Failure { error } => error.reason == reason && error.offset == offset
  }
}
pub fn main() -> i32 {
  if MAX_DEPTH != 64 { return 1 }
  let invalidUtf8: [u8; 8] = [120, 32, 61, 32, 34, 192, 128, 34]
  if !rejects(&invalidUtf8, TomlReason.InvalidUtf8, 5) { return 70 }
${valid.map((value, i) => `${silkBytes(`v${i}`, value)}\n  if !accepts(&v${i}) { return ${i + 2} }`).join('\n')}
${invalid.map(([value, reason, offset], i) => `${silkBytes(`e${i}`, value)}\n  if !rejects(&e${i}, TomlReason.${reason}, ${offset}) { return ${i + 30} }`).join('\n')}
  let mut scanner = TomlScanner.make(b"a = 07:32")
  let result = TomlScanner.validate(&mut scanner)
  match move result {
    Result<bool, TomlError>.Failure { error: _ } => { return 80 }
    Result<bool, TomlError>.Success { value: _ } => {}
  }
  let token = TomlScanner.next(&mut scanner)
  match move token {
    Result<TomlToken, TomlError>.Failure { error: _ } => { return 81 }
    Result<TomlToken, TomlError>.Success { value } => match move value {
      TomlToken.Text { span } => {
        if span.offset != 0 || span.length != 1 { return 82 }
      }
      _ => { return 83 }
    }
  }
  let equals = TomlScanner.next(&mut scanner)
  match move equals {
    Result<TomlToken, TomlError>.Failure { error: _ } => { return 84 }
    Result<TomlToken, TomlError>.Success { value } => match move value {
      TomlToken.Equals => {}
      _ => { return 85 }
    }
  }
  let scalar = TomlScanner.next(&mut scanner)
  match move scalar {
    Result<TomlToken, TomlError>.Failure { error: _ } => { return 86 }
    Result<TomlToken, TomlError>.Success { value } => match move value {
      TomlToken.Text { span } => {
        if span.offset != 4 || span.length != 5 { return 87 }
        let kind = TomlScanner.scalarKind(&scanner, move span)
        match move kind {
          Option<TomlScalarKind>.Some { value: kindValue } => {
            if kindValue != TomlScalarKind.LocalTime { return 88 }
          }
          Option<TomlScalarKind>.None => { return 89 }
        }
      }
      _ => { return 90 }
    }
  }
  return 42
}`
