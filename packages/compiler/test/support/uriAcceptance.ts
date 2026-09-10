// Independently pinned strict examples from RFC 3986 §5.4, in specification order.
// One program shares a parsed base and exercises each vector without recompilation.
const resolutionVectors: ReadonlyArray<readonly [string, string]> = [
  ['g:h', 'g:h'],
  ['g', 'http://a/b/c/g'],
  ['./g', 'http://a/b/c/g'],
  ['g/', 'http://a/b/c/g/'],
  ['/g', 'http://a/g'],
  ['//g', 'http://g'],
  ['?y', 'http://a/b/c/d;p?y'],
  ['g?y', 'http://a/b/c/g?y'],
  ['#s', 'http://a/b/c/d;p?q#s'],
  ['g#s', 'http://a/b/c/g#s'],
  ['g?y#s', 'http://a/b/c/g?y#s'],
  [';x', 'http://a/b/c/;x'],
  ['g;x', 'http://a/b/c/g;x'],
  ['g;x?y#s', 'http://a/b/c/g;x?y#s'],
  ['', 'http://a/b/c/d;p?q'],
  ['.', 'http://a/b/c/'],
  ['./', 'http://a/b/c/'],
  ['..', 'http://a/b/'],
  ['../', 'http://a/b/'],
  ['../g', 'http://a/b/g'],
  ['../..', 'http://a/'],
  ['../../', 'http://a/'],
  ['../../g', 'http://a/g'],
  ['../../../g', 'http://a/g'],
  ['../../../../g', 'http://a/g'],
  ['/./g', 'http://a/g'],
  ['/../g', 'http://a/g'],
  ['g.', 'http://a/b/c/g.'],
  ['.g', 'http://a/b/c/.g'],
  ['g..', 'http://a/b/c/g..'],
  ['..g', 'http://a/b/c/..g'],
  ['./../g', 'http://a/b/g'],
  ['./g/.', 'http://a/b/c/g/'],
  ['g/./h', 'http://a/b/c/g/h'],
  ['g/../h', 'http://a/b/c/h'],
  ['g;x=1/./y', 'http://a/b/c/g;x=1/y'],
  ['g;x=1/../y', 'http://a/b/c/y'],
  ['g?y/./x', 'http://a/b/c/g?y/./x'],
  ['g?y/../x', 'http://a/b/c/g?y/../x'],
  ['g#s/./x', 'http://a/b/c/g#s/./x'],
  ['g#s/../x', 'http://a/b/c/g#s/../x'],
  ['http:g', 'http:g'],
  // Encoded dots remain data; empty delimiters replace inherited components.
  ['%2e/%2E%2e/g?y/../x', 'http://a/b/c/%2e/%2E%2e/g?y/../x'],
  ['?#', 'http://a/b/c/d;p?#'],
]

export const uriAcceptanceSource = `import silk.uri { Uri }
import silk.uri_reference { UriReference, ParseError, ParseReason, Component, HostKind }
import silk.uri_percent { UriPercent, DecodeError }
import silk.uri_percent { Component as PercentComponent }
import silk.allocator { Allocator, OutOfMemoryError }
import silk.effect { Effect }
import silk.result { Result }
import silk.option { Option }
import silk.string { String, InvalidUtf8 }
import silk.bytes { Bytes }
import silk.usize as usize

fn present(actual: Option<string>, expected: string) -> bool {
  return match move actual {
    Option<string>.Some { value } => value == expected
    Option<string>.None => false
  }
}

fn absent(actual: Option<string>) -> bool {
  return match move actual {
    Option<string>.Some { value } => false
    Option<string>.None => true
  }
}

effect fn resolves(base: &Uri, text: string, expected: string) -> bool
! OutOfMemoryError ? &mut Allocator {
  let parsed = run UriReference.parse(text)
  return match move parsed {
    Result<UriReference, ParseError>.Failure { error } => false
    Result<UriReference, ParseError>.Success { value } => {
      let resolved = run Uri.resolve(base, &value)
      return match move resolved {
        Result<Uri, ParseError>.Failure { error } => false
        Result<Uri, ParseError>.Success { value: target } => Uri.format(&target) == expected
      }
    }
  }
}

effect fn vectors(base: &Uri) -> i32 ! OutOfMemoryError ? &mut Allocator {
  let references: [string<'static>; ${resolutionVectors.length}] = [${resolutionVectors.map(([reference]) => JSON.stringify(reference)).join(', ')}]
  let expected: [string<'static>; ${resolutionVectors.length}] = [${resolutionVectors.map(([, expected]) => JSON.stringify(expected)).join(', ')}]
  let mut index: usize = 0
  while index < ${resolutionVectors.length} {
    if !(run resolves(base, references[index], expected[index])) { return usize.toI32(index) + 1 }
    index = index + 1
  }
  return 0
}

effect fn copiedInput() -> Result<UriReference, ParseError>
! OutOfMemoryError ? &mut Allocator {
  let input = run String.copy("x:/owned?query")
  return run UriReference.parse(String.view(&input))
}

effect fn ownedInput() -> bool ! OutOfMemoryError ? &mut Allocator {
  let parsed = run copiedInput()
  return match move parsed {
    Result<UriReference, ParseError>.Failure { error } => false
    Result<UriReference, ParseError>.Success { value } => UriReference.format(&value) == "x:/owned?query"
  }
}

effect fn emptyBasePath() -> bool ! OutOfMemoryError ? &mut Allocator {
  let parsed = run Uri.parse("x://host?old#discard")
  return match move parsed {
    Result<Uri, ParseError>.Failure { error } => false
    Result<Uri, ParseError>.Success { value } => {
      if Uri.scheme(&value) != "x" || Uri.path(&value) != "" { return false }
      if !present(Uri.authority(&value), "host") || !present(Uri.host(&value), "host") { return false }
      if !present(Uri.query(&value), "old") || !present(Uri.fragment(&value), "discard") { return false }
      if !absent(Uri.userinfo(&value)) || !absent(Uri.port(&value)) { return false }
      return run resolves(&value, "g", "x://host/g")
    }
  }
}

// Literal RFC recomposition can expose an authority after dot removal. Revalidation owns
// the resulting component interpretation and reports offsets in the recomposed serialization.
effect fn recomposedAuthority(base: &Uri) -> bool ! OutOfMemoryError ? &mut Allocator {
  let parsed = run UriReference.parse("/a/..//g")
  return match move parsed {
    Result<UriReference, ParseError>.Failure { error } => false
    Result<UriReference, ParseError>.Success { value: reference } => {
      let resolved = run Uri.resolve(base, &reference)
      return match move resolved {
        Result<Uri, ParseError>.Failure { error } => false
        Result<Uri, ParseError>.Success { value } => Uri.format(&value) == "x://g" && present(Uri.authority(&value), "g")
      }
    }
  }
}

effect fn recomposition() -> bool ! OutOfMemoryError ? &mut Allocator {
  let parsedBase = run Uri.parse("x:a")
  return match move parsedBase {
    Result<Uri, ParseError>.Failure { error } => false
    Result<Uri, ParseError>.Success { value: base } => {
      if !(run recomposedAuthority(&base)) { return false }
      let invalid = run UriReference.parse("/a/..//g:h")
      return match move invalid {
        Result<UriReference, ParseError>.Failure { error } => false
        Result<UriReference, ParseError>.Success { value: reference } => {
          let resolved = run Uri.resolve(&base, &reference)
          return match move resolved {
            Result<Uri, ParseError>.Success { value } => false
            Result<Uri, ParseError>.Failure { error } => error.reason == ParseReason.InvalidPort && error.component == Component.Port && error.offset == 6
          }
        }
      }
    }
  }
}

effect fn emptyComponents() -> bool ! OutOfMemoryError ? &mut Allocator {
  let parsed = run UriReference.parse("x://@:/%2e?")
  return match move parsed {
    Result<UriReference, ParseError>.Failure { error } => false
    Result<UriReference, ParseError>.Success { value } => {
      return UriReference.format(&value) == "x://@:/%2e?"
        && present(UriReference.scheme(&value), "x")
        && present(UriReference.authority(&value), "@:")
        && present(UriReference.userinfo(&value), "")
        && present(UriReference.host(&value), "")
        && present(UriReference.port(&value), "")
        && UriReference.path(&value) == "/%2e"
        && present(UriReference.query(&value), "")
        && absent(UriReference.fragment(&value))
    }
  }
}

effect fn roundtrip(text: string) -> bool ! OutOfMemoryError ? &mut Allocator {
  let parsed = run UriReference.parse(text)
  return match move parsed {
    Result<UriReference, ParseError>.Failure { error } => false
    Result<UriReference, ParseError>.Success { value } => UriReference.format(&value) == text
  }
}

effect fn hostKind(text: string, expected: HostKind) -> bool ! OutOfMemoryError ? &mut Allocator {
  let parsed = run UriReference.parse(text)
  return match move parsed {
    Result<UriReference, ParseError>.Failure { error } => false
    Result<UriReference, ParseError>.Success { value } => {
      if UriReference.format(&value) != text { return false }
      return match move UriReference.hostKind(&value) {
        Option<HostKind>.None => false
        Option<HostKind>.Some { value: actual } => actual == expected
      }
    }
  }
}

effect fn badReference(text: string, reason: ParseReason, component: Component, offset: usize) -> bool
! OutOfMemoryError ? &mut Allocator {
  let parsed = run UriReference.parse(text)
  return match move parsed {
    Result<UriReference, ParseError>.Success { value } => false
    Result<UriReference, ParseError>.Failure { error } => error.reason == reason && error.component == component && error.offset == offset
  }
}

effect fn missingScheme() -> bool ! OutOfMemoryError ? &mut Allocator {
  let parsed = run Uri.parse("relative/path")
  return match move parsed {
    Result<Uri, ParseError>.Success { value } => false
    Result<Uri, ParseError>.Failure { error } => error.reason == ParseReason.MissingScheme && error.component == Component.Scheme && error.offset == 0
  }
}

effect fn encoded(raw: &[u8], component: PercentComponent, expected: string) -> bool
! OutOfMemoryError ? &mut Allocator {
  let text = run UriPercent.encode(raw, move component)
  return String.view(&text) == expected
}

effect fn percentBytes() -> bool ! OutOfMemoryError ? &mut Allocator {
  let decoded = run UriPercent.decode("%FF+%00")
  return match move decoded {
    Result<Bytes, DecodeError>.Failure { error } => false
    Result<Bytes, DecodeError>.Success { value } => decodedBytes(move value)
  }
}

fn decodedBytes(value: Bytes) -> bool {
  let bytes = Bytes.asSlice(&value)
  if bytes.length != 3 { return false }
  if bytes[0] != 255 || bytes[1] != 43 || bytes[2] != 0 { return false }
  return match move String.fromUtf8(bytes) {
    Result<string, InvalidUtf8>.Success { value: text } => false
    Result<string, InvalidUtf8>.Failure { error } => true
  }
}

effect fn badDecode(text: string, offset: usize) -> bool ! OutOfMemoryError ? &mut Allocator {
  let decoded = run UriPercent.decode(text)
  return match move decoded {
    Result<Bytes, DecodeError>.Success { value } => false
    Result<Bytes, DecodeError>.Failure { error } => error.offset == offset
  }
}

effect fn check() -> i32 ! OutOfMemoryError ? &mut Allocator {
  let parsed = run Uri.parse("http://a/b/c/d;p?q")
  let result = match move parsed {
    Result<Uri, ParseError>.Failure { error } => 100
    Result<Uri, ParseError>.Success { value } => run vectors(&value)
  }
  if result != 0 { return result }
  if !(run emptyComponents()) { return 101 }
  if !(run roundtrip("mailto:person@example.com")) { return 102 }
  if !(run roundtrip("urn:example:thing")) { return 103 }
  if !(run roundtrip("//user:pass@EXAMPLE.com:999999/a%2f?+#")) { return 104 }
  if !(run roundtrip("a/b:c")) { return 105 }
  let segment = b"a/b%2f"
  if !(run encoded(&segment, PercentComponent.PathSegment, "a%2Fb%252f")) { return 106 }
  let delimiters = b"a:b@c/?#%+"
  if !(run encoded(&delimiters, PercentComponent.Userinfo, "a:b%40c%2F%3F%23%25+")) { return 107 }
  if !(run encoded(&delimiters, PercentComponent.RegName, "a%3Ab%40c%2F%3F%23%25+")) { return 108 }
  if !(run encoded(&delimiters, PercentComponent.FirstPathSegment, "a%3Ab@c%2F%3F%23%25+")) { return 109 }
  if !(run encoded(&delimiters, PercentComponent.Query, "a:b@c/?%23%25+")) { return 110 }
  if !(run encoded(&delimiters, PercentComponent.Fragment, "a:b@c/?%23%25+")) { return 111 }
  if !(run percentBytes()) { return 112 }
  if !(run badDecode("a%", 1)) { return 113 }
  if !(run badDecode("a%0G", 1)) { return 114 }
  if !(run ownedInput()) { return 115 }
  if !(run emptyBasePath()) { return 116 }
  if !(run missingScheme()) { return 117 }
  if !(run recomposition()) { return 118 }
  if !(run hostKind("//127.0.0.1:999999", HostKind.Ipv4)) { return 120 }
  if !(run hostKind("//01.2.3.4", HostKind.RegName)) { return 121 }
  if !(run hostKind("//[::ffff:192.0.2.1]", HostKind.Ipv6)) { return 122 }
  if !(run hostKind("//[vF.a:b]", HostKind.IpvFuture)) { return 123 }
  if !(run hostKind("//[1:2:3:4:5:6:7:8]", HostKind.Ipv6)) { return 124 }
  if !(run hostKind("//[::]", HostKind.Ipv6)) { return 125 }
  if !(run hostKind("//[1:2:3:4:5:6:7::]", HostKind.Ipv6)) { return 126 }
  if !(run hostKind("//[1:2:3:4:5:6:192.0.2.1]", HostKind.Ipv6)) { return 127 }
  if !(run badReference("%", ParseReason.InvalidPercentEscape, Component.Path, 0)) { return 140 }
  if !(run badReference("/%0G", ParseReason.InvalidPercentEscape, Component.Path, 3)) { return 141 }
  if !(run badReference("a b", ParseReason.InvalidCharacter, Component.Path, 1)) { return 142 }
  if !(run badReference("a#b#c", ParseReason.InvalidCharacter, Component.Fragment, 3)) { return 143 }
  if !(run badReference("//[::1", ParseReason.InvalidHost, Component.Host, 6)) { return 144 }
  if !(run badReference("1x:a", ParseReason.InvalidScheme, Component.Scheme, 0)) { return 145 }
  if !(run badReference("/é", ParseReason.InvalidCharacter, Component.Path, 1)) { return 146 }
  if !(run badReference("//[1:2:3:4:5:6:7]", ParseReason.InvalidHost, Component.Host, 16)) { return 147 }
  if !(run badReference("//[1:2:3:4:5:6:7:8:9]", ParseReason.InvalidHost, Component.Host, 19)) { return 148 }
  if !(run badReference("//[1::2::3]", ParseReason.InvalidHost, Component.Host, 8)) { return 149 }
  if !(run badReference("//[::ffff:256.1.1.1]", ParseReason.InvalidHost, Component.Host, 10)) { return 150 }
  if !(run badReference("//[fe80::1%25eth0]", ParseReason.InvalidHost, Component.Host, 10)) { return 151 }
  if !(run badReference("//[v.a]", ParseReason.InvalidHost, Component.Host, 4)) { return 152 }
  if !(run badReference("//[v1.]", ParseReason.InvalidHost, Component.Host, 6)) { return 153 }
  if !(run badReference("//[1:2:3:4:5:6:7::8]", ParseReason.InvalidHost, Component.Host, 19)) { return 154 }
  if !(run badReference("//h:x", ParseReason.InvalidPort, Component.Port, 4)) { return 155 }
  if !(run badReference("//u@v@h", ParseReason.InvalidCharacter, Component.Authority, 5)) { return 156 }
  if !(run badReference("//[::]x", ParseReason.InvalidHost, Component.Host, 6)) { return 157 }
  return 0
}

effect fn allocated() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run check() |> Effect.provideMut<Allocator>(&mut allocator)
}

effect fn recover(error: OutOfMemoryError) -> i32 { return 250 }

pub fn main() -> i32 { return run Effect.catchAll(allocated(), recover) }
`
