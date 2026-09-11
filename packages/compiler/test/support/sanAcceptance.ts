// RFC 5280 (2008-05) GeneralNames DER recipes. One native program covers the adapter boundary.
const der = (tag: number, value: readonly number[]): number[] => [
  tag,
  ...(value.length < 128 ? [value.length] : [0x81, value.length]),
  ...value,
]
const ascii = (text: string): number[] => Array.from(text, (value) => value.charCodeAt(0))
const literal = (bytes: readonly number[]): string =>
  `b"${bytes.map((value) => `\\x${value.toString(16).padStart(2, '0')}`).join('')}"`
const dns = der(0x82, ascii('example.com'))
const ip = der(0x87, [192, 0, 2, 1])
const san = (...names: readonly number[][]): number[] => der(0x30, names.flat())
const alg = der(0x30, [...der(6, [42, 3]), ...der(5, [])])
const ext = (value: readonly number[]): number[] =>
  der(0x30, [...der(6, [85, 29, 17]), ...der(4, value)])
const certificate = (extensions: readonly number[][]): number[] =>
  der(0x30, [
    ...der(0x30, [
      ...der(0xa0, der(2, [2])),
      ...der(2, [1]),
      ...alg,
      ...der(0x30, []),
      ...der(0x30, [...der(23, ascii('260101000000Z')), ...der(23, ascii('270101000000Z'))]),
      ...der(0x30, []),
      ...der(0x30, [...alg, ...der(3, [0])]),
      ...(extensions.length === 0 ? [] : der(0xa3, der(0x30, extensions.flat()))),
    ]),
    ...alg,
    ...der(3, [0]),
  ])
const validOther = [
  der(0x81, ascii('a@example.com')),
  der(0x86, ascii('https://example.com')),
  der(0x88, [42, 3]),
  der(0xa0, [...der(6, [42, 3]), ...der(0xa0, der(12, ascii('value')))]),
  der(0xa3, der(0x30, [])),
  der(0xa4, der(0x30, [])),
  der(0xa5, der(0xa1, der(12, ascii('party')))),
]
const otherValue = (value: number[]): number[] => der(0xa0, [...der(6, [42]), ...der(0xa0, value)])
const cases: readonly (readonly [number[], string, string?])[] = [
  [san(dns, ip), '2'],
  ...[
    der(10, []),
    der(10, [0, 1]),
    der(42, [1]),
    der(18, [65]),
    der(26, [31]),
    der(13, [128, 1]),
    der(13, [129]),
    der(9, [128, 0, 2]),
    der(9, [128]),
    der(9, [3, ...ascii('1.0E+0')]),
    der(23, ascii('2601010000Z')),
    der(23, ascii('161231235960Z')),
    der(24, ascii('20161231235860Z')),
    der(24, ascii('20260230000000Z')),
    der(24, ascii('20260101000000.10Z')),
    der(15, []),
  ].map((value) => [san(dns, otherValue(value)), 'InvalidDer'] as const),
  [
    san(
      otherValue(der(10, [1])),
      otherValue(der(18, ascii('12 3'))),
      otherValue(der(26, ascii('value'))),
      otherValue(der(13, [42])),
    ),
    '4',
  ],

  ...[
    der(9, []),
    der(9, [128, 0, 1]),
    der(9, [64]),
    der(9, [3, ...ascii('-123.E-2')]),
    der(23, ascii('260101000000Z')),
    der(24, ascii('20240229000000.125Z')),
    der(24, ascii('20161231235960Z')),
  ].map((value) => [san(otherValue(value)), '1'] as const),
  [san(...Array.from({ length: 9 }, () => dns)), 'Storage'],
  [san(...validOther), '7'],
  [san(), 'EmptySan'],
  [[0x30, 0x80, 0, 0], 'InvalidDer'],
  [[0x30, 0x81, 0], 'InvalidDer'],
  [san(dns, [0x87, 4, 1]), 'InvalidDer'],
  [[...san(dns), 0], 'InvalidDer'],
  [san(der(0xa2, ascii('example.com'))), 'InvalidDer'],
  [san(der(0x89, [])), 'InvalidDer'],
  [san(der(0x82, [255])), 'InvalidDer'],
  [san(der(0x88, [128, 1])), 'InvalidDer'],
  [san(der(0xa0, [...der(6, [42, 3]), ...der(0xa0, [1, 1, 1])])), 'InvalidDer'],
  [san(der(0xa5, [])), 'InvalidDer'],
  [san(der(0xa4, der(0x30, der(0x31, [])))), 'InvalidDer'],
  [san(dns), 'InputBytes', 'limits.inputBytes = 1'],
  [san(dns), 'Identities', 'limits.identities = 0'],
  [san(dns), 'Nodes', 'limits.nodes = 1'],
  [san(dns), 'Depth', 'limits.depth = 1'],
  [
    san(
      der(0xa0, [
        ...der(6, [42, 3]),
        ...der(
          0xa0,
          Array.from({ length: 32 }).reduce<number[]>((value) => der(0x30, value), der(5, [])),
        ),
      ]),
    ),
    'Depth',
    'limits.depth = 99',
  ],
]
export const sanAcceptanceSource = `import silk.certificate {Certificate, DecodeLimits, DecodeError}
import silk.certificate_identities {CertificateSan, SanDecodeLimits, SanDecodeSummary, SanDecodeError, SanDecodeReason}
import silk.https_identity {PresentedIdentity, CertificateIdentities, HttpsIdentity, ReferenceIdentity, IdentityLimits, IdentityMatch, IdentityError}
import silk.result {Result}
import silk.effect {Effect}
import silk.allocator {Allocator, OutOfMemoryError}
import silk.slice {Slice}
import silk.usize
import silk.u8
import silk.i32
fn verifyEntries<'data: 'view, 'view>(storage: &'view [PresentedIdentity<'data>], count: usize) -> bool {
  let entries: &'view [PresentedIdentity<'view>] = Slice.view<PresentedIdentity<'data>>(storage, 0, count)
  let identities = CertificateIdentities<'view>.Decoded {entries: entries}
  let reference = ReferenceIdentity<'static>.Dns {bytes: b"example.com"}
  let matched = HttpsIdentity.verify(&reference, &identities, IdentityLimits.standard())
  return match move matched {
    Result<IdentityMatch, IdentityError>.Success {value} => value.sanIndex == 0
    Result<IdentityMatch, IdentityError>.Failure {error} => false
  }
}
fn inspectSan<'cert>(certificate: &'cert Certificate, expected: usize, reason: SanDecodeReason) -> bool {
  let empty = PresentedIdentity<'cert>.Dns {bytes: b""}
  let mut storage: [PresentedIdentity<'cert>; 8] = [empty,empty,empty,empty,empty,empty,empty,empty]
  let adapted = CertificateSan.decode(certificate, &mut storage, SanDecodeLimits.standard())
  return match move adapted {
    Result<SanDecodeSummary, SanDecodeError>.Success {value} => {
      if value.count != expected {return false}
      if expected == 0 {return !value.present}
      return verifyEntries(&storage, value.count)
    }
    Result<SanDecodeSummary, SanDecodeError>.Failure {error} => expected == 999 && error.reason == reason
  }
}
fn consumeCertificate(owner: Certificate, expected: usize, reason: SanDecodeReason) -> bool {
  return inspectSan(&owner, expected, reason)
}
fn raw<'a>(bytes: &'a [u8], expected: usize, reason: SanDecodeReason, limits: SanDecodeLimits) -> bool {
  let empty = PresentedIdentity<'a>.Dns {bytes: b""}
  let mut storage: [PresentedIdentity<'a>; 8] = [empty, empty, empty, empty, empty, empty, empty, empty]
  let result = CertificateSan.decodeValue(bytes, &mut storage, limits)
  return match move result {
    Result<SanDecodeSummary, SanDecodeError>.Success {value} => {
      if !value.present || value.count != expected {return false}
      if expected == 7 {
        let tags: [u8; 7] = [1, 6, 8, 0, 3, 4, 5]
        let mut index = usize.ZERO
        while index < 7 {
          let entry = storage[index]
          let correct = match move entry {
            PresentedIdentity<'a>.Other {tag, bytes: content} => tag == tags[index] && content.length > 0
            PresentedIdentity<'a>.Dns {bytes: content} => false
            PresentedIdentity<'a>.Ip {bytes: content} => false
          }
          if !correct {return false}
          index = index + 1
        }
      }
      return true
    }
    Result<SanDecodeSummary, SanDecodeError>.Failure {error} => expected == 999 && error.reason == reason
  }
}
effect fn allCases() -> i32 ! OutOfMemoryError ? &mut Allocator {
${cases
  .map(
    ([bytes, expected, setup], i) => `  let mut limits${i} = SanDecodeLimits.standard()
  ${(setup ?? '').replaceAll('limits.', `limits${i}.`)}
  if !raw(${literal(bytes)}, ${/^\d+$/.test(expected) ? expected : '999'}, SanDecodeReason.${/^\d+$/.test(expected) ? 'InvalidDer' : expected}, limits${i}) {return ${i + 1}}`,
  )
  .join('\n')}
${(
  [
    [certificate([ext(san(dns))]), 1, 'InvalidDer'],
    [certificate([]), 0, 'InvalidDer'],
    [certificate([ext(san(dns)), ext(san(ip))]), 999, 'DuplicateSan'],
    [certificate([ext(san(dns, [0x87, 4, 1]))]), 999, 'InvalidDer'],
  ] satisfies ReadonlyArray<readonly [number[], number, string]>
)
  .map(
    ([bytes, expected, reason], index) => `
  let decoded${index} = run Certificate.decodeDer(${literal(bytes)}, DecodeLimits.defaults())
  let owner${index} = match move decoded${index} {
    Result<Certificate, DecodeError>.Success {value} => move value
    Result<Certificate, DecodeError>.Failure {error} => {return ${40 + index}}
  }
  if !consumeCertificate(move owner${index}, ${expected}, SanDecodeReason.${reason}) {return ${50 + index}}

`,
  )
  .join('\n')}
  return 0
}
effect fn allocated() -> i32 ! OutOfMemoryError {
  let mut allocator = Allocator.systemAllocatorProvider()
  return run (allCases() |> Effect.provideMut<Allocator>(&mut allocator))
}
effect fn recover(error: OutOfMemoryError) -> i32 {return 250}
pub fn main() -> i32 {return run Effect.catchAll(allocated(), recover)}
`
