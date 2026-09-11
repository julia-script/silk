# HTTPS identity fixture matrix v1

Matrix v1 is the oracle for the runtime matcher corpus and SAN adapter integration fixtures.
Freeze expectations against RFC 9525 (November 2023), RFC 9110 (June 2022), RFC 5280 (May 2008),
RFC 3986 (January 2005), and the webpki comparison at commit
`3d0adc46704c1d985a9fefe2034650488d96d49e`. Links and policy differences are in
[design.md](design.md#7-authorities-and-implementation-comparison). A policy change requires a
new matrix version; implementation tests must not silently substitute webpki's answers.

Notation: `D(text)` is raw ASCII dNSName payload; `I(hex)` is raw iPAddress octets;
`O(tag, bytes)` is a structurally valid unsupported GeneralName. `N(text)` is an origin/reference
DNS input; `V4(hex)`/`V6(hex)` are owned binary references. Arrays list SANs in source order.
`x^n` means n repetitions, `+` concatenates bytes and `00`/`ff` mean exact hex bytes when outside
quoted text. No escaping, terminator, Unicode conversion or text-IP parsing occurs implicitly.
All rows use standard limits unless specified. Error abbreviations expand to `IdentityError`
variants from the design: `Reference`, `Structure`, `Name(index, reason)`, `Identity(index, reason)`,
`Unsupported`, `LimitExceeded`, `NoMatch`. `Match(n)` means `IdentityMatch {sanIndex: n}`.

## DNS matching and wildcard policy

| ID  | Reference                  | Certificate input                             | Expected                                                                     |
| --- | -------------------------- | --------------------------------------------- | ---------------------------------------------------------------------------- |
| D01 | `N(www.example)`           | `[D(www.example)]`                            | `Match(0)`                                                                   |
| D02 | `N(WWW.Example)`           | `[D(www.EXAMPLE)]`                            | `Match(0)`                                                                   |
| D03 | `N(www.example)`           | `[D(other.example)]`                          | `NoMatch`                                                                    |
| D04 | `N(a.example)`             | `[D(*.example)]`                              | `Match(0)`; one-label suffix is allowed                                      |
| D05 | `N(example)`               | `[D(*.example)]`                              | `NoMatch`; wildcard cannot match zero labels                                 |
| D06 | `N(a.b.example)`           | `[D(*.example)]`                              | `NoMatch`; wildcard cannot match two labels                                  |
| D07 | `N(a.example)`             | `[D(a.example.evil)]`                         | `NoMatch`; suffix/prefix comparison is insufficient                          |
| D08 | `N(www.example)`           | `[D(w*.example), D(www.example)]`             | `Match(1)`; partial wildcard skipped                                         |
| D09 | `N(www.example)`           | `[D(*.*.example), D(www.example)]`            | `Match(1)`; multiple wildcards skipped                                       |
| D10 | `N(www.example)`           | `[D(a.*.example), D(www.example)]`            | `Match(1)`; interior wildcard skipped                                        |
| D11 | `N(www.example)`           | `[D(**.example)]`                             | `NoMatch`; invalid wildcard alone                                            |
| D12 | `N(www.example)`           | `[D(www.example), D(w*.example)]`             | `Match(0)`; ignored wildcard after match                                     |
| D13 | `N(xn--bcher-kva.example)` | `[D(XN--BCHER-KVA.EXAMPLE)]`                  | `Match(0)`; prepared A-label                                                 |
| D14 | `N(xn--bcher-kva.example)` | `[D(*.example)]`                              | `Match(0)`; whole A-label wildcard                                           |
| D15 | `N(a.com)`                 | `[D(*.com)]`                                  | `Match(0)`; no public-suffix policy                                          |
| D16 | `N(intranet)`              | `[D(INTRANET)]`                               | `Match(0)`; absolute single-label identity                                   |
| D17 | `N(a.example)`             | `[D(*a.example), D(a.example), D(*.example)]` | `Match(1)`; first eligible match keeps original index                        |
| D18 | `N(www.example)`           | `[D(*), D(www.example)]`                      | `Name(0, MissingWildcardSuffix)`                                             |
| D19 | `N(www.example)`           | `[D(w*._bad.example), D(www.example)]`        | `Match(1)`; mandatory wildcard ignore precedes ordinary label grammar        |
| D20 | `N(www.example)`           | `[D("w*" + 00 + ".example"), D(www.example)]` | `Match(1)`; NUL is ordinary name syntax after invalid-wildcard skipping      |
| D21 | `N(www.example)`           | `[D("*.exa" + 00 + "mple"), D(www.example)]`  | `Name(0, Nul)`; valid wildcard placement does not hide malformed name syntax |
| D22 | `N(www.example)`           | `[D("w*" + ff + ".example"), D(www.example)]` | `Name(0, NonAscii)`; invalid IA5 octet precedes wildcard handling            |

## Reference and certificate-name admission

Apply R01–R13 twice: first to `HttpsIdentity.reference(OriginHost.Dns)` and then to a manually
constructed DNS reference passed to `verify`; the expected reference failure is identical.
For the SAN column use valid reference `N(www.example)` with the listed bad name as the only SAN.

| ID  | Raw DNS bytes                                                                     | Reference result                                  | SAN result                                   |
| --- | --------------------------------------------------------------------------------- | ------------------------------------------------- | -------------------------------------------- |
| R01 | empty                                                                             | `Reference.EmptyName`                             | `Name(0, EmptyName)`                         |
| R02 | `a..example`                                                                      | `Reference.EmptyLabel`                            | `Name(0, EmptyLabel)`                        |
| R03 | `.example`                                                                        | `Reference.EmptyLabel`                            | `Name(0, EmptyLabel)`                        |
| R04 | `a^64 + .example`                                                                 | `Reference.LabelTooLong`                          | `Name(0, LabelTooLong)`                      |
| R05 | `a^63 + . + b^63 + . + c^63 + . + d^62` (254 bytes)                               | `Reference.NameTooLong`                           | `Name(0, NameTooLong)`                       |
| R06 | `www.example.` or `www.example..`                                                 | `Reference.TrailingDot`                           | `Name(0, TrailingDot)`                       |
| R07 | `"www" + 00 + ".example"`                                                         | `Reference.Nul`                                   | `Name(0, Nul)`                               |
| R08 | `"b" + c3 bc + "cher.example"` (UTF-8 U-label)                                    | `Reference.NonAscii`                              | `Name(0, NonAscii)`                          |
| R09 | `"a" + ff + ".example"`                                                           | `Reference.NonAscii`                              | `Name(0, NonAscii)`                          |
| R10 | `bad_name.example`, `a b.example`, `a%2eb.example`, `a/example`, `a:443`, `[::1]` | `Reference.InvalidCharacter`                      | `Name(0, InvalidCharacter)`                  |
| R11 | `-a.example` or `a-.example`                                                      | `Reference.EdgeHyphen`                            | `Name(0, EdgeHyphen)`                        |
| R12 | `192.0.2.1`, `192.0.2.999`, `127.1`, `2130706433`, `example.123`                  | `Reference.NumericFinalLabel`                     | `Name(0, NumericFinalLabel)`                 |
| R13 | `*.example`                                                                       | `Reference.WildcardReference`                     | Valid wildcard; `Match(0)` for `www.example` |
| R14 | `a^63 + .example`                                                                 | Admitted                                          | Exact comparison matches                     |
| R15 | `a^63 + . + b^63 + . + c^63 + . + d^61` (253 bytes)                               | Admitted                                          | Exact comparison matches                     |
| R16 | `xn--.example`                                                                    | `Reference.EdgeHyphen`                            | `Name(0, EdgeHyphen)`                        |
| R17 | `xn--a.example`                                                                   | Admitted as LDH bytes; no IDNA-validity assertion | Exact bytes match; no Punycode validation    |

## IP equality and upstream host preparation

| ID  | Origin/reference                                      | Certificate input                                      | Expected                                                                             |
| --- | ----------------------------------------------------- | ------------------------------------------------------ | ------------------------------------------------------------------------------------ |
| I01 | `V4(c0 00 02 01)`                                     | `[I(c0 00 02 01)]`                                     | `Match(0)`                                                                           |
| I02 | `V4(c0 00 02 01)`                                     | `[I(c0 00 02 02)]`                                     | `NoMatch`                                                                            |
| I03 | `V6(20 01 0d b8 00 00 00 00 00 00 00 00 00 00 00 01)` | Same sixteen octets in `I`                             | `Match(0)`                                                                           |
| I04 | Same as I03                                           | Same bytes except final octet `02`                     | `NoMatch`                                                                            |
| I05 | `V4(c0 00 02 01)`                                     | `[I(00 00 00 00 00 00 00 00 00 00 ff ff c0 00 02 01)]` | `NoMatch`; mapped IPv6 is a different family                                         |
| I06 | Same mapped address as I05 as `V6`                    | Same sixteen octets in `I`                             | `Match(0)`                                                                           |
| I07 | `V4(c0 00 02 01)`                                     | `[D(192.0.2.1)]`                                       | `Name(0, NumericFinalLabel)`; never a DNS match                                      |
| I08 | `N(www.example)`                                      | `[I(c0 00 02 01)]`                                     | `NoMatch`; resolved IP cannot authorize DNS reference                                |
| I09 | `V4(c0 00 02 01)`                                     | `[I(00^3)]`, `[I(00^5)]`, `[I(00^15)]`, `[I(00^17)]`   | `Identity(0, InvalidIpLength)`                                                       |
| I10 | `V4(c0 00 02 01)`                                     | `[I(c0 00 02 00 ff ff ff 00)]` or `[I(00^32)]`         | `Identity(0, InvalidIpLength)`; constraints are not SAN addresses                    |
| I11 | URI host `[2001:db8::1]` or `[2001:0DB8:0:0:0:0:0:1]` | I03 SAN                                                | Adapter yields I03 `V6`; then `Match(0)`                                             |
| I12 | URI host `[::ffff:192.0.2.1]`                         | I05 SAN                                                | Adapter yields I05 `V6`; then `Match(0)`                                             |
| I13 | URI host `[v1.example]`                               | Any                                                    | Adapter supplies `Unsupported.IpvFuture`; no match attempted                         |
| I14 | `fe80::1%eth0` or `[fe80::1%25eth0]`                  | Any                                                    | Adapter rejects as `Unsupported.ZoneIdentifier` before binary conversion             |
| I15 | URI `https://[2001:db8::1]:8443/a?q#f`                | I03 SAN                                                | Adapter validates HTTPS, excludes brackets/port/resource components, yields I03 `V6` |
| I16 | `[2001:::1]` or `[::1]:443` offered as one raw host   | Any                                                    | Upstream malformed-host failure; not a DNS fallback or a matcher IP input            |
| I17 | HTTPS host `www%2eexample` or `www.example.`          | Any                                                    | HTTPS admission rejects; generic URI validity does not imply admitted identity       |

I11–I17 are integration obligations, not a request to implement an IP or URI parser in this
follow-up. Core matcher tests start at explicit `OriginHost` data. The no-DNS-alias requirement
also needs an integration test once a connector exists.

## Mixed inputs, structural failure and unsupported forms

Reference is `N(www.example)` except where specified.

| ID  | Certificate/input                                                                             | Expected                                            |
| --- | --------------------------------------------------------------------------------------------- | --------------------------------------------------- |
| M01 | `[D(www.example), D(bad_name.example)]`                                                       | `Name(1, InvalidCharacter)`                         |
| M02 | Reverse M01                                                                                   | `Name(0, InvalidCharacter)`                         |
| M03 | `[D(www.example), I(00^3)]`                                                                   | `Identity(1, InvalidIpLength)` despite DNS match    |
| M04 | `V4(c0 00 02 01)` with `[I(c0 00 02 01), D("a" + ff)]`                                        | `Name(1, NonAscii)` despite IP match                |
| M05 | `Malformed.InvalidDer`, `InvalidGeneralName`, `DuplicateSanExtension`, or `EmptySanExtension` | Corresponding `Structure` error                     |
| M06 | `Decoded([])`                                                                                 | `Structure.EmptySanExtension`                       |
| M07 | `Absent`, certificate CN is `www.example`                                                     | `NoMatch`; CN is outside input model                |
| M08 | `[O(6, "https://www.example"), O(0, structurally valid SRVName)]`                             | `NoMatch`; URI-ID and SRV-ID cannot authorize HTTPS |
| M09 | `[O(6, "https://other.example"), D(www.example)]`                                             | `Match(1)`                                          |
| M10 | `Other` with tag 2, 7 or 9                                                                    | `Identity(0, WrongGeneralNameTag)`                  |
| M11 | Valid matching SAN, separate invalid chain or name constraints                                | `Match(0)` here; trust owner rejects connection     |
| M12 | Malformed DNS reference plus malformed certificate structure                                  | Reference failure wins                              |
| M13 | Structurally malformed input plus zero budgets                                                | Structure failure wins                              |
| M14 | `OriginHost.Unsupported` with `UriId` or `SrvId`                                              | Corresponding `Unsupported`                         |

SAN adapter integration must map DER/SAN structure faults to M05 even if a valid name was decoded
first. JUL-182's certificate decoder preserves extension payloads opaquely and retains duplicate
OIDs: successful envelope decoding can therefore precede M05 for malformed SAN contents or
duplicate SAN extensions. Those cases need the separate adapter, not a change to the envelope
decoder or an assumed existing GeneralNames API. A fabricated `Decoded` value is not proof that
real DER was validated. These notes clarify integration provenance without changing matrix v1
expectations or adding executable fixtures.

## Limits and work

Use `O(6, "https://example/" + a^k)` as valid unsupported padding; empty payloads may be supplied
directly for accounting tests but are not claims of RFC-valid URI SAN syntax.

| ID  | Limits/input                                                                              | Expected                                                                      |
| --- | ----------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------- |
| L01 | Standard count 256, 256 copies of `D(www.example)`                                        | `Match(0)`                                                                    |
| L02 | Standard count 256, 257 copies of `D(www.example)`                                        | `LimitExceeded.SanCount`                                                      |
| L03 | Standard bytes 65536; `D(www.example)` (11 bytes) plus one padding `Other` of 65525 bytes | `Match(0)`                                                                    |
| L04 | L03 with one additional padding byte                                                      | `LimitExceeded.SanBytes`                                                      |
| L05 | Both limits exceeded                                                                      | `LimitExceeded.SanCount`                                                      |
| L06 | `{maxSanCount: 0, maxSanBytes: 0}` with `Absent`                                          | `NoMatch`                                                                     |
| L07 | Same zero limits with one `D(www.example)`                                                | `LimitExceeded.SanCount`                                                      |
| L08 | `{maxSanCount: 1, maxSanBytes: 0}` with one `D(www.example)`                              | `LimitExceeded.SanBytes`                                                      |
| L09 | `{maxSanCount: 2, maxSanBytes: 11}` with two descriptors aliasing `www.example` bytes     | `LimitExceeded.SanBytes`; account for both occurrences                        |
| L10 | `{maxSanCount: 2, maxSanBytes: 11}` with `D(w*.example)` plus `D(www.example)`            | `LimitExceeded.SanBytes`; ignored wildcard consumes bytes                     |
| L11 | Oversized total plus malformed DNS entry                                                  | Limit error precedes content error                                            |
| L12 | Checked payload-length sum overflows `usize`                                              | `LimitExceeded.SanBytes`; structural/arithmetic proof, no enormous allocation |

The runtime follow-up must consolidate these logical cases into the cheapest appropriate shared
corpus program and semantic checks. A row is not a mandate for a new Vitest case, compiler run,
fresh process, native binary or timing assertion. Work/allocation properties use implementation
inspection and suitable structural evidence; performance stress belongs in opt-in benchmarks.
