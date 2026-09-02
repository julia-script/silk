## ADDED Requirements

### Requirement: A Path admits exact platform bytes

`Path` SHALL offer construction from exact platform bytes and a borrowed view of those bytes. Both
SHALL apply every normalization rule textual construction applies — absolute, NUL-rejecting, and
rejecting `.`, `..`, empty components, and trailing separators — and SHALL lift only the requirement
that the value be well-formed text. The `string` construction and view operations SHALL keep their
existing checked meaning, and this capability MUST NOT introduce a second text or path type.

#### Scenario: Round-trip a platform path that is not text

- **WHEN** a Path is built from bytes that are not well-formed text
- **THEN** the byte view returns those exact bytes unchanged

#### Scenario: Keep normalization for byte construction

- **WHEN** byte construction receives a relative path, a NUL, or an unnormalized component
- **THEN** it is rejected exactly as the textual constructor rejects it
