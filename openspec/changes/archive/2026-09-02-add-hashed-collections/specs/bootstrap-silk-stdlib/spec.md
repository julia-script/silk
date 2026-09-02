## ADDED Requirements

### Requirement: The hashed collections ship as canonical Silk source

Canonical standard-library source SHALL define `HashKey`, `HashSeed`, `HashMap<K, V>`, and
`HashSet<T>`, registered in the shipped manifest under their own namespace so they resolve through
the ordinary module closure without being vendored into a user's source set. The generated
standard-library documentation and module table SHALL be regenerated from the manifest rather than
edited by hand.

#### Scenario: Import the hashed collections without vendoring

- **WHEN** a user program imports the hashed collection module and does not contain its source
- **THEN** resolution succeeds through the ordinary module closure and the resolved declarations carry the library's canonical module identity

#### Scenario: Generated documentation matches the manifest

- **WHEN** the generated standard-library documentation is checked against the manifest
- **THEN** the check passes without any hand-edited difference
