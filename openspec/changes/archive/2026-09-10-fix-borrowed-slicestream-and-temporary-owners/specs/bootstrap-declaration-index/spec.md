## ADDED Requirements

### Requirement: Conformance owner lifetimes elaborate before contract publication

An interface conformance header omitting nominal provider lifetime arguments SHALL introduce independent declaration-relative impl binders using the provider's completed lifetime arity. Its operation contracts SHALL retain those binders wherever the applied provider or Self uses them. Ordinary type arguments, bounds, coherence and static conformance selection SHALL remain equivalent to an explicitly bound lifetime form. Elision SHALL NOT inspect implementation bodies or change the fixed applied owner denoted by Self.

#### Scenario: Apply an elided borrowed holder conformance

- **WHEN** a borrowed SliceStream<A> implements Stream<A, never ? never> without written provider lifetime arguments
- **THEN** the completed conformance and its calls have the same lifetime relationships as the explicitly bound provider form
- **AND** Copy bounds and ordinary type argument arity remain enforced
