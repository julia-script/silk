# Context

`Instances.Discovery` already contains the concrete graph, substitutions, residual bodies, calls,
effects, constants, and reachability evidence. Its `SemanticContext.Registry` field is the remaining
live presentation attachment.

# Decisions

- `Realization.instantiate` is the public producer and takes a closed explicit input record rather
  than `Frontend`.
- Presentation may be derived transiently while constructing source-located facts, but it is not
  retained by the returned graph.
- Diagnostics and MIR lowering accept a registry beside the graph when converting authored anchors
  to current spans.
- Invalid discovery returns the same artifact shape without manufacturing an empty registry.

# Risks

Consumers could accidentally regain ambient presentation through a coordinator. Signature checks
and the absence of a registry field on the artifact keep that boundary visible.
