# Choose the ownership, lifetime, and scoped-allocation model

Type: grilling
Status: open

## Question

What ownership, borrowing, lifetime, and scope rules give safe code deterministic reclamation and
explicit dynamic allocation without requiring manual `free`, a tracing garbage collector, or a
full Rust-compatible borrow checker—and are expressive enough to implement the bootstrap compiler?
