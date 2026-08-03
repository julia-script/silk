# Design the bootstrap compiler pipeline and intermediate representations

Type: grilling
Status: open
Blocked by: 01, 02, 03, 04, 05

## Question

What staged compiler architecture takes Silk Effect source through parsing, name and type analysis,
function-contract checking, ownership checking, lowering, LLVM emission, and native linking while
remaining simple enough to port from Effect TypeScript to Silk Effect and avoiding gratuitous
barriers to a later direct WebAssembly backend?
