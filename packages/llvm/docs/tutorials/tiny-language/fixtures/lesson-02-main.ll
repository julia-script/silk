; This module came from a source file named answer.tiny.
source_filename = "answer.tiny"

; `define` begins a function definition.
; `i32` is its return type, `@main` is its name, and `()` is its parameter list.
define i32 @main() {
entry:
  ; `ret` is an instruction and the block's terminator.
  ; Its `i32 42` operand matches the function's return type.
  ret i32 42
}
