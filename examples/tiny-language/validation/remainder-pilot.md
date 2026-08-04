# Remainder exercise pilot

The transfer checklist was followed in a clean packed-package consumer without consulting a
finished patch in the lesson prose.

Validated outcomes:

- `%` tokenization retained exact spans.
- `10 + 7 % 4 * 2` rendered as `(+ 10 (* (% 7 4) 2))`.
- Both reference programs emitted signed `srem`.
- LLVM 22 compiled the generated modules.
- Native exit statuses were `16` for mixed precedence and `1` for `isOdd(7)`.
- The complete pre-existing suite stayed green.

The four hint levels were sufficient to locate the operator path without supplying a
solution-equivalent patch.
