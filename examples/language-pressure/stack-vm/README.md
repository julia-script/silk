# Silk stack VM pressure program

This is a complete bounded stack bytecode interpreter written in ordinary Silk. It consumes a
runtime-sized borrowed byte slice, executes with a visible 16-slot `i32` operand stack, and returns
one owned growable ordered stream of executed-step and VM-diagnostic events.

It is a language-pressure exercise, not a production VM or self-hosting milestone. The TypeScript
reference in the acceptance test remains the differential oracle. The bytecode uses local numeric
opcode values deliberately: the point is to learn whether a second real program confirms the
lexer's pressure for named closed values before designing that feature.

The checked-in program branches around a dead path and computes `6 * 7`. Tests replace only the
marked bytecode and fingerprint literals to run valid, malformed, allocation-failure, and
determinism corpora.
