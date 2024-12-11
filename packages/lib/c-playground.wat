(module $c-playground.wasm
  (type (;0;) (func (param i32)))
  (func $fib (type 0) (param i32)
    (local i32 i32 i32 i32 i32 i32 i64)
    global.get $__stack_pointer
    local.set 1
    i32.const 16
    local.set 2
    local.get 1
    local.get 2
    i32.sub
    local.set 3
    local.get 3
    local.get 0
    i32.store offset=12
    i32.const 8
    local.set 4
    local.get 3
    local.get 4
    i32.add
    local.set 5
    i32.const 0
    local.set 6
    local.get 5
    local.get 6
    i32.store
    i64.const 0
    local.set 7
    local.get 3
    local.get 7
    i64.store
    return)
  (memory (;0;) 2)
  (global $__stack_pointer (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "fib" (func $fib)))
