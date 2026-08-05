(module $declarations
  (type (func (param i32)))
  (type (func))
  (import "env" "log" (func $log (type 0)))
  (import "env" "indirect" (table $indirect 4 funcref))
  (import "env" "shared" (memory $shared 1 8))
  (import "env" "base" (global $base i32))
  (func $main (type 1)
    global.get $offset
    call $log
    global.get $cursor
    i64.const 1
    i64.add
    global.set $cursor
    i32.const 0
    i32.const 0
    i32.const 64
    memory.copy $scratch $shared
    memory.size
    memory.grow $scratch
    drop
  )
  (memory $scratch 2)
  (global $offset i32 global.get $base i32.const 64 i32.mul)
  (global $cursor (mut i64) i64.const 0)
  (global $externSlot (mut externref) ref.null extern)
  (export "main" (func $main))
  (export "scratch" (memory $scratch))
  (export "indirect" (table $indirect))
  (export "cursor" (global $cursor))
  (export "externSlot" (global $externSlot))
  (start $main)
)
