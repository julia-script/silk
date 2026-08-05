(module $segments
  (type (func (result i32)))
  (type (func))
  (type (func (param i32) (result i32)))
  (func $first (type 0)
    i32.const 1
  )
  (func $second (type 0)
    i32.const 2
  )
  (func $setup (type 1)
    i32.const 0
    i32.const 0
    i32.const 2
    table.init $funcs 1
    elem.drop 1
    i32.const 32
    i32.const 0
    i32.const 6
    memory.init $greeting
    data.drop $greeting
    i32.const 0
    i32.const 16
    i32.const 4
    memory.copy
    i32.const 48
    i32.const 0
    i32.const 8
    memory.fill
    i32.const 0
    ref.null func
    i32.const 1
    table.fill $funcs
    i32.const 0
    i32.const 0
    i32.const 1
    table.copy $funcs $funcs
    ref.null extern
    i32.const 1
    table.grow $externs
    drop
    i32.const 0
    i32.const 0
    table.get $funcs
    table.set $funcs
    table.size $externs
    drop
  )
  (func $chooser (type 2)
    local.get 0
    call_indirect $funcs (type 0)
  )
  (table $funcs 4 8 funcref)
  (table $externs 2 externref)
  (memory $memory 1)
  (export "setup" (func $setup))
  (export "chooser" (func $chooser))
  (elem (offset i32.const 0) funcref (ref.func $first) (ref.func $second))
  (elem funcref (ref.func $second) (ref.null func))
  (elem declare funcref (ref.func $first))
  (data (offset i32.const 16) "\01\02\03\04")
  (data $greeting "hi\00\ff\"\\")
)
