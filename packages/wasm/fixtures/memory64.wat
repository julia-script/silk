(module $memory64
  (type (func (result i64)))
  (func $probe (type 0)
    i64.const 0
    i32.const 0
    i32.const 8
    memory.copy $wide $narrow
    i64.const 64
    i64.load offset=8589934592 align=1
    memory.size
    i64.add
    i64.const 0
    table.get $bigTable
    ref.is_null
    i64.extend_i32_u
    i64.add
  )
  (table $bigTable i64 2 funcref)
  (memory $wide i64 1 1048576)
  (memory $narrow 1)
  (export "probe" (func $probe))
  (elem (offset i64.const 0) funcref (ref.func $probe))
  (data (offset i64.const 64) "\07\07")
)
