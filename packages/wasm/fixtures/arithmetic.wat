(module $arithmetic
  (type (func (param i32 i32) (result i32)))
  (type (func (param f64) (result i64)))
  (type (func (result i64)))
  (func $i32_add (type 0)
    local.get 0
    local.get 1
    i32.add
  )
  (func $i32_sub (type 0)
    local.get 0
    local.get 1
    i32.sub
  )
  (func $i32_mul (type 0)
    local.get 0
    local.get 1
    i32.mul
  )
  (func $i32_div_s (type 0)
    local.get 0
    local.get 1
    i32.div_s
  )
  (func $i32_rem_u (type 0)
    local.get 0
    local.get 1
    i32.rem_u
  )
  (func $i32_xor (type 0)
    local.get 0
    local.get 1
    i32.xor
  )
  (func $convert (type 1)
    (local $narrow f32)
    local.get 0
    f32.demote_f64
    local.tee 1
    f32.sqrt
    i64.trunc_sat_f32_s
    local.get 0
    i64.trunc_sat_f64_u
    i64.add
    i64.extend32_s
  )
  (func $wide (type 2)
    i64.const 9223372036854775807
    i64.const -1
    i64.xor
    f64.const 2.5
    i64.trunc_f64_s
    i64.or
  )
  (export "i32.add" (func $i32_add))
  (export "i32.sub" (func $i32_sub))
  (export "i32.mul" (func $i32_mul))
  (export "i32.div_s" (func $i32_div_s))
  (export "i32.rem_u" (func $i32_rem_u))
  (export "i32.xor" (func $i32_xor))
  (export "convert" (func $convert))
  (export "wide" (func $wide))
)
