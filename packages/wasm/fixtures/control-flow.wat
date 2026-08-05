(module $control-flow
  (type (func (param i32) (result i32)))
  (type (func (param i32) (result f64)))
  (func $classify (type 0)
    block (result i32)
      block
        block
          local.get 0
          br_table 0 1 1
        end
        i32.const 100
        br 1
      end
      loop
        local.get 0
        i32.const 100
        i32.gt_s
        br_if 0
      end
      local.get 0
      if (result i32)
        i32.const 1
      else
        i32.const 2
      end
    end
  )
  (func $even (type 0)
    local.get 0
    i32.eqz
    if (result i32)
      i32.const 1
    else
      local.get 0
      i32.const 1
      i32.sub
      return_call $odd
    end
  )
  (func $odd (type 0)
    local.get 0
    i32.eqz
    if (result i32)
      i32.const 0
    else
      local.get 0
      i32.const 1
      i32.sub
      return_call $even
    end
  )
  (func $pick (type 1)
    f64.const 1.5
    f64.const -0
    local.get 0
    select
    unreachable
  )
  (export "classify" (func $classify))
  (export "even" (func $even))
)
