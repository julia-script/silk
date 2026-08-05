(module $branch-hints
  (type (func (param i32) (result i32)))
  (func $fast (type 0)
    block
      local.get 0
      i32.eqz
      (@metadata.code.branch_hint "\00")
      br_if 0
    end
    local.get 0
    (@metadata.code.branch_hint "\01")
    if (result i32)
      local.get 0
    else
      i32.const -1
    end
  )
  (func $slow (type 0)
    local.get 0
    (@metadata.code.branch_hint "\00")
    if (result i32)
      i32.const 1
    else
      i32.const 0
    end
  )
  (export "fast" (func $fast))
  (export "slow" (func $slow))
)
