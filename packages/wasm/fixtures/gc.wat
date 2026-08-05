(module $gc
  (type $node (struct (field i32) (field (mut (ref null $node)))))
  (type $base (sub (struct (field i32))))
  (type $extended (sub final $base (struct (field i32) (field (mut f64)))))
  (type $bytes (array (mut i8)))
  (type (func (param i32) (result i32)))
  (func $twice (type 4)
    local.get 0
    local.get 0
    i32.add
  )
  (func $run (type 4)
    (local $list (ref null $node))
    (local $buffer (ref null $bytes))
    local.get 0
    ref.null $node
    struct.new $node
    local.set 1
    i32.const 1
    local.get 1
    struct.new $node
    local.set 1
    i32.const 0
    i32.const 3
    array.new_data $bytes $greeting
    local.tee 2
    array.len
    i32.const 2
    struct.new_default $base
    ref.cast (ref $base)
    struct.get $base 0
    i32.add
    i32.const 5
    ref.i31
    ref.test (ref i31)
    i32.add
    i32.add
    ref.func $twice
    call_ref 4
    block (result i32)
      block
        local.get 1
        br_on_null 0
        struct.get $node 0
        br 1
      end
      i32.const -1
    end
    i32.add
    struct.new_default $extended
    ref.is_null
    i32.add
    i32.const 4
    array.new_default $bytes
    array.len
    i32.add
  )
  (export "run" (func $run))
  (export "twice" (func $twice))
  (data $greeting "hi!")
)
