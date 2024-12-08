(module $fib.wasm
  (type (;0;) (func (param i32) (result i32)))
  (func $fib (type 0) (param i32) (result i32)
    (local i32 i32 i32)
    i32.const 1
    local.set 1
    block  ;; label = @1
      local.get 0
      i32.const 1
      i32.ge_s
      br_if 0 (;@1;)
      i32.const 0
      return
    end
    local.get 0
    i32.const 7
    i32.and
    local.set 2
    block  ;; label = @1
      block  ;; label = @2
        local.get 0
        i32.const 8
        i32.ge_u
        br_if 0 (;@2;)
        i32.const 0
        local.set 0
        br 1 (;@1;)
      end
      local.get 0
      i32.const 2147483640
      i32.and
      local.set 3
      i32.const 1
      local.set 1
      i32.const 0
      local.set 0
      loop  ;; label = @2
        local.get 1
        local.get 0
        i32.add
        local.tee 0
        local.get 1
        i32.add
        local.tee 1
        local.get 0
        i32.add
        local.tee 0
        local.get 1
        i32.add
        local.tee 1
        local.get 0
        i32.add
        local.tee 0
        local.get 1
        i32.add
        local.tee 1
        local.get 0
        i32.add
        local.tee 0
        local.get 1
        i32.add
        local.set 1
        local.get 3
        i32.const -8
        i32.add
        local.tee 3
        br_if 0 (;@2;)
      end
    end
    block  ;; label = @1
      local.get 2
      i32.eqz
      br_if 0 (;@1;)
      local.get 0
      local.set 3
      loop  ;; label = @2
        local.get 1
        local.tee 0
        local.get 3
        i32.add
        local.set 1
        local.get 0
        local.set 3
        local.get 2
        i32.const -1
        i32.add
        local.tee 2
        br_if 0 (;@2;)
      end
    end
    local.get 0)
  (memory (;0;) 2)
  (global $__stack_pointer (mut i32) (i32.const 66560))
  (export "memory" (memory 0))
  (export "fib" (func $fib)))
