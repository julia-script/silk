(module $atomics
  (type (func (param i32) (result i32)))
  (func $bump (type 0)
    i32.const 0
    local.get 0
    i32.atomic.rmw.add
    atomic.fence
    i32.const 0
    i32.const 1
    memory.atomic.notify offset=4
    i32.add
  )
  (memory $shared 1 4 shared)
  (export "bump" (func $bump))
  (export "shared" (memory $shared))
)
