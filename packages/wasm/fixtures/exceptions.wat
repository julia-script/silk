(module $exceptions
  (type (func (param i32)))
  (type (func))
  (type (func (result i32 exnref)))
  (type (func (param i32) (result i32)))
  (import "env" "panic" (tag $panic (type 1)))
  (func $guarded (type 3)
    block (result i32)
      block (type 2)
        block (result exnref)
          block
            try_table (catch $error 3) (catch_ref $error 2) (catch_all 0) (catch_all_ref 1)
              local.get 0
              i32.eqz
              if
                i32.const 41
                throw $error
              end
              local.get 0
              i32.const 100
              i32.gt_s
              if
                throw $panic
              end
            end
            i32.const 7
            br 3
          end
          i32.const -1
          br 2
        end
        throw_ref
      end
      drop
    end
    i32.const 1
    i32.add
  )
  (tag $error (type 0))
  (export "guarded" (func $guarded))
  (export "error" (tag $error))
)
