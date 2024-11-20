(module
(func $add (param $a i32) )
  ;; (func $add (param $a i32) (param $b i32) (result i32)
  ;;   local.get $a       ;; Push the value of local variable 'a' onto the stack
  ;;   local.get $b       ;; Push the value of local variable 'b' onto the stack
  ;;   i32.add            ;; Pop two integers from the stack, add them, push the result
  ;; )
  ;; (export "add" (func $add))  ;; Export the function as 'add'
)