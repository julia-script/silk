// const __memory_base: i32 = 1024
// var __stack_pointer: i32 = 1024
// fn __stack_alloc(size: usize) i32 {
//     __stack_pointer = __stack_pointer - size;
//     return __stack_pointer;
// }

// fn __stack_free(size: u32) void {
//   __stack_pointer = __stack_pointer + size;
// }

// fn __stack_alloc_i32_array(size: i32) i32 {
//   return __stack_alloc((1 + size) * 4);
// }
// fn sum_array() i32 {
//     var arr = [1]i32 {1, 2, 3}
//     var i: usize = 0;
//     var sum: i32 = 0;
//     while (i < arr.len) {
//           sum = sum + arr[i];
//           i = i + 1;
//     }
//     return sum;
// }
export fn fib(n: i32): i32 {
  var a:i32 = 0;
  var b:i32 = 1;
  if (n > 0) {
    while (n > 1) {
      var t:i32 = a + b;
      a = b;
      b = t;
      n = n - 1;
    }
    return b
  }
  return a
}
// fn main() void {
//     // const slice: [5]i32 = _{1, 2, 3, 4, 5};
//     // foo.bar.baz(){
//     //   a: 3,
//     //   b: 4,
//     //   c: 5,
//     // };
// }

// const array_of_value = [1024]i32 {1};

// const sample: i32 = 1;
// const array: [5]i32 = [1, 2, 3, 4, 5];
// const inferred_size_array: [_]i32 = [1, 2, 3, 4, 5];
// const mutable_slice: mut []i32 = [];
// const a: i32 = 1
// const a: i32 = 1

// export fn rec(n: f64): f64 {
//   // if (n < 10) {
//      rec(n + 1);
//          var a = 3 + 4 + rec(n + 1);
//   // }
//   return n;
// }
