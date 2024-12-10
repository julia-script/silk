  // const slice: [a]i32 = [1, 2, 3, 4, 5];
  // const array_of_value = [1024]i32 {1};
  
export fn fib(n: i32): i32 {
  // const sample: i32 = 1;
  // const array: [5]i32 = [1, 2, 3, 4, 5];
  // const inferred_size_array: [_]i32 = [1, 2, 3, 4, 5];
  // const mutable_slice: mut []i32 = [];
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

// const a:i32 = 1;

// export fn rec(n: f64): f64 {
//   // if (n < 10) {
//      rec(n + 1);
//          var a = 3 + 4 + rec(n + 1);
//   // }
//   return n;
// }
