export fn fib(n: i32): i32 {
  var a:i32 = 0;
  var b:i32 = 1;
    // fib(a, b);
  if (n > 0) {
    while (n > 0) {
      n = n - 1;
      var t:i32 = a + b;
      a = b;
      b = t;
    }
    return b
  }
  return a
}

// const a:i32 = 1;

// export fn rec(n: f64): f64 {
//  return n + 2 / 3 * 4;
// }
