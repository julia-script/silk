
// clang --target=wasm32 -nostdlib -Wl,--no-entry -Wl,--export=fib -o fib.wasm
// fib.c clang --target=wasm32 -O0 -S -emit-llvm fib.c -o fib.ll -o fib.wasm
// fib.c && wasm2wat fib.wasm -o fib.wat
// int fib(int n) {
//   int a = 0, b = 1;
//   if (n > 0) {
//     while (--n) {
//       int t = a + b;
//       a = b;
//       b = t;
//     }
//     return b;
//   }
//   return a;
// }
#include <stdio.h>

// type E = struct {
//   a: i8,
//   b: i32,
// }

// type F = struct {
//   a: i8,
//   b: E,
//   c: i32,
// }
struct E {
  char a;
  int b;
};

struct F {
  struct E b;
  int c;
  char a;
};

int main() {
  printf("Size of E: %zu bytes\n", sizeof(struct E));
  printf("Alignment of E: %zu bytes\n", _Alignof(struct E));

  printf("Size of F: %zu bytes\n", sizeof(struct F));
  printf("Alignment of F: %zu bytes\n", _Alignof(struct F));

  return 0;
}