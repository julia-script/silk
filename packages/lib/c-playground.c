
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

// extern void p(char* fmt, ...);
void fib(int n) {
  int myNumbers[] = {0, 0, 0};
  myNumbers[2] = n;
  // int i;

  // for (i = 0; i < 4; i++) {
  //   p("%d\n", myNumbers[i]);
  // }
}