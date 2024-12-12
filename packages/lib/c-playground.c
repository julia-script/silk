
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
struct MyStruct {
  char varSignedChar;
  unsigned char varUnsignedChar;
  int varInt;
  unsigned int varUnsignedInt;
};
// extern void p(char* fmt, ...);
char playground(char n) { return n + 2; }