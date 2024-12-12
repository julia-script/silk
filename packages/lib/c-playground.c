
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
  char var0;
  char var2;
  char var3;
  int var1;
  char var4;
};
// extern void p(char* fmt, ...);
void fib(int n) {
  // struct MyStruct s1;
  // s1.var2 = s1.var0;
  // s1.var2 = sizeof(s1);

  int myNumbers[] = {0, 0, 0};
  myNumbers[2] = myNumbers[1];
  // // int i;

  // for (i = 0; i < 4; i++) {
  //   p("%d\n", myNumbers[i]);
  // }
}