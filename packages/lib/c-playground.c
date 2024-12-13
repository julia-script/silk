
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
int len(int arr[]) { return 1; }
// extern void p(char* fmt, ...);
void playground() {
  int arr[4] = {1, 2, 3, 4};
  int arr2[4] = {1, 2, 3, 4};
  len(arr);
  // int i = 0;
  // int sum = 0;
  // while (i < len(arr)) {
  //   sum = sum + arr[i];
  //   i = i + 1;
  // }
  // return sum;
  // //   var arr :[1]i32 = [1]i32 {3}
  // var i: usize = 0;
  // var sum: i32 = 0;
  // while (i < arr.len) {
  //       sum = sum + arr[i];
  //       i = i + 1;
  // }
  // return sum;
}
