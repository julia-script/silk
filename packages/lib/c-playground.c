struct T {
  int a;
};

// fn should_alloc_1(): void {
//   const a = T { a = 1 };
// }

// fn should_alloc_2(): void {
//   var b = T { a = 2 };
// }

// fn should_alloc_3(): void {
//   const c: i32 = 10;
// }

// fn should_not_alloc_1(b: T): void {
//   b.a = 3;
// }

// fn should_not_alloc_2(b: T): void {
//   b = T { a = 4 };
// }

// fn should_not_alloc_3(a: T): void {
//   const b = a;
// }
void playground() { char greetings[] = "Hello World!"; }
