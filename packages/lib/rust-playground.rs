#![allow(warnings)]
#[no_mangle]
#[derive(Debug)]
struct T {
  a: i32,

}
pub fn main() {
  let mut a:T  = T { a: 1};
  let mut b = a;
  b.a = 3;
  // a.a = 3;
  println!("a: {}", a.a);
  
  // a.a = 333;
}
