#[no_mangle]
struct T {
  a: i32,

}
pub fn main() {
  let mut a:T  = T { a: 1,};
  a.a = 333;
}
