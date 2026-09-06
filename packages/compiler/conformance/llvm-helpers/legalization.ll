; Synthetic LLVM legalization witness: every request below originates in compiler IR.
declare void @llvm.memcpy.p0.p0.i64(ptr, ptr, i64, i1 immarg)
declare void @llvm.memmove.p0.p0.i64(ptr, ptr, i64, i1 immarg)
declare void @llvm.memset.p0.i64(ptr, i8, i64, i1 immarg)
declare i32 @memcmp(ptr, ptr, i64)
define void @copy_fixture(ptr %d, ptr %s, i64 %n) {
  call void @llvm.memcpy.p0.p0.i64(ptr %d, ptr %s, i64 %n, i1 false)
  ret void
}
define void @move_fixture(ptr %d, ptr %s, i64 %n) {
  call void @llvm.memmove.p0.p0.i64(ptr %d, ptr %s, i64 %n, i1 false)
  ret void
}
define void @fill_fixture(ptr %d, i8 %v, i64 %n) {
  call void @llvm.memset.p0.i64(ptr %d, i8 %v, i64 %n, i1 false)
  ret void
}
define i32 @compare_fixture(ptr %a, ptr %b, i64 %n) {
  %result = call i32 @memcmp(ptr %a, ptr %b, i64 %n)
  ret i32 %result
}
define double @remainder_fixture(double %a, double %b) {
  %result = frem double %a, %b
  ret double %result
}
define float @remainder_float_fixture(float %a, float %b) {
  %result = frem float %a, %b
  ret float %result
}

define i32 @equal_fixture(ptr %a, ptr %b, i64 %n) {
  %r = call i32 @memcmp(ptr %a, ptr %b, i64 %n)
  %same = icmp eq i32 %r, 0
  %result = zext i1 %same to i32
  ret i32 %result
}
define void @zero_fixture(ptr %d, i64 %n) {
  call void @llvm.memset.p0.i64(ptr %d, i8 0, i64 %n, i1 false)
  ret void
}
