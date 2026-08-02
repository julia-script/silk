source_filename = "core.ll"
target datalayout = "e-p:64:64-i32:32"
target triple = "aarch64-unknown-linux"

define i32 @max_plus_one(i32 %v0, i32 %v1) {
entry:
  %condition = icmp sgt i32 %v0, %v1
  br i1 %condition, label %greater, label %lesser
greater:
  %increased_left = add i32 %v0, %v1
  br label %merge
lesser:
  %increased_right = sub i32 %v1, %v0
  br label %merge
merge:
  %result = phi i32 [ %increased_left, %greater ], [ %increased_right, %lesser ]
  ret i32 %result
}
define i32 @choose(i32 %v0) {
entry:
  switch i32 %v0, label %fallback [
    i32 0, label %selected
  ]
fallback:
  ret i32 1
selected:
  ret i32 0
}
define i32 @count_up(i32 %v0) {
entry:
  br label %repeat
repeat:
  %current = phi i32 [ %v0, %entry ], [ %next, %repeat ]
  %next = add i32 %current, 1
  %condition = icmp slt i32 %next, 10
  br i1 %condition, label %repeat, label %exit
exit:
  ret i32 %next
}
define i32 @integer_ops(i32 %v0, i32 %v1) {
entry:
  %add = add i32 %v0, %v1
  %sub = sub i32 %add, %v1
  %mul = mul i32 %sub, %v1
  %udiv = udiv i32 %mul, %v1
  %sdiv = sdiv i32 %udiv, %v1
  %urem = urem i32 %sdiv, %v1
  %srem = srem i32 %urem, %v1
  %shl = shl i32 %srem, %v1
  %lshr = lshr i32 %shl, %v1
  %ashr = ashr i32 %lshr, %v1
  %and = and i32 %ashr, %v1
  %or = or i32 %and, %v1
  %xor = xor i32 %or, %v1
  %negated = sub i32 zeroinitializer, %xor
  %inverted = xor i32 %negated, -1
  %eq = icmp eq i32 %v0, %v1
  %ne = icmp ne i32 %v0, %v1
  %ugt = icmp ugt i32 %v0, %v1
  %uge = icmp uge i32 %v0, %v1
  %ult = icmp ult i32 %v0, %v1
  %ule = icmp ule i32 %v0, %v1
  %sgt = icmp sgt i32 %v0, %v1
  %sge = icmp sge i32 %v0, %v1
  %slt = icmp slt i32 %v0, %v1
  %sle = icmp sle i32 %v0, %v1
  %selected = select i1 %sle, i32 %inverted, i32 %v0
  ret i32 %selected
}
define float @floating_ops(float %v0, float %v1) {
entry:
  %negated = fneg fast float %v0
  %fadd = fadd fast  float %negated, %v1
  %fsub = fsub fast  float %fadd, %v1
  %fmul = fmul fast  float %fsub, %v1
  %fdiv = fdiv fast  float %fmul, %v1
  %frem = frem fast  float %fdiv, %v1
  %false = fcmp false float %v0, %v1
  %oeq = fcmp fast oeq float %v0, %v1
  %ogt = fcmp fast ogt float %v0, %v1
  %oge = fcmp fast oge float %v0, %v1
  %olt = fcmp fast olt float %v0, %v1
  %ole = fcmp fast ole float %v0, %v1
  %one = fcmp fast one float %v0, %v1
  %ord = fcmp fast ord float %v0, %v1
  %ueq = fcmp fast ueq float %v0, %v1
  %ugt = fcmp fast ugt float %v0, %v1
  %uge = fcmp fast uge float %v0, %v1
  %ult = fcmp fast ult float %v0, %v1
  %ule = fcmp fast ule float %v0, %v1
  %une = fcmp fast une float %v0, %v1
  %uno = fcmp fast uno float %v0, %v1
  %true = fcmp fast true float %v0, %v1
  %selected = select fast i1 %true, float %frem, float %v0
  ret float %selected
}
define i32 @casts(i32 %v0, i16 %v1, i64 %v2, float %v3, double %v4, ptr %v5) {
entry:
  %truncated = trunc i32 %v0 to i16
  %zero_extended = zext i16 %v1 to i32
  %sign_extended = sext i16 %v1 to i32
  %float_unsigned = fptoui float %v3 to i32
  %float_signed = fptosi float %v3 to i32
  %unsigned_float = uitofp i32 %v0 to float
  %signed_float = sitofp i32 %v0 to float
  %float_truncated = fptrunc double %v4 to float
  %float_extended = fpext float %v3 to double
  %pointer_integer = ptrtoint ptr %v5 to i64
  %integer_pointer = inttoptr i64 %v2 to ptr
  %bit_cast = bitcast i32 %v0 to float
  %address_cast = addrspacecast ptr %v5 to ptr addrspace(1)
  ret i32 %v0
}
define i64 @aggregate_ops(i32 %v0, i64 %v1) {
entry:
  %with_first = insertvalue { i32, i64 } poison, i32 %v0, 0
  %complete = insertvalue { i32, i64 } %with_first, i64 %v1, 1
  %extracted = extractvalue { i32, i64 } %complete, 1
  ret i64 %extracted
}
define void @noop() {
entry:
  ret void
}
define void @trap() {
entry:
  unreachable
}
define i32 @call_max(i32 %v0, i32 %v1) {
entry:
  %result = call i32 @max_plus_one(i32 %v0, i32 %v1) [ "cold"() ]
  ret i32 %result
}
define i32 @tail_call(i32 %v0, i32 %v1) {
entry:
  %result = tail call i32 @max_plus_one(i32 %v0, i32 %v1)
  ret i32 %result
}
define i32 @musttail_call(i32 %v0, i32 %v1) {
entry:
  %result = musttail call i32 @max_plus_one(i32 %v0, i32 %v1)
  ret i32 %result
}
define i32 @notail_call(i32 %v0, i32 %v1) {
entry:
  %result = notail call i32 @max_plus_one(i32 %v0, i32 %v1)
  ret i32 %result
}
define float @fast_call(float %v0, float %v1) {
entry:
  %result = call fast float @floating_ops(float %v0, float %v1)
  ret float %result
}
define i32 @indirect_call(ptr %v0, i32 %v1, i32 %v2) {
entry:
  %result = call i32 %v0(i32 %v1, i32 %v2)
  ret i32 %result
}
declare i32 @variadic(i32, ...) nounwind
define i32 @variadic_call(i32 %v0) {
entry:
  %result = call i32 @variadic(i32 %v0, i32 1) nounwind
  ret i32 %result
}
