source_filename = "decl.ll"
target datalayout = "e-p:64:64-i32:32"
target triple = "aarch64-unknown-linux"

@answer = internal constant i32 1
@answer_alias = alias i32, ptr @answer
declare i32 @compute(i32) nounwind
