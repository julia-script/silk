; ModuleID = 'c-playground.c'
source_filename = "c-playground.c"
target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-n32:64-S128-ni:1:10:20"
target triple = "wasm32"

; Function Attrs: noinline nounwind optnone
define hidden void @fib(i32 noundef %0) #0 {
  %2 = alloca i32, align 4
  %3 = alloca [3 x i32], align 4
  store i32 %0, ptr %2, align 4
  call void @llvm.memset.p0.i32(ptr align 4 %3, i8 0, i32 12, i1 false)
  ret void
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i32(ptr nocapture writeonly, i8, i32, i1 immarg) #1

attributes #0 = { noinline nounwind optnone "no-trapping-math"="true" "stack-protector-buffer-size"="8" "target-cpu"="generic" "target-features"="+multivalue,+mutable-globals,+reference-types,+sign-ext" }
attributes #1 = { nocallback nofree nounwind willreturn memory(argmem: write) }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"Homebrew clang version 19.1.5"}
