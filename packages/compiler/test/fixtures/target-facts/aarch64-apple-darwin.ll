; ModuleID = 'primitives.c'
source_filename = "primitives.c"
target datalayout = "e-m:o-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-n32:64-S128-Fn32"
target triple = "arm64-apple-macosx11.0.0"

@silk_primitive_facts = constant [20 x i32] [i32 4, i32 4, i32 1, i32 1, i32 1, i32 1, i32 2, i32 2, i32 4, i32 4, i32 8, i32 8, i32 4, i32 4, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8], align 4

!llvm.module.flags = !{!0, !1, !2}
!llvm.ident = !{!3}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"frame-pointer", i32 4}
!3 = !{!"Homebrew clang version 22.1.8"}
