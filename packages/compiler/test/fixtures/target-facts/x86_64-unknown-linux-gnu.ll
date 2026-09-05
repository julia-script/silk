; ModuleID = 'packages/compiler/test/fixtures/target-facts/primitives.c'
source_filename = "packages/compiler/test/fixtures/target-facts/primitives.c"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

@silk_primitive_facts = dso_local constant [20 x i32] [i32 4, i32 4, i32 1, i32 1, i32 1, i32 1, i32 2, i32 2, i32 4, i32 4, i32 8, i32 8, i32 4, i32 4, i32 8, i32 8, i32 8, i32 8, i32 8, i32 8], align 16

!llvm.module.flags = !{!0, !1, !2, !3}
!llvm.ident = !{!4}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{i32 8, !"PIC Level", i32 2}
!2 = !{i32 7, !"PIE Level", i32 2}
!3 = !{i32 7, !"frame-pointer", i32 2}
!4 = !{!"Homebrew clang version 22.1.8"}
