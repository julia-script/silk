; ModuleID = 'packages/compiler/test/fixtures/target-facts/primitives.c'
source_filename = "packages/compiler/test/fixtures/target-facts/primitives.c"
target datalayout = "e-m:e-p:32:32-p10:8:8-p20:8:8-i64:64-i128:128-n32:64-S128-ni:1:10:20"
target triple = "wasm32-unknown-unknown"

@silk_primitive_facts = hidden constant [18 x i32] [i32 1, i32 1, i32 1, i32 1, i32 2, i32 2, i32 4, i32 4, i32 8, i32 8, i32 4, i32 4, i32 8, i32 8, i32 4, i32 4, i32 4, i32 4], align 16

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"Homebrew clang version 22.1.8"}
