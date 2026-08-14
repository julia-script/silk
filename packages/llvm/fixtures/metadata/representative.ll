source_filename = "metadata.c"
target datalayout = "e-p:64:64-i32:32-i64:64-n32:64"
target triple = "aarch64-unknown-linux"

@counter = global i32 0, !dbg !32
define void @debugged(i1 %v0) !dbg !25 {
entry:
  %slot = alloca i32, i32 1, !dbg !27
  store i32 1, ptr %slot, !dbg !27
  br i1 %v0, label %yes, label %no, !dbg !28, !prof !35
yes:
  ret void
no:
  ret void
}

!llvm.dbg.cu = !{!22}
!llvm.module.flags = !{!23, !24}
!silk.metadata.inventory = !{!34}
!0 = !DIFile(filename: "metadata.c", directory: "/src")
!1 = !DIBasicType(name: "bool", size: 1, encoding: DW_ATE_boolean)
!2 = !DIBasicType(name: "u32", size: 32, encoding: DW_ATE_unsigned)
!3 = !DIBasicType(name: "i32", size: 32, encoding: DW_ATE_signed)
!4 = !DIBasicType(name: "f64", size: 64, encoding: DW_ATE_float)
!5 = !DIDerivedType(tag: DW_TAG_pointer_type, name: "NodePtr", line: 0, baseType: !8, size: 64, align: 64, offset: 0, flags: 0)
!6 = !DIDerivedType(tag: DW_TAG_member, name: "next", file: !0, line: 0, baseType: !5, size: 64, align: 64, offset: 0, flags: 0)
!7 = !{!6}
!8 = !DICompositeType(tag: DW_TAG_structure_type, name: "Node", file: !0, line: 0, size: 64, align: 64, flags: 0, elements: !7)
!9 = !{!3, !4}
!10 = !DICompositeType(tag: DW_TAG_union_type, name: "Number", file: !0, line: 0, size: 64, align: 64, flags: 0, elements: !9)
!11 = !DIEnumerator(name: "Two", value: 2, isUnsigned: true)
!12 = !{!11}
!13 = !DICompositeType(tag: DW_TAG_enumeration_type, name: "Kind", file: !0, line: 0, baseType: !2, size: 32, align: 32, flags: 0, elements: !12)
!14 = !DISubrange(count: i32 2, lowerBound: i32 0)
!15 = !{!14}
!16 = !DICompositeType(tag: DW_TAG_array_type, line: 0, baseType: !3, size: 64, align: 32, flags: 0, elements: !15)
!17 = !DICompositeType(tag: DW_TAG_array_type, line: 0, baseType: !3, size: 128, align: 128, flags: DIFlagVector, elements: !15)
!18 = !DIDerivedType(tag: DW_TAG_typedef, name: "Integer", file: !0, line: 0, baseType: !3, size: 32, align: 32, offset: 0, flags: 0)
!19 = !{null, !1, !2, !3, !4, !8, !10, !13, !16, !17, !18}
!20 = !DISubroutineType(types: !19, flags: DIFlagPrototyped)
!21 = !{}
!22 = distinct !DICompileUnit(language: DW_LANG_C99, file: !0, producer: "silk-effect", isOptimized: true, runtimeVersion: 0, emissionKind: FullDebug, globals: !21, splitDebugInlining: false)
!23 = !{i32 2, !"Dwarf Version", i32 5}
!24 = !{i32 2, !"Debug Info Version", i32 3}
!25 = distinct !DISubprogram(scope: !0, name: "debugged", file: !0, line: 10, type: !20, scopeLine: 10, flags: DIFlagPrototyped | DIFlagAllCallsDescribed, spFlags: DISPFlagDefinition | DISPFlagOptimized, unit: !22)
!26 = !DILexicalBlock(scope: !25, file: !0, line: 11, column: 1)
!27 = !DILocation(line: 11, column: 3, scope: !26)
!28 = !DILocation(line: 12, column: 1, scope: !26)
!29 = !DILocalVariable(name: "value", scope: !26, file: !0, line: 11, type: !3, flags: 0)
!30 = !DILocalVariable(name: "condition", arg: 1, scope: !25, file: !0, line: 10, type: !1, flags: 0)
!31 = distinct !DIGlobalVariable(name: "counter", linkageName: "counter", scope: !22, file: !0, line: 3, type: !3, isLocal: false, isDefinition: true)
!32 = !DIGlobalVariableExpression(var: !31, expr: !DIExpression(35, 0))
!33 = distinct !{!"local-node"}
!34 = distinct !{!8, !10, !13, !16, !17, !18, !11, !14, !29, !30, !31, !32, !33}
!35 = !{!"branch_weights", i32 2000, i32 1}
