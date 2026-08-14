; Frozen spike-local LLVM 22.1.8 switched-resume construction.
; This module is intentionally not imported by production compiler or LLVM code.
source_filename = "effect-suspension-native-lowering-spike/switched.ll"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
target triple = "arm64-apple-macosx15.0.0"

@variant_name = private unnamed_addr constant [21 x i8] c"llvm-switched-resume\00"

declare token @llvm.coro.id(i32, ptr, ptr, ptr)
declare i1 @llvm.coro.alloc(token)
declare i64 @llvm.coro.size.i64()
declare i64 @llvm.coro.align.i64()
declare ptr @llvm.coro.begin(token, ptr)
declare i8 @llvm.coro.suspend(token, i1)
declare ptr @llvm.coro.free(token, ptr)
declare void @llvm.coro.end(ptr, i1, token)
declare void @llvm.coro.resume(ptr)
declare void @llvm.coro.destroy(ptr)

declare ptr @spike_allocate(ptr, i64, i64, i32)
declare void @spike_reclaim(ptr, ptr, i32)
declare void @spike_source_cleanup(ptr, i32)

define internal ptr @open_one(ptr %context, i32 %owner, i32 %scalar) presplitcoroutine !dbg !10 {
entry:
  %context.slot = alloca ptr, align 8
  %owner.slot = alloca i32, align 4
  %scalar.slot = alloca i32, align 4
  store ptr %context, ptr %context.slot, align 8
  store i32 %owner, ptr %owner.slot, align 4
  store i32 %scalar, ptr %scalar.slot, align 4
  %id = call token @llvm.coro.id(i32 0, ptr null, ptr null, ptr null), !dbg !20
  %needs.allocation = call i1 @llvm.coro.alloc(token %id), !dbg !20
  br i1 %needs.allocation, label %allocate, label %begin

allocate:
  %size = call i64 @llvm.coro.size.i64(), !dbg !20
  %alignment = call i64 @llvm.coro.align.i64(), !dbg !20
  %allocation = call ptr @spike_allocate(ptr %context, i64 %size, i64 %alignment, i32 1), !dbg !20
  %accepted = icmp ne ptr %allocation, null
  br i1 %accepted, label %begin, label %refused

begin:
  %storage = phi ptr [ null, %entry ], [ %allocation, %allocate ]
  %handle = call noalias ptr @llvm.coro.begin(token %id, ptr %storage), !dbg !20
  %suspend.result = call i8 @llvm.coro.suspend(token none, i1 false), !dbg !20
  switch i8 %suspend.result, label %suspend [i8 0, label %resume
                                            i8 1, label %destroy]

resume:
  %saved.context = load ptr, ptr %context.slot, align 8
  %saved.owner = load i32, ptr %owner.slot, align 4
  %saved.scalar = load volatile i32, ptr %scalar.slot, align 4
  call void @spike_source_cleanup(ptr %saved.context, i32 %saved.owner), !dbg !20
  br label %cleanup

destroy:
  br label %cleanup, !dbg !20

cleanup:
  %cleanup.context = load ptr, ptr %context.slot, align 8
  %allocation.to.free = call ptr @llvm.coro.free(token %id, ptr %handle), !dbg !20
  %was.allocated = icmp ne ptr %allocation.to.free, null
  br i1 %was.allocated, label %reclaim, label %finish

reclaim:
  call void @spike_reclaim(ptr %cleanup.context, ptr %allocation.to.free, i32 1), !dbg !20
  br label %finish

finish:
  call void @llvm.coro.end(ptr %handle, i1 false, token none), !dbg !20
  br label %suspend

suspend:
  %return.handle = phi ptr [ %handle, %begin ], [ %handle, %finish ]
  ret ptr %return.handle

refused:
  ret ptr null
}

define internal ptr @open_two(ptr %context, i32 %owner, i32 %scalar) presplitcoroutine !dbg !11 {
entry:
  %context.slot = alloca ptr, align 8
  %owner.slot = alloca i32, align 4
  %scalar.slot = alloca i32, align 4
  store ptr %context, ptr %context.slot, align 8
  store i32 %owner, ptr %owner.slot, align 4
  store i32 %scalar, ptr %scalar.slot, align 4
  %id = call token @llvm.coro.id(i32 0, ptr null, ptr null, ptr null), !dbg !21
  %needs.allocation = call i1 @llvm.coro.alloc(token %id), !dbg !21
  br i1 %needs.allocation, label %allocate, label %begin

allocate:
  %size = call i64 @llvm.coro.size.i64(), !dbg !21
  %alignment = call i64 @llvm.coro.align.i64(), !dbg !21
  %allocation = call ptr @spike_allocate(ptr %context, i64 %size, i64 %alignment, i32 2), !dbg !21
  %accepted = icmp ne ptr %allocation, null
  br i1 %accepted, label %begin, label %refused

begin:
  %storage = phi ptr [ null, %entry ], [ %allocation, %allocate ]
  %handle = call noalias ptr @llvm.coro.begin(token %id, ptr %storage), !dbg !21
  %suspend.result = call i8 @llvm.coro.suspend(token none, i1 false), !dbg !21
  switch i8 %suspend.result, label %suspend [i8 0, label %resume
                                            i8 1, label %destroy]

resume:
  %saved.context = load ptr, ptr %context.slot, align 8
  %saved.owner = load i32, ptr %owner.slot, align 4
  %saved.scalar = load volatile i32, ptr %scalar.slot, align 4
  call void @spike_source_cleanup(ptr %saved.context, i32 %saved.owner), !dbg !21
  br label %cleanup

destroy:
  br label %cleanup, !dbg !21

cleanup:
  %cleanup.context = load ptr, ptr %context.slot, align 8
  %allocation.to.free = call ptr @llvm.coro.free(token %id, ptr %handle), !dbg !21
  %was.allocated = icmp ne ptr %allocation.to.free, null
  br i1 %was.allocated, label %reclaim, label %finish

reclaim:
  call void @spike_reclaim(ptr %cleanup.context, ptr %allocation.to.free, i32 2), !dbg !21
  br label %finish

finish:
  call void @llvm.coro.end(ptr %handle, i1 false, token none), !dbg !21
  br label %suspend

suspend:
  %return.handle = phi ptr [ %handle, %begin ], [ %handle, %finish ]
  ret ptr %return.handle

refused:
  ret ptr null
}

define ptr @spike_variant_open(ptr %context, i32 %boundary, i32 %owner, i32 %scalar) !dbg !12 {
entry:
  switch i32 %boundary, label %invalid [i32 1, label %one
                                       i32 2, label %two]
one:
  %first = call ptr @open_one(ptr %context, i32 %owner, i32 %scalar), !dbg !24
  ret ptr %first
two:
  %second = call ptr @open_two(ptr %context, i32 %owner, i32 %scalar), !dbg !25
  ret ptr %second
invalid:
  ret ptr null
}

define void @spike_variant_resume(ptr %continuation) !dbg !13 {
entry:
  call void @llvm.coro.resume(ptr %continuation), !dbg !22
  ret void
}

define void @spike_variant_teardown(ptr %continuation) !dbg !14 {
entry:
  call void @llvm.coro.destroy(ptr %continuation), !dbg !23
  ret void
}

define ptr @spike_variant_name() {
entry:
  ret ptr @variant_name
}

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}

!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "Silk native suspension lowering spike", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "boundaries.silk", directory: "effect-suspension-native-lowering-spike")
!2 = !{i32 7, !"Dwarf Version", i32 5}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = !DISubroutineType(types: !5)
!5 = !{}
!10 = distinct !DISubprogram(name: "suspend.one.ramp", linkageName: "open_one", scope: !1, file: !1, line: 1, type: !4, scopeLine: 1, spFlags: DISPFlagDefinition, unit: !0)
!11 = distinct !DISubprogram(name: "suspend.two.ramp", linkageName: "open_two", scope: !1, file: !1, line: 2, type: !4, scopeLine: 2, spFlags: DISPFlagDefinition, unit: !0)
!12 = distinct !DISubprogram(name: "suspension.ramp.dispatch", linkageName: "spike_variant_open", scope: !1, file: !1, line: 1, type: !4, scopeLine: 1, spFlags: DISPFlagDefinition, unit: !0)
!13 = distinct !DISubprogram(name: "suspension.resume.dispatch", linkageName: "spike_variant_resume", scope: !1, file: !1, line: 1, type: !4, scopeLine: 1, spFlags: DISPFlagDefinition, unit: !0)
!14 = distinct !DISubprogram(name: "suspension.destroy.dispatch", linkageName: "spike_variant_teardown", scope: !1, file: !1, line: 2, type: !4, scopeLine: 2, spFlags: DISPFlagDefinition, unit: !0)
!20 = !DILocation(line: 1, column: 1, scope: !10)
!21 = !DILocation(line: 2, column: 1, scope: !11)
!22 = !DILocation(line: 1, column: 1, scope: !13)
!23 = !DILocation(line: 2, column: 1, scope: !14)
!24 = !DILocation(line: 1, column: 1, scope: !12)
!25 = !DILocation(line: 2, column: 1, scope: !12)
