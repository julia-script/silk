; Frozen spike-local LLVM 22.1.8 returned-continuation construction.
; This module is intentionally not imported by production compiler or LLVM code.
source_filename = "effect-suspension-native-lowering-spike/retcon.ll"
target datalayout = "e-m:o-i64:64-i128:128-n32:64-S128"
; LLVM's Darwin driver canonicalizes the repository's aarch64-apple-darwin request to this
; deployment-qualified triple on the pinned host. Matching it keeps the -Werror verifier honest.
target triple = "arm64-apple-macosx15.0.0"

%retcon.header = type { ptr }

@variant_name = private unnamed_addr constant [27 x i8] c"llvm-returned-continuation\00"
@unexpected_allocate = private unnamed_addr constant [47 x i8] c"retcon unexpectedly exceeded its inline buffer\00"
@unexpected_deallocate = private unnamed_addr constant [43 x i8] c"retcon unexpectedly requested deallocation\00"

declare token @llvm.coro.id.retcon(i32, i32, ptr, ptr, ptr, ptr)
declare ptr @llvm.coro.prepare.retcon(ptr)
declare ptr @llvm.coro.begin(token, ptr)
declare i1 @llvm.coro.suspend.retcon.i1(...)
declare void @llvm.coro.end(ptr, i1, token)

declare ptr @spike_allocate(ptr, i64, i64, i32)
declare void @spike_reclaim(ptr, ptr, i32)
declare void @spike_source_cleanup(ptr, i32)
declare void @spike_abort(ptr)

declare ptr @retcon_prototype(ptr, i1 zeroext)

define internal noalias ptr @retcon_fallback_allocate(i32 %size) {
entry:
  call void @spike_abort(ptr @unexpected_allocate)
  unreachable
}

define internal void @retcon_fallback_deallocate(ptr %allocation) {
entry:
  call void @spike_abort(ptr @unexpected_deallocate)
  unreachable
}

define internal ptr @open_one(ptr %buffer, ptr %context, ptr %allocation, i32 %owner, i32 %scalar) noinline presplitcoroutine !dbg !10 {
entry:
  %context.slot = alloca ptr, align 8
  %allocation.slot = alloca ptr, align 8
  %owner.slot = alloca i32, align 4
  %scalar.slot = alloca i32, align 4
  store ptr %context, ptr %context.slot, align 8
  store ptr %allocation, ptr %allocation.slot, align 8
  store i32 %owner, ptr %owner.slot, align 4
  store i32 %scalar, ptr %scalar.slot, align 4
  %id = call token @llvm.coro.id.retcon(i32 56, i32 8, ptr %buffer, ptr @retcon_prototype, ptr @retcon_fallback_allocate, ptr @retcon_fallback_deallocate), !dbg !20
  %handle = call ptr @llvm.coro.begin(token %id, ptr null), !dbg !20
  %destroying = call i1 (...) @llvm.coro.suspend.retcon.i1(), !dbg !20
  br i1 %destroying, label %destroy, label %resume

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
  %cleanup.allocation = load ptr, ptr %allocation.slot, align 8
  call void @spike_reclaim(ptr %cleanup.context, ptr %cleanup.allocation, i32 1), !dbg !20
  call void @llvm.coro.end(ptr %handle, i1 false, token none), !dbg !20
  unreachable
}

define internal ptr @open_two(ptr %buffer, ptr %context, ptr %allocation, i32 %owner, i32 %scalar) noinline presplitcoroutine !dbg !11 {
entry:
  %context.slot = alloca ptr, align 8
  %allocation.slot = alloca ptr, align 8
  %owner.slot = alloca i32, align 4
  %scalar.slot = alloca i32, align 4
  store ptr %context, ptr %context.slot, align 8
  store ptr %allocation, ptr %allocation.slot, align 8
  store i32 %owner, ptr %owner.slot, align 4
  store i32 %scalar, ptr %scalar.slot, align 4
  %id = call token @llvm.coro.id.retcon(i32 56, i32 8, ptr %buffer, ptr @retcon_prototype, ptr @retcon_fallback_allocate, ptr @retcon_fallback_deallocate), !dbg !21
  %handle = call ptr @llvm.coro.begin(token %id, ptr null), !dbg !21
  %destroying = call i1 (...) @llvm.coro.suspend.retcon.i1(), !dbg !21
  br i1 %destroying, label %destroy, label %resume

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
  %cleanup.allocation = load ptr, ptr %allocation.slot, align 8
  call void @spike_reclaim(ptr %cleanup.context, ptr %cleanup.allocation, i32 2), !dbg !21
  call void @llvm.coro.end(ptr %handle, i1 false, token none), !dbg !21
  unreachable
}

define ptr @spike_variant_open(ptr %context, i32 %boundary, i32 %owner, i32 %scalar) !dbg !12 {
entry:
  %allocation = call ptr @spike_allocate(ptr %context, i64 64, i64 8, i32 %boundary), !dbg !24
  %accepted = icmp ne ptr %allocation, null
  br i1 %accepted, label %open, label %refused

open:
  %buffer = getelementptr inbounds i8, ptr %allocation, i64 8
  switch i32 %boundary, label %invalid [i32 1, label %one
                                       i32 2, label %two]

one:
  %prepared.first = call ptr @llvm.coro.prepare.retcon(ptr @open_one)
  %first = call ptr %prepared.first(ptr %buffer, ptr %context, ptr %allocation, i32 %owner, i32 %scalar), !dbg !24
  store ptr %first, ptr %allocation, align 8
  ret ptr %allocation

two:
  %prepared.second = call ptr @llvm.coro.prepare.retcon(ptr @open_two)
  %second = call ptr %prepared.second(ptr %buffer, ptr %context, ptr %allocation, i32 %owner, i32 %scalar), !dbg !25
  store ptr %second, ptr %allocation, align 8
  ret ptr %allocation

invalid:
  call void @spike_abort(ptr @unexpected_allocate)
  unreachable

refused:
  ret ptr null
}

define void @spike_variant_resume(ptr %allocation) !dbg !13 {
entry:
  %continuation = load ptr, ptr %allocation, align 8
  %buffer = getelementptr inbounds i8, ptr %allocation, i64 8
  %next = call ptr %continuation(ptr %buffer, i1 zeroext false), !dbg !22
  ret void
}

define void @spike_variant_teardown(ptr %allocation) !dbg !14 {
entry:
  %continuation = load ptr, ptr %allocation, align 8
  %buffer = getelementptr inbounds i8, ptr %allocation, i64 8
  %next = call ptr %continuation(ptr %buffer, i1 zeroext true), !dbg !23
  ret void
}

define ptr @spike_variant_name() {
entry:
  ret ptr @variant_name
}

!llvm.dbg.cu = !{!0}
!llvm.module.flags = !{!2, !3}

!0 = distinct !DICompileUnit(language: DW_LANG_C, file: !1, producer: "Silk native suspension retcon spike", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug)
!1 = !DIFile(filename: "boundaries.silk", directory: "effect-suspension-native-lowering-spike")
!2 = !{i32 7, !"Dwarf Version", i32 5}
!3 = !{i32 2, !"Debug Info Version", i32 3}
!4 = !DISubroutineType(types: !5)
!5 = !{}
!10 = distinct !DISubprogram(name: "suspend.one.ramp.retcon", linkageName: "open_one", scope: !1, file: !1, line: 1, type: !4, scopeLine: 1, spFlags: DISPFlagDefinition, unit: !0)
!11 = distinct !DISubprogram(name: "suspend.two.ramp.retcon", linkageName: "open_two", scope: !1, file: !1, line: 2, type: !4, scopeLine: 2, spFlags: DISPFlagDefinition, unit: !0)
!12 = distinct !DISubprogram(name: "suspension.ramp.dispatch.retcon", linkageName: "spike_variant_open", scope: !1, file: !1, line: 1, type: !4, scopeLine: 1, spFlags: DISPFlagDefinition, unit: !0)
!13 = distinct !DISubprogram(name: "suspension.resume.dispatch.retcon", linkageName: "spike_variant_resume", scope: !1, file: !1, line: 1, type: !4, scopeLine: 1, spFlags: DISPFlagDefinition, unit: !0)
!14 = distinct !DISubprogram(name: "suspension.destroy.dispatch.retcon", linkageName: "spike_variant_teardown", scope: !1, file: !1, line: 2, type: !4, scopeLine: 2, spFlags: DISPFlagDefinition, unit: !0)
!20 = !DILocation(line: 1, column: 1, scope: !10)
!21 = !DILocation(line: 2, column: 1, scope: !11)
!22 = !DILocation(line: 1, column: 1, scope: !13)
!23 = !DILocation(line: 2, column: 1, scope: !14)
!24 = !DILocation(line: 1, column: 1, scope: !12)
!25 = !DILocation(line: 2, column: 1, scope: !12)
