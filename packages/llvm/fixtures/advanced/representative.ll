source_filename = "advanced.ll"
target datalayout = "e-p:64:64-i32:32-i64:64-n32:64"
target triple = "aarch64-unknown-linux"

define void @advanced(ptr %v0, i32 %v1, <4 x i32> %v2) {
entry:
  %slot = alloca { i32, <4 x i32> }, i32 1, align 4
  %field = getelementptr inbounds { i32, <4 x i32> }, ptr %slot, i32 0, i32 0
  store volatile i32 %v1, ptr %field, align 4
  %loaded = load i32, ptr %field, align 4
  %first = extractelement <4 x i32> %v2, i32 0
  %inserted = insertelement <4 x i32> zeroinitializer, i32 %v1, i32 1
  %shuffled = shufflevector <4 x i32> %inserted, <4 x i32> zeroinitializer, <4 x i32> zeroinitializer
  %old = atomicrmw add ptr %v0, i32 1 monotonic, align 4
  %pair_result = cmpxchg weak ptr %v0, i32 %v1, i32 1 acq_rel acquire, align 4
  %atomic_loaded = load atomic i32, ptr %v0 syncscope("singlethread") acquire, align 4
  store atomic i32 %v1, ptr %v0 release, align 4
  fence seq_cst
  ret void
}
define void @scalable_vectors(i32 %v0, <vscale x 4 x i32> %v1) {
entry:
  %v2 = insertelement <vscale x 1 x i32> poison, i32 %v0, i32 0
  %splat = shufflevector <vscale x 1 x i32> %v2, <vscale x 1 x i32> poison, <vscale x 4 x i32> splat (i32 0)
  %poison_shuffle = shufflevector <vscale x 4 x i32> %v1, <vscale x 4 x i32> %v1, <vscale x 4 x i32> poison
  ret void
}
define void @atomic_inventory(ptr %v0, i32 %v1, float %v2) {
entry:
  %xchg = atomicrmw xchg ptr %v0, i32 %v1 monotonic
  %add = atomicrmw add ptr %v0, i32 %v1 monotonic
  %sub = atomicrmw sub ptr %v0, i32 %v1 monotonic
  %and = atomicrmw and ptr %v0, i32 %v1 monotonic
  %nand = atomicrmw nand ptr %v0, i32 %v1 monotonic
  %or = atomicrmw or ptr %v0, i32 %v1 monotonic
  %xor = atomicrmw xor ptr %v0, i32 %v1 monotonic
  %max = atomicrmw max ptr %v0, i32 %v1 monotonic
  %min = atomicrmw min ptr %v0, i32 %v1 monotonic
  %umax = atomicrmw umax ptr %v0, i32 %v1 monotonic
  %umin = atomicrmw umin ptr %v0, i32 %v1 monotonic
  %fadd = atomicrmw volatile fadd ptr %v0, float %v2 syncscope("singlethread") seq_cst
  %fsub = atomicrmw volatile fsub ptr %v0, float %v2 syncscope("singlethread") seq_cst
  %fmax = atomicrmw volatile fmax ptr %v0, float %v2 syncscope("singlethread") seq_cst
  %fmin = atomicrmw volatile fmin ptr %v0, float %v2 syncscope("singlethread") seq_cst
  ret void
}
define void @intrinsic_user(ptr %v0, ptr %v1, i64 %v2, i8 %v3) {
entry:
  %inalloca_slot = alloca inalloca i8, i32 1, align 4, addrspace(1)
  call void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly align 4 %v0, ptr noalias readonly nocapture align 4 %v1, i64 %v2, i1 immarg 0) nofree nounwind nocallback willreturn memory(argmem: readwrite)
  call void @llvm.memmove.p0.p0.i64(ptr nocapture writeonly align 4 %v0, ptr readonly nocapture align 4 %v1, i64 %v2, i1 immarg 1) nofree nounwind nocallback willreturn memory(argmem: readwrite)
  call void @llvm.memset.p0.i64(ptr nocapture writeonly align 4 %v0, i8 %v3, i64 %v2, i1 immarg 0) nofree nounwind nocallback willreturn memory(argmem: write)
  call void @llvm.assume(i1 noundef 1) nofree nosync nounwind nocallback willreturn [ "cold"() ]
  call void asm sideeffect alignstack "nop", ""()
  ret void
}
declare void @llvm.memcpy.p0.p0.i64(ptr noalias nocapture writeonly, ptr noalias readonly nocapture, i64, i1 immarg) nofree nounwind nocallback willreturn memory(argmem: readwrite)
declare void @llvm.memmove.p0.p0.i64(ptr nocapture writeonly, ptr readonly nocapture, i64, i1 immarg) nofree nounwind nocallback willreturn memory(argmem: readwrite)
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) nofree nounwind nocallback willreturn memory(argmem: write)
declare void @llvm.assume(i1 noundef) nofree nosync nounwind nocallback willreturn
define void @indirect(ptr %v0) {
entry:
  indirectbr ptr %v0, [label %target]
target:
  ret void
}
define i32 @read_vararg(ptr %v0, ...) {
entry:
  %next = va_arg ptr %v0, i32
  ret i32 %next
}
