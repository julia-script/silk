const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    _ = hir_inst; // autofix
    _ = ctx; // autofix
    @panic("not implemented");
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const typed_value = ctx.getTypedValue(inst_index);
    ctx.setValue(inst_index, typed_value);
}
