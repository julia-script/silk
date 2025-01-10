const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    _ = ctx; // autofix
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const operand_inst_index = scope.getInstructionIndex(hir_inst.param_get.operand);
    const operand_inst = scope.getInstruction(operand_inst_index);

    return scope.pushInstruction(hir_inst_index, .{
        .op = .param_get,
        .typed_value = operand_inst.typed_value,
        .data = .{ .operand = operand_inst_index },
    });
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const typed_value = ctx.getTypedValue(inst.data.operand);
    ctx.setValue(inst_index, typed_value);
}
