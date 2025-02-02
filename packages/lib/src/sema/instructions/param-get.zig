const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    const operand_inst_index = block.ctx.getInstructionByHirIndex(hir_inst.param_get.operand);
    const operand_inst = block.ctx.getInstruction(operand_inst_index);

    return block.appendInstruction(hir_inst_index, .{
        .op = .param_get,
        .typed_value = operand_inst.typed_value,
        .data = .{ .operand = operand_inst_index },
    });
}
const ExecContext = @import("./ExecContext.zig");
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const typed_value = ctx.getInstruction(inst.data.operand).typed_value;
    try ctx.setValue(inst_index, typed_value);
}
