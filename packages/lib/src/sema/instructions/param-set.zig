const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    const lhs_index = block.ctx.getInstructionByHirIndex(hir_inst.param_set.lhs);
    const rhs_index = block.ctx.getInstructionByHirIndex(hir_inst.param_set.rhs);
    return block.appendInstruction(hir_inst_index, .{
        .op = .param_set,
        .typed_value = Sema.TypedValue.VOID,
        .data = .{ .operand_payload = .{
            .operand = lhs_index,
            .payload = rhs_index,
        } },
    });
}
const ExecContext = @import("./ExecContext.zig");
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const lhs_inst_index = inst.data.operand_payload.operand;
    const rhs_inst_index = inst.data.operand_payload.payload;
    const rhs_inst = ctx.getInstruction(rhs_inst_index);
    try ctx.setValue(lhs_inst_index, rhs_inst.typed_value);
}
