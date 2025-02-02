const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    const lhs_inst_index = block.ctx.getInstructionByHirIndex(hir_inst.as.lhs);
    const rhs_inst_index = block.ctx.getInstructionByHirIndex(hir_inst.as.rhs);
    const rhs_inst = block.ctx.getInstruction(rhs_inst_index);

    block.ctx.markDeadIfComptimeKnown(rhs_inst_index);
    block.ctx.markDeadIfComptimeKnown(lhs_inst_index);
    const rhs_type = block.ctx.builder.unwrapTypeValue(rhs_inst.typed_value.value);

    return try block.pushCastInstruction(hir_inst_index, lhs_inst_index, rhs_type);
}
const ExecContext = @import("./ExecContext.zig");
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const value_inst_value = ctx.getInstruction(inst.data.operand).typed_value;
    if (!value_inst_value.isComptimeKnown()) @panic("not comptime known");
    const value = try ctx.builder.convertNumberType(value_inst_value, inst.typed_value.type);
    try ctx.setValue(inst_index, .{
        .type = inst.typed_value.type,
        .value = value.value,
    });
}
