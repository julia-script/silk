const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");
pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);

    const condition_index = block.ctx.getInstructionByHirIndex(hir_inst.if_expr.cond);
    const condition_inst = block.ctx.getInstruction(condition_index);

    if (condition_inst.typed_value.isComptimeKnown()) {
        const condition_result = block.ctx.builder.getBooleanValueKeyAsBool(condition_inst.typed_value.value);
        block.ctx.markDead(condition_index);
        if (condition_result) {
            return try block.emitInstruction(hir_inst.if_expr.then_body);
        } else {
            if (hir_inst.if_expr.else_body) |else_body| {
                return try block.emitInstruction(else_body);
            }
            return block.appendInstruction(hir_inst_index, .{
                .op = .block,
                .typed_value = Sema.TypedValue.VOID,
                .data = .void,
                .liveness = 0,
            });
        }
    }

    const index = try block.appendInstruction(hir_inst_index, .{
        .op = .@"if",
        .typed_value = Sema.TypedValue.VOID,
        .data = .void,
    });
    // const current_block_instructions = block.active_block_instructions;

    var if_block = try block.makeChild(hir_inst_index);

    // block.active_block_instructions = null;

    const then_block_index = try if_block.emitInstruction(hir_inst.if_expr.then_body);
    const else_block_index = if (hir_inst.if_expr.else_body) |else_body| try if_block.emitInstruction(else_body) else null;

    block.ctx.setInstruction(index, .{
        .op = .@"if",
        .typed_value = Sema.TypedValue.VOID,
        .data = .{ .@"if" = .{
            .condition = condition_index,
            .then_block = then_block_index,
            .else_block = else_block_index,
            .finally_block = null,
        } },
        // .liveness = if (condition_result == null) 1 else 0,
    });
    // block.active_block_instructions = current_block_instructions;
    return index;
}
const ExecContext = @import("./ExecContext.zig");
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const condition_value = ctx.getInstruction(inst.data.@"if".condition).typed_value;
    const condition_result = ctx.builder.getBooleanValueKeyAsBool(condition_value.value);

    if (condition_result) {
        try ctx.execInstruction(inst.data.@"if".then_block);
    } else {
        if (inst.data.@"if".else_block) |else_block_inst_index| {
            try ctx.execInstruction(else_block_inst_index);
        }
    }
}
