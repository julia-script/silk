const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");
pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);

    const condition_index = scope.getInstructionIndex(hir_inst.if_expr.cond);
    const condition_inst = ctx.getInstruction(condition_index);

    if (condition_inst.typed_value.isComptimeKnown()) {
        const condition_result = scope.builder.getBooleanValueKeyAsBool(condition_inst.typed_value.value);
        ctx.markDead(condition_index);
        if (condition_result) {
            return try Index.gen(ctx, scope, hir_inst.if_expr.then_body);
        } else {
            if (hir_inst.if_expr.else_body) |else_body| {
                return try Index.gen(ctx, scope, else_body);
            }
            return ctx.pushWithoutExec(hir_inst_index, .{
                .op = .block,
                .typed_value = Sema.TypedValue.VOID,
                .data = .void,
                .liveness = 0,
            });
        }
    }

    const index = ctx.pushWithoutExec(hir_inst_index, .{
        .op = .@"if",
        .typed_value = Sema.TypedValue.VOID,
        .data = .void,
    });
    const current_block_instructions = scope.active_block_instructions;

    scope.active_block_instructions = null;

    const then_block_index = try Index.gen(ctx, scope, hir_inst.if_expr.then_body);
    const else_block_index = if (hir_inst.if_expr.else_body) |else_body| try Index.gen(ctx, scope, else_body) else null;

    ctx.setInstruction(index, .{
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
    scope.active_block_instructions = current_block_instructions;
    return index;
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const condition_value = ctx.getTypedValue(inst.data.@"if".condition);
    if (!ctx.is_comptime) return;
    const condition_result = ctx.builder.getBooleanValueKeyAsBool(condition_value.value);

    if (condition_result) {
        ctx.execInstruction(inst.data.@"if".then_block);
    } else {
        if (inst.data.@"if".else_block) |else_block_inst_index| {
            ctx.execInstruction(else_block_inst_index);
        }
    }
}
