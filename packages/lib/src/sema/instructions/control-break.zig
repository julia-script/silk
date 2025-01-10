const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const payload_index: ?Sema.Instruction.Index = if (hir_inst.br.operand) |operand| scope.getInstructionIndex(operand) else null;

    if (ctx.is_comptime) {
        const target_hir_inst = scope.entity.getHirInstruction(hir_inst.br.target);
        if (std.meta.activeTag(target_hir_inst) == .loop) {
            ctx.depth += 1;
            if (ctx.depth > 10) {
                @panic("depth limit reached");
            }
            const body_block_index = try Index.gen(ctx, scope, hir_inst.br.target);
            return body_block_index;
        }
    }

    const target_index = scope.getInstructionIndex(hir_inst.br.target);
    const index = ctx.pushInstruction(hir_inst_index, .{
        .op = .br,
        .typed_value = Sema.TypedValue.VOID,
        .data = .{
            .br = .{
                .target = target_index,
                .payload = payload_index,
            },
        },
    });
    if (ctx.is_comptime) {
        ctx.active_node = target_index;
    }
    try maybeInline(ctx, index);
    return index;
}
fn maybeInline(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    if (inst.data.br.payload) |payload| {
        const operand_inst_value = ctx.getTypedValue(payload);
        ctx.setValue(inst.data.br.target, operand_inst_value);
        std.debug.print("inline {d} {}\n", .{ payload, ctx.builder.getFormattableTypedValue(operand_inst_value) });
    }
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    if (inst.data.br.payload) |payload| {
        const operand_inst_value = ctx.getTypedValue(payload);
        ctx.setValue(inst.data.br.target, operand_inst_value);
    }

    ctx.active_node = inst.data.br.target;
    const target_inst = ctx.getInstruction(inst.data.br.target);
    _ = target_inst; // autofix
    // switch (target_inst.op) {
    //     // .loop => ctx.execInstruction(inst.data.br.target),
    //     else => unreachable,
    // }
    // if (ctx.is_comptime) {
    // }
}
