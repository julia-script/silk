const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");

pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    const payload_index: ?Sema.Instruction.Index = if (hir_inst.br.operand) |operand| block.ctx.getInstructionByHirIndex(operand) else null;

    var payload_value = Sema.TypedValue.VOID;
    if (payload_index) |payload| {
        payload_value = block.ctx.getTypedValue(payload);
    }
    if (block.is_comptime) {
        // const target_hir_inst = block.ctx.getHirInstruction(hir_inst.br.target);
        // if (std.meta.activeTag(target_hir_inst) == .loop) {
        //     if (block.ctx.depth > 10) {
        //         @panic("depth limit reached");
        //     }
        //     var child_block = try block.makeChild(hir_inst.br.target);
        //     return try child_block.emit();
        // }
        block.ctx.depth += 1;
        if (block.ctx.depth > 10) {
            @panic("depth limit reached");
        }
        // const index = try block.emit();
        const target_index = block.ctx.getInstructionByHirIndex(hir_inst.br.target);

        const index = try block.appendInstruction(hir_inst_index, .{
            .op = .br,
            .typed_value = Sema.TypedValue.VOID,
            .data = .{
                .br = .{
                    .target = target_index,
                    .payload = payload_index,
                },
            },
        });
        block.breakTo(hir_inst.br.target, payload_value);

        return index;
    }

    const target_index = block.ctx.getInstructionByHirIndex(hir_inst.br.target);
    const index = try block.appendInstruction(hir_inst_index, .{
        .op = .br,
        .typed_value = Sema.TypedValue.VOID,
        .data = .{
            .br = .{
                .target = target_index,
                .payload = payload_index,
            },
        },
    });
    // const inst = block.ctx.getInstruction(index);
    block.breakTo(hir_inst.br.target, payload_value);

    return index;
}

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
const ExecContext = @import("./ExecContext.zig");
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    if (inst.data.br.payload) |payload| {
        const operand_inst_value = ctx.getInstruction(payload).typed_value;
        try ctx.setValue(inst.data.br.target, operand_inst_value);
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
