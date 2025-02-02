const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");
pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    var loop_block = try block.makeChild(hir_inst_index);
    if (loop_block.is_comptime) {
        const index = try block.appendInstruction(hir_inst_index, .{
            .op = .block,
            .typed_value = Sema.TypedValue.VOID,
            .data = .void,
        });

        _ = try loop_block.emitInstruction(hir_inst.loop.body);

        std.debug.print("{} {}\n", .{ loop_block.done, index });
        while (loop_block.done) {
            loop_block.done = false;
            _ = try loop_block.emitInstruction(hir_inst.loop.body);
        }

        block.ctx.setData(index, .{
            .block = .{
                .instructions_list = try block.ctx.builder.sema.lists.internSlice(loop_block.instruction_list.items),
                .is_comptime = loop_block.is_comptime,
            },
        });
        try loop_block.instruction_list.append(block.ctx.allocator, index);
        return index;
    }

    const index = try block.appendInstruction(hir_inst_index, .{
        .op = .loop,
        .typed_value = Sema.TypedValue.VOID,
        .data = .void,
    });
    // const current_active_block_instructions = block.active_block_instructions;
    // _ = current_active_block_instructions; // autofix
    // block.active_block_instructions = null;

    // const body_block_index = try Index.gen(ctx, scope, hir_inst.loop.body);

    const body_block_index = try loop_block.emitInstruction(hir_inst.loop.body);
    if (loop_block.is_comptime) {
        return body_block_index;
    } else {
        block.ctx.setInstruction(index, .{
            .op = .loop,
            .typed_value = Sema.TypedValue.VOID,
            .data = .{
                .loop = .{
                    .body_block = body_block_index,
                },
            },
        });
    }

    // scope.active_block_instructions = current_active_block_instructions;

    return index;
}

const ExecContext = @import("./ExecContext.zig");
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const parent_active_node = ctx.active_node;
    // ctx.goTo(inst.data.loop.body_block);
    try ctx.execInstruction(inst.data.loop.body_block);
    while (ctx.active_node == inst_index) {
        ctx.active_node = parent_active_node;
        try ctx.execInstruction(inst.data.loop.body_block);
    }
    // ctx.active_block = inst_index;
}
