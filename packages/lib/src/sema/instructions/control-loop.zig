const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");
pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    if (ctx.is_comptime) {
        return try Index.gen(ctx, scope, hir_inst.loop.body);
    }

    const index = ctx.pushWithoutExec(hir_inst_index, .{
        .op = .loop,
        .typed_value = Sema.TypedValue.VOID,
        .data = .void,
    });
    const current_active_block_instructions = scope.active_block_instructions;
    scope.active_block_instructions = null;

    const body_block_index = try Index.gen(ctx, scope, hir_inst.loop.body);
    if (ctx.is_comptime) {
        return body_block_index;
    } else {
        ctx.setInstruction(index, .{
            .op = .loop,
            .typed_value = Sema.TypedValue.VOID,
            .data = .{
                .loop = .{
                    .body_block = body_block_index,
                },
            },
        });
    }

    scope.active_block_instructions = current_active_block_instructions;

    return index;
}

pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const parent_active_node = ctx.active_node;
    // ctx.goTo(inst.data.loop.body_block);
    ctx.execInstruction(inst.data.loop.body_block);
    while (ctx.active_node == inst_index) {
        std.debug.print("active_block: {?} loop_inst_index: {}\n", .{ ctx.active_node, inst_index });
        ctx.active_node = parent_active_node;
        ctx.execInstruction(inst.data.loop.body_block);
    }
    // ctx.active_block = inst_index;
}
