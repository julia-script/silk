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
        //     const index = ctx.pushWithoutExec(hir_inst_index, .{
        //         .op = .block,
        //         .typed_value = Sema.TypedValue.VOID,
        //         .data = .void,
        //     });
        //     const body_block_index = try Index.gen(ctx, scope, hir_inst.loop.body);
        //     ctx.markDead(body_block_index);
        //     const body_block_inst = ctx.getInstruction(body_block_index);
        //     ctx.setInstruction(index, .{
        //         .op = .block,
        //         .typed_value = body_block_inst.typed_value,
        //         .data = body_block_inst.data,
        //     });
        //     return index;
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
    if (ctx.is_comptime) {
        const inst = ctx.getInstruction(inst_index);
        ctx.goTo(inst.data.loop.body_block);
    }
}
