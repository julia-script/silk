const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);

    const block_hir_data = switch (hir_inst) {
        .inline_block, .block => |inst| inst,
        else => unreachable,
    };

    const parent_is_comptime = ctx.is_comptime;
    const parent_working_list = scope.active_block_instructions;
    const is_comptime = ctx.is_comptime or block_hir_data.is_comptime;
    ctx.is_comptime = is_comptime;
    const block_type = blk: {
        const type_hir_inst = block_hir_data.type orelse break :blk null;
        const type_entity = scope.builder.getEntityByHirInst(type_hir_inst);
        break :blk try type_entity.resolveType();
        // scope.builder.getEntityByHirInst(type_inst_index) else null;
    };
    const index = ctx.pushWithoutExec(hir_inst_index, .{
        .op = .block,
        .typed_value = .{
            .type = block_type orelse Sema.Type.simple(.void),
            .value = Sema.Value.simple(.void),
        },
        .data = .{ .block = .{
            .instructions_list = Sema.Instruction.List.empty,
            .is_comptime = is_comptime,
        } },
    });
    const hir_inst_list = scope.entity.getHir().lists.getSlice(block_hir_data.instructions_list);
    var instructions_list = scope.builder.newList();
    const parent_active_node = ctx.active_node;
    scope.active_block_instructions = &instructions_list;
    ctx.active_node = index;

    for (hir_inst_list) |child_hir_inst_index| {
        _ = try Index.gen(ctx, scope, child_hir_inst_index);
        if (index != ctx.active_node) {
            break;
        }
    }
    // @panic("not implemented");

    ctx.setData(index, .{
        .block = .{
            .instructions_list = try instructions_list.commit(),
            .is_comptime = is_comptime,
        },
    });

    ctx.is_comptime = parent_is_comptime;
    scope.active_block_instructions = parent_working_list;
    if (ctx.active_node == index) {
        ctx.active_node = parent_active_node;
    }
    return index;
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    if (!ctx.is_comptime) return;
    const instructions_list = ctx.builder.sema.lists.getSlice(inst.data.block.instructions_list);
    const parent_active_node = ctx.active_node;
    ctx.active_node = inst_index;
    // std.debug.print("executing block {d} with {d} instructions\n", .{ inst_index, instructions_list.len });
    for (instructions_list) |child_inst_index| {
        // const child_inst = ctx.getInstruction(child_inst_index);
        // try Index.Block.exec(ctx, child_inst_index);
        // std.debug.print("executing '{s}'\n", .{@tagName(child_inst.op)});
        try Index.exec(ctx, child_inst_index);
        if (ctx.active_node != inst_index) {
            return;
        }
    }
    // @panic("not implemented");
    ctx.active_node = parent_active_node;
}
