const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");
const ExecContext = @import("./ExecContext.zig");
pub fn emit(parent_block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const emitted = try emitBlock(parent_block, hir_inst_index);

    return emitted;
}
pub fn emitBlock(parent_block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = parent_block.ctx.getHirInstruction(hir_inst_index);
    const block_hir_data = switch (hir_inst) {
        .inline_block, .block => |inst| inst,
        else => unreachable,
    };

    // const parent_is_comptime = ctx.is_comptime;
    // const parent_working_list = scope.active_block_instructions;
    // const is_comptime = ctx.is_comptime or block_hir_data.is_comptime;
    // ctx.is_comptime = is_comptime;
    const block_type = blk: {
        const type_hir_inst = block_hir_data.type orelse break :blk null;
        const type_entity = parent_block.ctx.builder.getEntityByHirInst(type_hir_inst);
        break :blk try type_entity.resolveType();
        // scope.builder.getEntityByHirInst(type_inst_index) else null;
    };
    var block = try parent_block.makeChild(hir_inst_index);
    const is_comptime = block.is_comptime or block_hir_data.is_comptime;
    block.is_comptime = is_comptime;

    const index = try parent_block.appendInstruction(hir_inst_index, .{
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
    // const index = block.parent(hir_inst_index, .{
    //     .op = .block,
    //     .typed_value = .{
    //         .type = block_type orelse Sema.Type.simple(.void),
    //         .value = Sema.Value.simple(.void),
    //     },
    //     .data = .{ .block = .{
    //         .instructions_list = Sema.Instruction.List.empty,
    //         .is_comptime = is_comptime,
    //     } },
    // });
    const hir_inst_list = block
        .ctx
        .getEntity()
        .getHir()
        .lists
        .getSlice(block_hir_data.instructions_list);

    block.ctx.indent += 1;
    for (hir_inst_list) |child_hir_inst_index| {
        _ = try block.emitInstruction(child_hir_inst_index);
        if (block.done) {
            break;
        }
    }

    for (block.instruction_list.items) |item| {
        std.debug.print("appending {any}\n", .{item});
    }
    block.ctx.indent -= 1;

    block.ctx.setData(index, .{
        .block = .{
            .instructions_list = try block.ctx.builder.sema.lists.internSlice(block.instruction_list.items),
            .is_comptime = is_comptime,
        },
    });

    // return index;
    return index;
}

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
    const index = ctx.pushInstruction(hir_inst_index, .{
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
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const instructions_list = ctx.builder.sema.lists.getSlice(inst.data.block.instructions_list);
    const parent_active_node = ctx.active_node;
    ctx.active_node = inst_index;
    for (instructions_list) |child_inst_index| {
        try ctx.execInstruction(child_inst_index);
        if (ctx.active_node != inst_index) {
            return;
        }
    }
    // @panic("not implemented");
    ctx.active_node = parent_active_node;
}
