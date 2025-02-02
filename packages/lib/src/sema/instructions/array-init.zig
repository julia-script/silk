const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);

    const type_inst_index = block.ctx.getInstructionByHirIndex(hir_inst.array_init.type);
    const type_inst = block.ctx.getInstruction(type_inst_index);
    const type_key = block.ctx.builder.unwrapTypeValue(type_inst.typed_value.value);
    const entity = block.ctx.getEntity();

    const items_List_range = blk: {
        if (hir_inst.array_init.items_list.len == 0) {
            break :blk Sema.Lists.Range.empty;
        }
        var items_list = block.ctx.builder.newList();
        for (entity.getHir().lists.getSlice(hir_inst.array_init.items_list)) |item_hir_index| {
            std.debug.print("item_hir_index: {d}\n", .{item_hir_index});
            const item_inst_index = block.ctx.getInstructionByHirIndex(item_hir_index);
            try items_list.append(item_inst_index);
        }
        break :blk try items_list.commit();
    };

    block.ctx.markDead(type_inst_index);
    return try block.appendInstruction(hir_inst_index, .{
        .op = .array_init,
        .typed_value = .{
            .type = type_key,
            .value = Sema.Value.simple(.exec_time),
        },
        .data = .{
            .array_init = .{
                .items_list = items_List_range,
                .type = type_key,
                // .type = type_key,
            },
        },
    });
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    _ = ctx; // autofix
    _ = inst_index; // autofix
}
