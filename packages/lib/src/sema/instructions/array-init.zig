const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);

    const type_inst_index = scope.getInstructionIndex(hir_inst.array_init.type);
    const type_inst = ctx.getInstruction(type_inst_index);
    const type_key = scope.builder.unwrapTypeValue(type_inst.typed_value.value);
    var items_list = scope.builder.newList();

    for (scope.entity.getHir().lists.getSlice(hir_inst.array_init.items_list)) |item_hir_index| {
        const item_inst_index = scope.getInstructionIndex(item_hir_index);
        try items_list.append(item_inst_index);
    }

    ctx.markDead(type_inst_index);
    return ctx.pushInstruction(hir_inst_index, .{
        .op = .array_init,
        .typed_value = .{
            .type = type_key,
            .value = Sema.Value.simple(.exec_time),
        },
        .data = .{
            .array_init = .{
                .items_list = try items_list.commit(),
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
