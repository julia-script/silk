const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    const type_inst_index = block.ctx.getInstructionByHirIndex(hir_inst.type_init.type);
    const type_inst = block.ctx.getInstruction(type_inst_index);
    block.ctx.markDead(type_inst_index);
    std.debug.print("type_inst: {}\n", .{block.ctx.builder.getFormattableTypedValue(type_inst.typed_value)});
    const type_key = block.ctx.builder.unwrapTypeValue(type_inst.typed_value.value);

    var field_init_list = block.ctx.builder.newList();
    const entity = block.ctx.getEntity();
    // std.debug.print("genTypeInitInstruction: {d} {}\n", .{ hir_inst_index, ctx.builder.getFormattableType(type_key) });
    // ctx.markDead(global_type_inst_index);

    for (entity.getHir().lists.getSlice(hir_inst.type_init.field_init_list)) |field_init_hir_index| {
        const field_init_inst_index = block.ctx.getInstructionByHirIndex(field_init_hir_index);
        // scope.markDead(field_init_inst_index);
        try field_init_list.append(field_init_inst_index);
    }

    const index = try block.appendInstruction(hir_inst_index, .{
        .op = .type_init,
        .typed_value = .{
            .type = type_key,
            .value = Sema.Value.simple(.exec_time),
        },
        .data = .{ .type_init = .{
            .field_init_list = try field_init_list.commit(),
            .type = type_key,
        } },
    });
    return index;
}
const ExecContext = @import("./ExecContext.zig");
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    _ = ctx; // autofix
    _ = inst_index; // autofix
}
