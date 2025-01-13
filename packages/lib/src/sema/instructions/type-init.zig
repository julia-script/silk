const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const type_inst_index = scope.getInstructionIndex(hir_inst.type_init.type);
    const type_inst = ctx.getInstruction(type_inst_index);
    const type_key = scope.builder.unwrapTypeValue(type_inst.typed_value.value);
    var field_init_list = scope.builder.newList();
    // std.debug.print("genTypeInitInstruction: {d} {}\n", .{ hir_inst_index, ctx.builder.getFormattableType(type_key) });
    // ctx.markDead(global_type_inst_index);

    for (scope.entity.getHir().lists.getSlice(hir_inst.type_init.field_init_list)) |field_init_hir_index| {
        const field_init_inst_index = scope.getInstructionIndex(field_init_hir_index);
        // scope.markDead(field_init_inst_index);
        try field_init_list.append(field_init_inst_index);
    }

    const index = ctx.pushInstruction(hir_inst_index, .{
        .op = .type_init,
        .typed_value = .{
            .type = type_key,
            .value = Sema.Value.simple(.exec_time),
        },
        // .data = .void,
        .data = .{ .type_init = .{
            .field_init_list = try field_init_list.commit(),
            .type = type_key,
        } },
    });
    // const struct_type = ctx.builder.getType(type_key) orelse std.debug.panic("type_to_store is not a type", .{});
    // const module = ctx.builder.getEntity(struct_type.data.@"struct".entity);

    // for (scope.entity.getHir().lists.getSlice(hir_inst.type_init.field_init_list)) |field_init_hir_index| {
    //     const field_init_inst_index = scope.getInstructionIndex(field_init_hir_index);
    //     scope.markDead(field_init_inst_index);
    //     const field_init_inst = scope.getInstruction(field_init_inst_index);

    //     const field_init_inst_value = ctx.builder.getValue(field_init_inst.typed_value.value) orelse std.debug.panic("field_init_inst_value is not a number", .{});
    //     const property_name_range = field_init_inst_value.data.field_init.field_name;
    //     const property = module.data.module_declaration.fields.get(property_name_range) orelse unreachable;

    //     const field_entity = ctx.builder.getEntity(property.entity);
    //     const field_type = try field_entity.resolveType();
    //     const field_value_inst_index = try ctx.pushMaybeCastInstructionToType(
    //         null,
    //         field_init_inst_value.data.field_init.value_inst,
    //         field_type,
    //     ) orelse field_init_inst_value.data.field_init.value_inst;
    //     const index_inst_index = ctx.pushInstruction(null, .{
    //         .op = .constant,
    //         .typed_value = .{
    //             .type = Sema.Type.simple(.usize),
    //             .value = try ctx.builder.numberAsBytesValueKey(property.index),
    //         },
    //         .data = .void,
    //     });
    //     const get_element_pointer_inst_index = ctx.pushInstruction(null, .{
    //         .op = .get_element_pointer,
    //         .typed_value = .{
    //             .type = try ctx.builder.internTypeData(.{ .pointer = .{ .child = field_type } }),
    //             .value = Sema.Value.simple(.exec_time),
    //         },
    //         .data = .{ .get_element_pointer = .{
    //             .base = index,
    //             .index = index_inst_index,
    //         } },
    //     });

    //     _ = ctx.pushInstruction(null, .{
    //         .op = .store,
    //         .typed_value = .{
    //             .type = Sema.Type.simple(.void),
    //             .value = Sema.Value.simple(.void),
    //         },
    //         .data = .{ .operand_payload = .{
    //             .operand = get_element_pointer_inst_index,
    //             .payload = field_value_inst_index,
    //         } },
    //     });
    //     return index;
    // }

    return index;
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    _ = ctx; // autofix
    _ = inst_index; // autofix
}
