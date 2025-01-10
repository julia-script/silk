const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);

    const type_inst_index = scope.getInstructionIndex(hir_inst.alloc.type);
    const type_inst = scope.getInstruction(type_inst_index);
    const type_to_alloc = ctx.builder.unwrapTypeValue(type_inst.typed_value.value);
    // const type_to_alloc = ctx.builder.unwrapPointerType(pointer_type_to_alloc) orelse std.debug.panic("type_to_alloc is not a pointer type", .{});
    const value_inst_index = scope.getInstructionIndex(hir_inst.alloc.init);
    const value_inst = scope.getInstruction(value_inst_index);
    // const value_to_alloc = ctx.builder.unwrapTypeValue(value_inst.typed_value.value);
    scope.markDead(type_inst_index);
    // This means that the allocation is already done on a LOAD instruction
    // Ex.
    // const a = T {};
    // const b = a;
    // will translate to HIR
    // %1 = alloc T {};
    // %2 = load %1;
    // %3 = alloc T {};
    // %4 = store %3, %2;
    // if it's a primitive value, this will translate 1 to 1 with the hir instructions at the sema level, for example
    // %1 = alloc i32
    // %2 = load %1;
    // %2 = alloc i32
    // %3 = store %2, %1;
    //
    // but if it's a complex value, we need to do a memcpy
    // %1 = alloc T {};
    // %2 = alloc T {};
    // %3 = memcpy %2, %1;
    //

    // if (value_inst.op == .memdupe) {
    //     ctx.markDead(value_inst_index);
    //     ctx.setIdMap(hir_inst_index, value_inst_index);
    //     const index = ctx.pushInstruction(hir_inst_index, .{
    //         .op = .alloc,
    //         .typed_value = .{
    //             .type = type_to_alloc,
    //             .value = value_inst.typed_value.value,
    //         },
    //         .data = .{ .alloc = .{
    //             .type = type_to_alloc,
    //             .mutable = hir_inst.alloc.mutable,
    //         } },
    //     });

    //     _ = ctx.pushInstruction(null, .{
    //         .op = .memcpy,
    //         .typed_value = .{
    //             .type = Sema.Type.simple(.void),
    //             .value = Sema.Value.simple(.void),
    //         },
    //         .data = .{ .memcpy = .{
    //             .src = value_inst.data.operand,
    //             .dest = index,
    //         } },
    //     });
    //     return index;
    // }

    const index = ctx.pushInstruction(hir_inst_index, .{
        .op = .alloc,
        .typed_value = .{
            .type = try ctx.builder.internTypeData(.{
                .pointer = .{
                    .child = type_to_alloc,
                },
            }),
            .value = Sema.Value.simple(.exec_time),
        },
        .data = .{ .alloc = .{
            .type = type_to_alloc,
            .mutable = hir_inst.alloc.mutable,
        } },
    });
    switch (value_inst.op) {
        .type_init => {
            scope.markDead(value_inst_index);
            try maybeInline(ctx, index);
            // const type_init = ctx.builder.getComplexValue(value_inst.typed_value.value).data.type_init;
            const list = ctx.builder.sema.lists.getSlice(value_inst.data.type_init.field_init_list);
            // try Index.exec(ctx, index);
            for (list) |field_inst_index| {
                _ = try pushSetInstructionField(ctx, index, field_inst_index);
            }

            // return try handleTypeInit(ctx, scope, index, value_inst_index);
            return index;
        },
        .array_init => {
            scope.markDead(value_inst_index);
            try maybeInline(ctx, index);
            return try handleArrayInit(ctx, scope, index, value_inst_index);
        },
        else => {},
    }
    if (ctx.is_comptime or hir_inst.alloc.mutable == false and value_inst.typed_value.isComptimeKnown()) {
        // try Index.exec(ctx, index);
        try maybeInline(ctx, index);
    }

    // )
    switch (type_to_alloc) {
        .complex => |complex| {
            _ = complex; // autofix
            switch (value_inst.op) {
                .load => {

                    // const value_inst = ctx.getInstruction(value_inst_index);
                    ctx.markDead(value_inst_index);
                    const memcpy = ctx.pushInstruction(null, .{
                        .op = .memcpy,
                        .typed_value = .{
                            .type = Sema.Type.simple(.void),
                            .value = Sema.Value.simple(.void),
                        },
                        .data = .{ .memcpy = .{
                            .dest = index,
                            .src = value_inst.data.operand,
                        } },
                    });
                    try maybeInline(ctx, memcpy);
                    return index;
                },
                .constant => {
                    ctx.markDead(value_inst_index);
                    const memcpy = ctx.pushInstruction(null, .{
                        .op = .memcpy,
                        .typed_value = .{
                            .type = Sema.Type.simple(.void),
                            .value = Sema.Value.simple(.void),
                        },
                        .data = .{ .memcpy = .{
                            .dest = index,
                            .src = value_inst_index,
                        } },
                    });
                    try maybeInline(ctx, memcpy);
                    return index;
                },

                else => {
                    std.debug.panic("unhandled store value: {s}", .{@tagName(value_inst.op)});
                },
            }
            // switch (ctx.builder.getComplexType(complex).data) {
            //     .@"struct" => {
            //         return try handleStoreStruct(ctx, scope, index, value_inst_index);
            //     },
            //     .array => {
            //         return try handleStoreArray(ctx, scope, index, value_inst_index);
            //     },
            //     else => |data| {
            //         std.debug.panic("unhandled store value: {s}", .{@tagName(data)});
            //     },
            // }
        },
        .simple => |simple| switch (simple) {
            .i8,
            .i16,
            .i32,
            .i64,

            .u8,
            .u16,
            .u32,
            .u64,
            .usize,

            .f32,
            .f64,
            .bchar,
            .bool,
            .boolean,
            .int,
            .float,
            .void,
            => {
                // const value_inst_index = try scope.getInstructionAsTypeByHirInst(hir_inst.store.value, pointer_type);
                const store_index = ctx.pushInstruction(null, .{
                    .op = .store,
                    .typed_value = .{
                        .type = Sema.Type.simple(.void),
                        .value = Sema.Value.simple(.void),
                    },
                    .data = .{ .operand_payload = .{
                        .operand = index,
                        .payload = try scope.getInstructionAsTypeByHirInst(hir_inst.alloc.init, type_to_alloc),
                    } },
                });
                try Index.Store.maybeInline(ctx, store_index);
                // try maybeInline(ctx, store_index);
                return store_index;
            },
            else => |data| {
                std.debug.panic("unhandled store value: {s}", .{@tagName(data)});
            },
        },
    }

    return index;
}
// pub fn handleTypeInit(ctx: *InstContext, scope: *GenScope, pointer_inst_index: Sema.Instruction.Index, value_inst_index: Sema.Instruction.Index) !Sema.Instruction.Index {

//     // const value_inst_index = scope.getInstructionIndex(hir_inst.store.value);
//     const value_inst = scope.getInstruction(value_inst_index);

//     switch (value_inst.op) {
//         .type_init => {
//             scope.markDead(value_inst_index);
//             const type_init = ctx.builder.getComplexValue(value_inst.typed_value.value).data.type_init;
//             const list = ctx.builder.sema.lists.getSlice(type_init.field_init_list);
//             for (list) |field_inst_index| {
//                 _ = try pushSetInstructionField(ctx, pointer_inst_index, field_inst_index);
//             }

//             return pointer_inst_index;
//         },
//         .load => {
//             // const value_inst = ctx.getInstruction(value_inst_index);
//             ctx.markDead(value_inst_index);
//             const index = ctx.pushInstruction(null, .{
//                 .op = .memcpy,
//                 .typed_value = .{
//                     .type = Sema.Type.simple(.void),
//                     .value = Sema.Value.simple(.void),
//                 },
//                 .data = .{ .memcpy = .{
//                     .dest = pointer_inst_index,
//                     .src = value_inst.data.operand,
//                 } },
//             });
//             try maybeInline(ctx, index);
//             return index;
//         },
//         else => {
//             std.debug.panic("unhandled store value: {s}", .{@tagName(value_inst.op)});
//         },
//     }
// }
fn pushSetInstructionField(
    ctx: *InstContext,
    struct_inst_index: Sema.Instruction.Index,
    field_inst_index: Sema.Instruction.Index,
) !Sema.Instruction.Index {
    const struct_inst = ctx.getInstruction(struct_inst_index);

    const struct_unwrapped_pointer_type_key = ctx.builder.unwrapPointerType(struct_inst.typed_value.type) orelse std.debug.panic("struct_unwrapped_pointer_type is not a type", .{});
    const struct_type = ctx.builder.getType(struct_unwrapped_pointer_type_key) orelse std.debug.panic("type_to_store is not a type", .{});

    const module = ctx.builder.getEntity(struct_type.data.@"struct".entity);
    const field_init_inst = ctx.getInstruction(field_inst_index);
    // const field_init_inst_value = ctx.builder.getValue(field_init_inst.typed_value.value) orelse std.debug.panic("field_init_inst_value is not a number", .{});
    const property_name_range = field_init_inst.data.field_init.field_name;
    ctx.markDead(field_inst_index);

    const property = module.data.module_declaration.fields.get(property_name_range) orelse unreachable;

    const field_entity = ctx.builder.getEntity(property.entity);
    const field_type = try field_entity.resolveType();
    const field_value_inst_index = try ctx.pushMaybeCastInstructionToType(
        null,
        field_init_inst.data.field_init.value_inst,
        field_type,
    ) orelse field_inst_index;

    const index_inst_index = ctx.pushInstruction(null, .{
        .op = .constant,
        .typed_value = .{
            .type = Sema.Type.simple(.usize),
            .value = try ctx.builder.numberAsBytesValueKey(property.index),
        },
        .data = .void,
    });
    const get_element_pointer_inst_index = ctx.pushInstruction(null, .{
        .op = .get_element_pointer,
        .typed_value = .{
            .type = try ctx.builder.internTypeData(.{ .pointer = .{ .child = field_type } }),
            .value = Sema.Value.simple(.exec_time),
        },
        .data = .{ .get_element_pointer = .{
            .base = struct_inst_index,
            .index = index_inst_index,
        } },
    });
    try Index.GetElementPtr.maybeInline(ctx, get_element_pointer_inst_index);

    const store_inst_index = ctx.pushInstruction(null, .{
        .op = .store,
        .typed_value = .{
            .type = Sema.Type.simple(.void),
            .value = Sema.Value.simple(.void),
        },
        .data = .{ .operand_payload = .{
            .operand = get_element_pointer_inst_index,
            .payload = field_value_inst_index,
        } },
    });
    try Index.Store.maybeInline(ctx, store_inst_index);
    return store_inst_index;
}
pub fn handleArrayInit(ctx: *InstContext, scope: *GenScope, pointer_inst_index: Sema.Instruction.Index, value_inst_index: Sema.Instruction.Index) !Sema.Instruction.Index {
    const pointer_inst = scope.getInstruction(pointer_inst_index);

    const value_inst = scope.getInstruction(value_inst_index);

    // switch (value_inst.op) {
    //     .array_init => {
    scope.markDead(value_inst_index);
    const array_init = value_inst.data.array_init;
    const array_type = scope.builder.unwrapPointerType(pointer_inst.typed_value.type) orelse std.debug.panic("array_type is not a type", .{});
    const element_type = scope.builder.getComplexType(array_type).data.array.child;

    const list = ctx.builder.sema.lists.getSlice(array_init.items_list);
    for (list, 0..) |item_inst_index, i| {
        const index_inst = ctx.pushInstruction(null, .{
            .op = .constant,
            .typed_value = .{
                .type = Sema.Type.simple(.usize),
                .value = try ctx.builder.numberAsBytesValueKey(i),
            },
            .data = .void,
        });

        const get_element_pointer_inst = blk: {
            const data: Sema.Instruction.Data = .{ .get_element_pointer = .{
                .base = pointer_inst_index,
                .index = index_inst,
            } };

            break :blk ctx.pushInstruction(null, .{
                .op = .get_element_pointer,
                .typed_value = .{
                    .type = try ctx.builder.internTypeData(.{ .pointer = .{ .child = element_type } }),
                    .value = Sema.Value.simple(.exec_time),
                },
                .data = data,
            });
        };
        try Index.GetElementPtr.maybeInline(ctx, get_element_pointer_inst);
        const item_inst = try ctx.pushMaybeCastInstructionToType(null, item_inst_index, element_type) orelse item_inst_index;

        const store_inst = ctx.pushInstruction(null, .{
            .op = .store,
            .typed_value = .{
                .type = Sema.Type.simple(.void),
                .value = Sema.Value.simple(.void),
            },
            .data = .{ .operand_payload = .{
                .operand = get_element_pointer_inst,
                .payload = item_inst,
            } },
        });
        try Index.Store.maybeInline(ctx, store_inst);
        // try maybeInline(ctx, store_inst);
    }

    return pointer_inst_index;
    // },

    // .load => {
    //     const type_to_store = ctx.builder.unwrapPointerType(pointer_inst.typed_value.type) orelse std.debug.panic("type_to_store is not a type", .{});

    //     const value_to_store_inst_index = try ctx.pushMaybeCastInstructionToType(
    //         null,
    //         value_inst_index,
    //         type_to_store,
    //     ) orelse value_inst_index;
    //     // const value_inst = ctx.getInstruction(value_inst_index);
    //     // std.debug.panic("type_to_store: {any}\n", .{type_to_store});

    //     const index = ctx.pushInstruction(null, .{
    //         .op = .memcpy,
    //         .typed_value = .{
    //             .type = Sema.Type.simple(.void),
    //             .value = Sema.Value.simple(.void),
    //         },
    //         .data = .{ .memcpy = .{
    //             .src = pointer_inst_index,
    //             .dest = value_to_store_inst_index,
    //         } },
    //     });
    //     try maybeInline(ctx, index);

    //     return index;
    // },
    // else => {
    //     std.debug.panic("unhandled store value: {s}", .{@tagName(value_inst.op)});
    // },
    // }
}
pub fn maybeInline(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    switch (inst.op) {
        .alloc => {
            if (!ctx.is_comptime and inst.data.alloc.mutable) return;
            const ptr = ctx.builder.sema.memory.stackCreate(inst.typed_value.type);
            ctx.setValue(inst_index, .{
                .type = inst.typed_value.type,
                .value = try ctx.builder.numberAsBytesValueKey(ptr),
            });

            // const type_inst_value = inst.typed_value.isComptimeKnown();
            // if (!type_inst_value.isComptimeKnown()) return;
            // const base_inst_value = ctx.getTypedValue(inst.data.alloc.);

        },
        // .store => {
        //     var base_inst_value = ctx.getTypedValue(inst.data.operand_payload.operand);
        //     var value_inst_value = ctx.getTypedValue(inst.data.operand_payload.payload);
        //     if (!base_inst_value.isComptimeKnown()) return;
        //     if (!value_inst_value.isComptimeKnown()) return;
        //     const ptr = try ctx.builder.readNumberAsType(usize, base_inst_value);
        //     try ctx.builder.sema.memory.store(ptr, value_inst_value);
        // },
        .memcpy => {
            var src_inst_value = ctx.getTypedValue(inst.data.memcpy.src);
            var dest_inst_value = ctx.getTypedValue(inst.data.memcpy.dest);
            if (!src_inst_value.isComptimeKnown()) return;
            if (!dest_inst_value.isComptimeKnown()) return;
            std.debug.print("maybeInline memcpy {any} {any}\n", .{ ctx.builder.getFormattableTypedValue(src_inst_value), ctx.builder.getFormattableTypedValue(dest_inst_value) });
            const src = try ctx.builder.readNumberAsType(usize, src_inst_value);
            const dest = try ctx.builder.readNumberAsType(usize, dest_inst_value);
            const type_to_memcpy = ctx.builder.unwrapPointerType(dest_inst_value.type) orelse std.debug.panic("type_to_memcpy is not a pointer type", .{});
            try ctx.builder.sema.memory.memcpy(type_to_memcpy, src, dest);
        },
        else => {
            std.debug.panic("unhandled store value: {s}", .{@tagName(inst.op)});
        },
    }
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);

    const ptr = ctx.builder.sema.memory.stackCreate(inst.data.alloc.type);

    ctx.setValue(inst_index, .{
        .type = inst.typed_value.type,
        .value = try ctx.builder.numberAsBytesValueKey(ptr),
    });
}
