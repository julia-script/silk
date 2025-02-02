const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");
pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);

    const type_inst_index = block.ctx.getInstructionByHirIndex(hir_inst.alloc.type);
    const type_inst = block.ctx.getInstruction(type_inst_index);
    const type_to_alloc = block.ctx.builder.unwrapTypeValue(type_inst.typed_value.value);
    // const type_to_alloc = ctx.builder.unwrapPointerType(pointer_type_to_alloc) orelse std.debug.panic("type_to_alloc is not a pointer type", .{});
    const value_inst_index = block.ctx.getInstructionByHirIndex(hir_inst.alloc.init);
    const value_inst = block.ctx.getInstruction(value_inst_index);
    // const value_to_alloc = ctx.builder.unwrapTypeValue(value_inst.typed_value.value);
    block.ctx.markDead(type_inst_index);

    const entity = block.ctx.getEntity();
    const name = try entity.internNode(hir_inst.alloc.name_node);
    const index = try block.appendInstruction(hir_inst_index, .{
        .op = .alloc,
        .typed_value = .{
            .type = try block.ctx.builder.internTypeData(.{
                .pointer = .{
                    .child = type_to_alloc,
                },
            }),
            .value = Sema.Value.simple(.exec_time),
        },
        .data = .{
            .alloc = .{
                .type = type_to_alloc,
                .mutable = hir_inst.alloc.mutable,
                .name = name,
            },
        },
    });
    switch (value_inst.op) {
        .type_init => {
            block.ctx.markDead(value_inst_index);
            try maybeInline(block, index);
            const list = block.ctx.builder.sema.lists.getSlice(value_inst.data.type_init.field_init_list);
            for (list) |field_inst_index| {
                _ = try pushSetInstructionField(block, index, field_inst_index);
            }

            return index;
        },
        .array_init => {
            block.ctx.markDead(value_inst_index);
            try maybeInline(block, index);
            return try handleArrayInit(block, index, value_inst_index);
        },
        else => {},
    }
    if (block.is_comptime or hir_inst.alloc.mutable == false and value_inst.typed_value.isComptimeKnown()) {
        try maybeInline(block, index);
    }

    switch (type_to_alloc) {
        .complex => |complex| {
            _ = complex; // autofix
            switch (value_inst.op) {
                .load => {
                    block.ctx.markDead(value_inst_index);
                    const memcpy = try block.appendInstruction(null, .{
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
                    try maybeInline(block, memcpy);
                    return index;
                },
                .constant => {
                    block.ctx.markDead(value_inst_index);
                    const memcpy = try block.appendInstruction(null, .{
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
                    try maybeInline(block, memcpy);
                    return index;
                },

                else => {
                    std.debug.panic("unhandled store value: {s}", .{@tagName(value_inst.op)});
                },
            }
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
                const instruction_index = block.ctx.getInstructionByHirIndex(hir_inst.alloc.init);
                const store_value_inst_index = (try block.pushMaybeCastInstructionToType(
                    hir_inst.alloc.init,
                    instruction_index,
                    type_to_alloc,
                )) orelse instruction_index;
                const store_index = try block.appendInstruction(null, .{
                    .op = .store,
                    .typed_value = .{
                        .type = Sema.Type.simple(.void),
                        .value = Sema.Value.simple(.void),
                    },
                    .data = .{
                        .operand_payload = .{
                            .operand = index,
                            .payload = store_value_inst_index, //try ctx.getInstructionAsTypeByHirInst(hir_inst.alloc.init, type_to_alloc),
                        },
                    },
                });
                try Index.Store.maybeInline(block, store_index);
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

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);

    const type_inst_index = scope.getInstructionIndex(hir_inst.alloc.type);
    const type_inst = ctx.getInstruction(type_inst_index);
    const type_to_alloc = ctx.builder.unwrapTypeValue(type_inst.typed_value.value);
    // const type_to_alloc = ctx.builder.unwrapPointerType(pointer_type_to_alloc) orelse std.debug.panic("type_to_alloc is not a pointer type", .{});
    const value_inst_index = scope.getInstructionIndex(hir_inst.alloc.init);
    const value_inst = ctx.getInstruction(value_inst_index);
    // const value_to_alloc = ctx.builder.unwrapTypeValue(value_inst.typed_value.value);
    ctx.markDead(type_inst_index);

    const name = try scope.entity.internNode(hir_inst.alloc.name_node);
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
        .data = .{
            .alloc = .{
                .type = type_to_alloc,
                .mutable = hir_inst.alloc.mutable,
                .name = name,
            },
        },
    });
    switch (value_inst.op) {
        .type_init => {
            ctx.markDead(value_inst_index);
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
            ctx.markDead(value_inst_index);
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
                const instruction_index = scope.getInstructionIndex(hir_inst.alloc.init);
                const store_value_inst_index = (try ctx.pushMaybeCastInstructionToType(
                    hir_inst.alloc.init,
                    instruction_index,
                    type_to_alloc,
                )) orelse instruction_index;
                const store_index = ctx.pushInstruction(null, .{
                    .op = .store,
                    .typed_value = .{
                        .type = Sema.Type.simple(.void),
                        .value = Sema.Value.simple(.void),
                    },
                    .data = .{
                        .operand_payload = .{
                            .operand = index,
                            .payload = store_value_inst_index, //try ctx.getInstructionAsTypeByHirInst(hir_inst.alloc.init, type_to_alloc),
                        },
                    },
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
    block: *InstContext.Block,
    struct_inst_index: Sema.Instruction.Index,
    field_inst_index: Sema.Instruction.Index,
) !Sema.Instruction.Index {
    const struct_inst = block.ctx.getInstruction(struct_inst_index);

    const struct_unwrapped_pointer_type_key = block.ctx.builder.unwrapPointerType(struct_inst.typed_value.type) orelse std.debug.panic("struct_unwrapped_pointer_type is not a type", .{});
    const struct_type = block.ctx.builder.getType(struct_unwrapped_pointer_type_key) orelse std.debug.panic("type_to_store is not a type", .{});

    const module = block.ctx.builder.getEntity(struct_type.data.@"struct".entity);
    const field_init_inst = block.ctx.getInstruction(field_inst_index);
    // const field_init_inst_value = ctx.builder.getValue(field_init_inst.typed_value.value) orelse std.debug.panic("field_init_inst_value is not a number", .{});
    const property_name_range = field_init_inst.data.field_init.field_name;
    block.ctx.markDead(field_inst_index);

    const property = module.data.module_declaration.fields.get(property_name_range) orelse unreachable;

    const field_entity = block.ctx.builder.getEntity(property.entity);
    const field_type = try field_entity.resolveType();

    const get_element_pointer_inst_index = try block.appendInstruction(null, .{
        .op = .get_element_pointer,
        .typed_value = .{
            .type = try block.ctx.builder.internTypeData(.{ .pointer = .{ .child = field_type } }),
            .value = Sema.Value.simple(.exec_time),
        },
        .data = .{ .get_element_pointer = .{
            .base = struct_inst_index,
            .index = .{ .constant = .{
                .type = block.ctx.builder.getPointerType(.unsigned),
                .value = try block.ctx.builder.numberAsBytesValueKey(property.index),
            } },
        } },
    });
    try Index.GetElementPtr.maybeInline(block, get_element_pointer_inst_index);
    const field_value_inst_index = try block.pushMaybeCastInstructionToType(
        null,
        field_init_inst.data.field_init.value_inst,
        field_type,
    ) orelse field_inst_index;

    const store_inst_index = try block.appendInstruction(null, .{
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
    try Index.Store.maybeInline(block, store_inst_index);
    return store_inst_index;
}
pub fn handleArrayInit(block: *InstContext.Block, pointer_inst_index: Sema.Instruction.Index, value_inst_index: Sema.Instruction.Index) !Sema.Instruction.Index {
    const pointer_inst = block.ctx.getInstruction(pointer_inst_index);

    const value_inst = block.ctx.getInstruction(value_inst_index);

    // switch (value_inst.op) {
    //     .array_init => {
    block.ctx.markDead(value_inst_index);
    const array_init = value_inst.data.array_init;
    const array_type = block.ctx.builder.unwrapPointerType(pointer_inst.typed_value.type) orelse std.debug.panic("array_type is not a type", .{});
    const element_type = block.ctx.builder.getComplexType(array_type).data.array.child;

    const list = block.ctx.builder.sema.lists.getSlice(array_init.items_list);
    for (list, 0..) |item_inst_index, i| {
        // const index_inst = ctx.pushInstruction(null, .{
        //     .op = .constant,
        //     .typed_value = .{
        //         .type = Sema.Type.simple(.usize),
        //         .value = try ctx.builder.numberAsBytesValueKey(i),
        //     },
        //     .data = .void,
        // });

        const get_element_pointer_inst = blk: {
            const data: Sema.Instruction.Data = .{ .get_element_pointer = .{
                .base = pointer_inst_index,
                .index = .{ .constant = .{
                    .type = block.ctx.builder.getPointerType(.unsigned),
                    .value = try block.ctx.builder.numberAsBytesValueKey(i),
                } },
            } };

            break :blk try block.appendInstruction(null, .{
                .op = .get_element_pointer,
                .typed_value = .{
                    .type = try block.ctx.builder.internTypeData(.{ .pointer = .{ .child = element_type } }),
                    .value = Sema.Value.simple(.exec_time),
                },
                .data = data,
            });
        };
        try Index.GetElementPtr.maybeInline(block, get_element_pointer_inst);
        const item_inst = try block.pushMaybeCastInstructionToType(null, item_inst_index, element_type) orelse item_inst_index;

        const store_inst = try block.appendInstruction(null, .{
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
        try Index.Store.maybeInline(block, store_inst);
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
pub fn maybeInline(block: *InstContext.Block, inst_index: Sema.Instruction.Index) !void {
    const inst = block.ctx.getInstruction(inst_index);
    switch (inst.op) {
        .alloc => {
            if (!block.is_comptime and inst.data.alloc.mutable) return;
            const ptr = block.ctx.builder.sema.memory.stackCreate(inst.typed_value.type);
            block.ctx.setValue(inst_index, .{
                .type = inst.typed_value.type,
                .value = try block.ctx.builder.numberAsBytesValueKey(ptr),
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
            var src_inst_value = block.ctx.getTypedValue(inst.data.memcpy.src);
            var dest_inst_value = block.ctx.getTypedValue(inst.data.memcpy.dest);
            if (!src_inst_value.isComptimeKnown()) return;
            if (!dest_inst_value.isComptimeKnown()) return;
            std.debug.print("maybeInline memcpy {any} {any}\n", .{ block.ctx.builder.getFormattableTypedValue(src_inst_value), block.ctx.builder.getFormattableTypedValue(dest_inst_value) });
            const src = try block.ctx.builder.readNumberAsType(usize, src_inst_value);
            const dest = try block.ctx.builder.readNumberAsType(usize, dest_inst_value);
            const type_to_memcpy = block.ctx.builder.unwrapPointerType(dest_inst_value.type) orelse std.debug.panic("type_to_memcpy is not a pointer type", .{});
            try block.ctx.builder.sema.memory.memcpy(type_to_memcpy, src, dest);
        },
        else => {
            std.debug.panic("unhandled store value: {s}", .{@tagName(inst.op)});
        },
    }
}
const ExecContext = @import("./ExecContext.zig");
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);

    const ptr = ctx.builder.sema.memory.stackCreate(inst.data.alloc.type);

    try ctx.setValue(inst_index, .{
        .type = inst.typed_value.type,
        .value = try ctx.builder.numberAsBytesValueKey(ptr),
    });
}
