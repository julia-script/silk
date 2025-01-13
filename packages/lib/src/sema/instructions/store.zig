const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");
const Self = @This();

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);

    const pointer_inst_index = scope.getInstructionIndex(hir_inst.store.pointer);
    const pointer_inst = ctx.getInstruction(pointer_inst_index);
    const pointer_type = ctx.builder.unwrapPointerType(pointer_inst.typed_value.type) orelse std.debug.panic("unwrapped_pointer_inst is not a type {d}", .{pointer_inst_index});

    switch (pointer_type) {
        .complex => |complex| switch (ctx.builder.getComplexType(complex).data) {
            .@"struct" => {
                return try handleStoreStruct(ctx, scope, hir_inst_index);
            },
            .array => {
                return try handleStoreArray(ctx, scope, hir_inst_index);
            },
            else => |data| {
                std.debug.panic("unhandled store value: {s}", .{@tagName(data)});
            },
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
                const instruction_index = scope.getInstructionIndex(hir_inst.store.value);

                const value_inst_index = (try ctx.pushMaybeCastInstructionToType(
                    hir_inst.store.value,
                    instruction_index,
                    pointer_type,
                )) orelse instruction_index;
                const index = ctx.pushInstruction(hir_inst_index, .{
                    .op = .store,
                    .typed_value = .{
                        .type = Sema.Type.simple(.void),
                        .value = Sema.Value.simple(.void),
                    },
                    .data = .{ .operand_payload = .{
                        .operand = pointer_inst_index,
                        .payload = value_inst_index,
                    } },
                });
                try maybeInline(ctx, index);
                return index;
            },

            else => |data| {
                std.debug.panic("unhandled store value: {s}", .{@tagName(data)});
            },
        },
    }
}
pub fn pushSetInstructionField(
    ctx: *InstContext,
    hir_inst_index: Hir.Inst.Index,
    struct_inst_index: Sema.Instruction.Index,
    field_inst_index: Sema.Instruction.Index,
) !Sema.Instruction.Index {
    const struct_inst = ctx.getInstruction(struct_inst_index);

    const struct_unwrapped_pointer_type_key = ctx.builder.unwrapPointerType(struct_inst.typed_value.type) orelse std.debug.panic("struct_unwrapped_pointer_type is not a type", .{});
    const struct_type = ctx.builder.getType(struct_unwrapped_pointer_type_key) orelse std.debug.panic("type_to_store is not a type", .{});

    const module = ctx.builder.getEntity(struct_type.data.@"struct".entity);
    const field_init_inst = ctx.getInstruction(field_inst_index);
    const field_init_inst_value = ctx.builder.getValue(field_init_inst.typed_value.value) orelse std.debug.panic("field_init_inst_value is not a number", .{});
    _ = field_init_inst_value; // autofix
    const property_name_range = field_init_inst.data.field_init.field_name;
    ctx.markDead(field_inst_index);

    const property = module.data.module_declaration.fields.get(property_name_range) orelse unreachable;

    const field_entity = ctx.builder.getEntity(property.entity);
    const field_type = try field_entity.resolveType();
    const field_value_inst_index = try ctx.pushMaybeCastInstructionToType(
        hir_inst_index,
        field_init_inst.data.field_init.value_inst,
        field_type,
    ) orelse field_inst_index;

    const index_inst_index = ctx.pushInstruction(hir_inst_index, .{
        .op = .constant,
        .typed_value = .{
            .type = Sema.Type.simple(.usize),
            .value = try ctx.builder.numberAsBytesValueKey(property.index),
        },
        .data = .void,
    });
    const get_element_pointer_inst_index = ctx.pushInstruction(hir_inst_index, .{
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

    const store_inst_index = ctx.pushInstruction(hir_inst_index, .{
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
    try maybeInline(ctx, store_inst_index);
    return store_inst_index;
}
pub fn handleStoreStruct(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const pointer_inst_index = scope.getInstructionIndex(hir_inst.store.pointer);

    const value_inst_index = scope.getInstructionIndex(hir_inst.store.value);
    const value_inst = ctx.getInstruction(value_inst_index);

    switch (value_inst.op) {
        .type_init => {
            ctx.markDead(value_inst_index);
            // const type_init = ctx.builder.getComplexValue(value_inst.typed_value.value).data.type_init;
            const list = ctx.builder.sema.lists.getSlice(value_inst.data.type_init.field_init_list);
            for (list) |field_inst_index| {
                _ = try pushSetInstructionField(ctx, hir_inst_index, pointer_inst_index, field_inst_index);
            }

            return pointer_inst_index;
        },
        .load => {
            const index = ctx.pushInstruction(hir_inst_index, .{
                .op = .store,
                .typed_value = .{
                    .type = Sema.Type.simple(.void),
                    .value = Sema.Value.simple(.void),
                },
                .data = .{ .operand_payload = .{
                    .operand = pointer_inst_index,
                    .payload = value_inst_index,
                } },
            });
            try maybeInline(ctx, index);
            return index;
        },
        else => {
            std.debug.panic("unhandled store value: {s}", .{@tagName(value_inst.op)});
        },
    }
}
pub fn handleStoreArray(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const pointer_inst_index = scope.getInstructionIndex(hir_inst.store.pointer);
    const pointer_inst = ctx.getInstruction(pointer_inst_index);

    const value_inst_index = scope.getInstructionIndex(hir_inst.store.value);
    const value_inst = ctx.getInstruction(value_inst_index);

    switch (value_inst.op) {
        .array_init => {
            ctx.markDead(value_inst_index);
            const array_init = value_inst.data.array_init;
            const array_type = scope.builder.unwrapPointerType(pointer_inst.typed_value.type) orelse std.debug.panic("array_type is not a type", .{});
            const element_type = scope.builder.getComplexType(array_type).data.array.child;

            const list = ctx.builder.sema.lists.getSlice(array_init.items_list);
            for (list, 0..) |item_inst_index, i| {
                const index_inst = ctx.pushInstruction(hir_inst_index, .{
                    .op = .constant,
                    .typed_value = .{
                        .type = Sema.Type.simple(.usize),
                        .value = try ctx.builder.numberAsBytesValueKey(i),
                    },
                    .data = .void,
                });

                const get_element_pointer_inst = ctx.pushInstruction(hir_inst_index, .{
                    .op = .get_element_pointer,
                    .typed_value = .{
                        .type = try ctx.builder.internTypeData(.{ .pointer = .{ .child = element_type } }),
                        .value = Sema.Value.simple(.exec_time),
                    },
                    .data = .{ .get_element_pointer = .{
                        .base = pointer_inst_index,
                        .index = index_inst,
                    } },
                });
                try Index.GetElementPtr.maybeInline(ctx, get_element_pointer_inst);

                const item_inst = try ctx.pushMaybeCastInstructionToType(hir_inst_index, item_inst_index, element_type) orelse item_inst_index;

                const store_inst = ctx.pushInstruction(hir_inst_index, .{
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
                try maybeInline(ctx, store_inst);
                // try maybeInline(ctx, store_inst);
            }

            return pointer_inst_index;
        },

        .constant => {
            const type_to_store = ctx.builder.unwrapPointerType(pointer_inst.typed_value.type) orelse std.debug.panic("type_to_store is not a type", .{});

            const value_to_store_inst_index = try ctx.pushMaybeCastInstructionToType(
                hir_inst.store.value,
                value_inst_index,
                type_to_store,
            ) orelse value_inst_index;
            const index = ctx.pushInstruction(hir_inst_index, .{
                .op = .store,
                .typed_value = .{
                    .type = Sema.Type.simple(.void),
                    .value = Sema.Value.simple(.void),
                },
                .data = .{ .operand_payload = .{
                    .operand = pointer_inst_index,
                    .payload = value_to_store_inst_index,
                } },
            });
            try maybeInline(ctx, index);
            return index;
        },
        else => {
            std.debug.panic("unhandled store value: {s}", .{@tagName(value_inst.op)});
        },
    }
}
pub fn maybeInline(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    var base_inst_value = ctx.getTypedValue(inst.data.operand_payload.operand);
    var value_inst_value = ctx.getTypedValue(inst.data.operand_payload.payload);
    std.debug.print("maybeInline {d} {any} {any}\n", .{ inst_index, base_inst_value, value_inst_value });
    if (!base_inst_value.isComptimeKnown()) return;
    if (!value_inst_value.isComptimeKnown()) return;
    const ptr = try ctx.builder.readNumberAsType(usize, base_inst_value);
    // ctx.markDead(inst.data.operand_payload.operand);
    // ctx.markDead(inst.data.operand_payload.payload);
    // ctx.markDead(inst_index);
    try ctx.builder.sema.memory.store(ptr, value_inst_value);
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    var base_inst_value = ctx.getTypedValue(inst.data.operand_payload.operand);
    var value_inst_value = ctx.getTypedValue(inst.data.operand_payload.payload);
    if (!base_inst_value.isComptimeKnown()) @panic("not a comptime known value");
    if (!value_inst_value.isComptimeKnown()) @panic("not a comptime known value");
    const ptr = try ctx.builder.readNumberAsType(usize, base_inst_value);
    // std.debug.print("{d} base: {any} value: {any}\n", .{ inst_index, base, value_inst_value });
    try ctx.builder.sema.memory.store(ptr, value_inst_value);
}
