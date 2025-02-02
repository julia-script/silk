const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const genGlobalGetInstruction = @import("./global-get.zig").genGlobalGetInstruction;
const ExecContext = @import("./ExecContext.zig");

pub fn genFromBuiltinPropertyAccess(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    _ = ctx; // autofix
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    // @panic("todo");

    const base_hir_index = hir_inst.get_property_pointer.base;
    _ = base_hir_index; // autofix
    // _ = base_hir_index; // autofix

    const property_name_range = try scope.entity.internNode(hir_inst.get_property_pointer.property_name_node);
    // // const property_name_slice = self.builder.getSlice(property_name_range);
    return try scope.getPropertyByName(hir_inst_index, property_name_range);

    // const base_index = self.getInstructionIndex(base_hir_index);
    // const base_instruction = self.getInstruction(base_index);
    // if (self.builder.getType(base_instruction.type)) |base_type| {
    //     std.debug.panic("todo {s}", .{@tagName(base_type.data)});
    // }

    // // var a: u16 = 0xabcd; // runtime-known
    // // _ = &a;
    // // const b: u8 = @intCast(a);
    // // _ = b; // autofix

    // switch (base_instruction.type.simple) {
    //     .usize,
    //     .i8,
    //     .i16,
    //     .i32,
    //     .i64,
    //     .u8,
    //     .u16,
    //     .u32,
    //     .u64,
    //     .f32,
    //     .f64,

    //     .number,
    //     => {
    //         if (std.mem.eql(u8, property_name_slice, "as")) {
    //             return try self.pushInstruction(hir_inst_index, .{
    //                 .op = .get_builtin_fn_as,
    //                 .type = Sema.Type.simple(.builtin_fn_as),
    //                 .value = Sema.Value.simple(.type_builtin_fn_as),
    //                 .data = .{ .operand = base_index },
    //             });
    //         }
    //         std.debug.panic("todo {s}", .{property_name_slice});
    //     },
    //     else => |tag| std.debug.panic("unhandled base type: {s}", .{@tagName(tag)}),
    // }
    // std.debug.panic("unhandled base type", .{});
}
fn genFromGetTypeProperty(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    const base_hir_index = hir_inst.get_property_pointer.base;
    const base_inst_index = block.ctx.getInstructionByHirIndex(base_hir_index);
    const base_inst = block.ctx.getInstruction(base_inst_index);
    const base_type = block.ctx.builder.unwrapTypeValue(base_inst.typed_value.value);

    switch (base_type) {
        .simple => |simple_type| switch (simple_type) {
            // .type => |struct_type| {
            //     _ = struct_type; // autofix
            // },
            else => {
                std.debug.panic("getTypePropertyByName: unhandled base type: {s}", .{@tagName(simple_type)});
            },
        },
        .complex => |complex_type| switch (block.ctx.builder.getComplexType(complex_type).data) {
            .@"struct" => |struct_type| {
                const entity = block.ctx.builder.getEntity(struct_type.entity);
                const name_range = try entity.internNode(hir_inst.get_property_pointer.property_name_node);
                const declaration_entity_index = entity.data.module_declaration.declarations.get(name_range) orelse {
                    std.debug.panic("error: property '{s}' not found in struct '{s}'", .{
                        block.ctx.builder.getSlice(name_range),
                        block.ctx.builder.getSlice(entity.name),
                    });
                };
                return genGlobalGetInstruction(block, hir_inst_index, declaration_entity_index);
            },
            else => {
                const ty = block.ctx.builder.getComplexType(complex_type);
                std.debug.panic("getTypePropertyByName: unhandled complex type: {s}", .{@tagName(ty.data)});
            },
        },
    }
}
// const Entity = @import("../gen.zig").Entity;
// pub fn genGlobalGetInstruction(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index, entity_key: Entity.Key) !Sema.Instruction.Index {
//     const entity = scope.builder.getEntity(entity_key);
//     // try self.scope.pushDependency(entity_key);
//     const global_type = try entity.resolveType();
//     switch (entity.data) {
//         .function_declaration => |fn_decl| {
//             // global_entity.data.function_declaration.declaration_index,
//             return ctx.pushInstruction(hir_inst_index, .{
//                 .op = .global_get,
//                 .typed_value = .{
//                     .type = global_type,
//                     .value = try scope.maybeResolveDependency(entity_key),
//                 },
//                 .data = .{ .declaration = fn_decl.declaration_index },
//             });
//         },
//         .global_type_declaration => |type_decl| {
//             return ctx.pushInstruction(hir_inst_index, .{
//                 .op = .global_get,
//                 .typed_value = .{
//                     .type = global_type,
//                     .value = try scope.maybeResolveDependency(entity_key),
//                 },
//                 .data = .{ .declaration = type_decl.declaration_index },
//             });
//         },
//         else => std.debug.panic("unhandled global_entity: {s}", .{@tagName(entity.data)}),
//     }
// }

fn genFromGetPropertyPointer(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    // @panic("todo");
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    //
    const property_name_range = try block.ctx.getEntity().internNode(hir_inst.get_property_pointer.property_name_node);
    const base_hir_index = hir_inst.get_property_pointer.base;
    const base_inst_index = block.ctx.getInstructionByHirIndex(base_hir_index);
    const base_inst = block.ctx.getInstruction(base_inst_index);
    const base_type = block.ctx.builder.maybeUnwrapPointerType(base_inst.typed_value.type);
    const name_slice = block.ctx.builder.getSlice(property_name_range);
    if (base_type.isOneOfSimple(&.{ .int, .float, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .f32, .f64 })) {
        if (block.ctx.builder.isSliceEqual(property_name_range, "as")) {
            const ty = try block.ctx.builder.internTypeData(.{ .builtin_member = .{ .member = .as } });
            return try block.appendInstruction(hir_inst_index, .{
                .op = .constant,
                .typed_value = .{
                    .type = ty,
                    .value = try block.ctx.builder.internValueData(.{ .type = ty }),
                },
                .data = .{ .operand = base_inst_index },
            });
        }
    }

    switch (base_type) {
        .simple => |simple_type| switch (simple_type) {
            else => {
                std.debug.panic("getPropertyByName: {s} unhandled base type: {s}", .{
                    name_slice,
                    @tagName(simple_type),
                });
            },
        },
        .complex => |complex_type| switch (block.ctx.builder.getComplexType(complex_type).data) {
            .@"struct" => |struct_type| {
                const entity = block.ctx.builder.getEntity(struct_type.entity);

                if (entity.data.module_declaration.declarations.get(property_name_range)) |declaration| {
                    return genGlobalGetInstruction(block, hir_inst_index, declaration);
                }

                const field = entity.data.module_declaration.fields.get(property_name_range) orelse {
                    std.debug.panic("error: property '{s}' not found in struct '{s}'", .{
                        block.ctx.builder.getSlice(property_name_range),
                        block.ctx.builder.getSlice(entity.name),
                    });
                };
                const field_entity = block.ctx.builder.getEntity(field.entity);
                // const field_index = field
                std.debug.print("pushing index instruction\n", .{});
                // const base
                var field_value: Sema.Value.Key = Sema.Value.simple(.exec_time);
                // const field_type = try field_entity.resolveType();
                std.debug.print("field_type: {any}\n", .{block.ctx.builder.getFormattableTypedValue(base_inst.typed_value)});
                if (try block.ctx.builder.maybeGetPointer(base_inst.typed_value)) |ptr| {
                    // const offset = self.builder.getComplexType(field_type).data.struct_field.offset;

                    field_value = try block.ctx.builder.numberAsBytesValueKey(ptr + field.offset);
                }
                // const index_inst_index = ctx.pushInstruction(hir_inst_index, .{
                //     .op = .constant,
                //     .typed_value = .{
                //         .type = Sema.Type.simple(.usize),
                //         .value = try scope.builder.numberAsBytesValueKey(field.index),
                //     },
                //     .data = .void,
                // });
                // _ = index_inst_index; // autofix

                const get_element_pointer_inst = try block.appendInstruction(hir_inst_index, .{
                    .op = .get_element_pointer,
                    .typed_value = .{
                        .type = try block.ctx.builder.internTypeData(.{ .pointer = .{ .child = try field_entity.resolveType() } }),
                        .value = field_value,
                    },
                    // .value = base_inst.value,
                    // .value = field_value,
                    .data = .{ .get_element_pointer = .{
                        .base = base_inst_index,
                        .index = .{
                            .constant = .{
                                .type = block.ctx.builder.getPointerType(.unsigned),
                                .value = try block.ctx.builder.numberAsBytesValueKey(field.index),
                            },
                        },
                    } },
                });
                // std.debug.panic("todo {s}", .{self.builder.getSlice(name_range)});
                // return try self.pushGlobalGetInstruction(hir_inst_index, field.entity);
                try maybeInline(block, get_element_pointer_inst);
                return get_element_pointer_inst;
            },
            .array => |array_type| {
                if (block.ctx.builder.isSliceEqual(property_name_range, "len")) {
                    return try block.appendInstruction(hir_inst_index, .{
                        .op = .constant,
                        .typed_value = .{
                            .type = block.ctx.builder.getPointerType(.unsigned),
                            .value = try block.ctx.builder.numberAsBytesValueKey(array_type.len),
                        },
                        .data = .void,
                    });
                }
                std.debug.panic("todo {s}", .{block.ctx.builder.getSlice(property_name_range)});
            },
            else => {
                const ty = block.ctx.builder.getComplexType(complex_type);
                std.debug.panic("getPropertyByName: unhandled complex type: {s}", .{@tagName(ty.data)});
            },
        },
    }
}
pub fn genFromGetElementPointer(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    const base_index = block.ctx.getInstructionByHirIndex(hir_inst.get_element_pointer.base);
    const base_instruction = block.ctx.getInstruction(base_index);
    const instruction_index = block.ctx.getInstructionByHirIndex(hir_inst.get_element_pointer.index);

    const index_inst_index = (try block.pushMaybeCastInstructionToType(
        hir_inst.get_element_pointer.index,
        instruction_index,
        block.ctx.builder.getPointerType(.unsigned),
    )) orelse instruction_index;
    // const index_inst_index = try scope.getInstructionAsTypeByHirInst(hir_inst.get_element_pointer.index, Sema.Type.simple(.usize));

    const type_to_access = if (block.ctx.builder.getType(base_instruction.typed_value.type)) |ty| switch (ty.data) {
        .array, .slice => ty,
        .pointer => |pointer_type| block.ctx.builder.getComplexType(pointer_type.child).*,
        else => std.debug.panic("unhandled base type: {s}", .{@tagName(ty.data)}),
    } else std.debug.panic("unreachable: should get a base type", .{});

    const element_type = switch (type_to_access.data) {
        .slice => |slice_type| slice_type.child,
        .array => |array_type| array_type.child,
        else => std.debug.panic("unhandled base type: {s}", .{@tagName(type_to_access.data)}),
    };
    const index_typed_value = block.ctx.getTypedValue(index_inst_index);
    const index = try block.appendInstruction(hir_inst_index, .{
        .op = .get_element_pointer,
        .typed_value = .{
            .type = try block.ctx.builder.internTypeData(.{ .pointer = .{ .child = element_type } }),
            .value = Sema.Value.simple(.exec_time),
        },
        .data = .{ .get_element_pointer = .{
            .base = base_index,
            .index = if (index_typed_value.isComptimeKnown()) blk: {
                block.ctx.markDead(index_inst_index);

                break :blk .{ .constant = index_typed_value };
            } else .{
                .instruction = index_inst_index,
            },
        } },
    });
    try maybeInline(block, index);
    return index;
}
pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    switch (hir_inst) {
        .get_element_pointer => return try genFromGetElementPointer(block, hir_inst_index),
        .get_property_pointer => |get_property| {
            _ = get_property; // autofix
            // if (get_property.is_builtin) return try genFromBuiltinPropertyAccess(ctx, scope, hir_inst_index);
            const base_hir_index = hir_inst.get_property_pointer.base;

            const base_inst_index = block.ctx.getInstructionByHirIndex(base_hir_index);
            const base_inst = block.ctx.getInstruction(base_inst_index);
            const base_type = block.ctx.builder.maybeUnwrapPointerType(base_inst.typed_value.type);
            switch (base_type) {
                .simple => |simple_type| switch (simple_type) {
                    .type => return try genFromGetTypeProperty(block, hir_inst_index),
                    else => {},
                },
                .complex => |complex_type| switch (block.ctx.builder.getComplexType(complex_type).data) {
                    .typeof => return try genFromGetTypeProperty(block, hir_inst_index),
                    else => {},
                },
            }
            // if (base_type.isOneOfSimple(&.{ .type, .typeof })) return try genFromGetTypeProperty(ctx, scope, hir_inst_index);
            return try genFromGetPropertyPointer(block, hir_inst_index);
        },
        else => unreachable,
    }
}
pub fn maybeInline(block: *InstContext.Block, inst_index: Sema.Instruction.Index) !void {
    const inst = block.ctx.getInstruction(inst_index);
    const base = inst.data.get_element_pointer.base;
    const index = inst.data.get_element_pointer.index;
    const base_inst = block.ctx.getInstruction(base);
    const index_typed_value = switch (index) {
        .instruction => |index_inst_index| block.ctx.getInstruction(index_inst_index).typed_value,
        .constant => |constant| constant,
    };
    if (!base_inst.typed_value.isComptimeKnown()) return;
    if (!index_typed_value.isComptimeKnown()) return;
    const index_int = try block.ctx.builder.readNumberAsType(usize, index_typed_value);
    const base_type_key = block.ctx.builder.unwrapPointerType(base_inst.typed_value.type) orelse std.debug.panic("base_type is not a pointer type", .{});
    std.debug.print("base_type_key: {any}\n", .{base_type_key});

    const base_type = block.ctx.builder.getComplexType(base_type_key);
    const base_ptr = try block.ctx.builder.readNumberAsType(usize, base_inst.typed_value);
    switch (base_type.data) {
        .@"struct" => {
            const fields = block.ctx.builder.sema.lists.getSlice(base_type.data.@"struct".fields);

            const field_type = block.ctx.builder.getComplexType(Sema.Type.Key.decode(fields[index_int])).data.struct_field.offset;
            // const ptr = base_int + field_type;
            const ptr = base_ptr + field_type;
            block.ctx.setValue(inst_index, .{
                .type = inst.typed_value.type,
                .value = try block.ctx.builder.numberAsBytesValueKey(ptr),
            });
        },
        .array => {
            const array_type = block.ctx.builder.getComplexType(base_type_key).data.array;
            const element_size = block.ctx.builder.getTypeSize(array_type.child);
            const ptr = base_ptr + index_int * element_size;
            block.ctx.setValue(inst_index, .{
                .type = inst.typed_value.type,
                .value = try block.ctx.builder.numberAsBytesValueKey(ptr),
            });
        },
        else => std.debug.panic("unhandled base type: {s}", .{@tagName(base_type.data)}),
    }
}
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const base = inst.data.get_element_pointer.base;
    const index = inst.data.get_element_pointer.index;
    const base_inst = ctx.getInstruction(base);
    const index_typed_value = switch (index) {
        .instruction => |index_inst_index| ctx.getInstruction(index_inst_index).typed_value,
        .constant => |constant| constant,
    };
    if (!base_inst.typed_value.isComptimeKnown()) @panic("base_inst is not a comptime known value");
    if (!index_typed_value.isComptimeKnown()) @panic("index_inst is not a comptime known value");
    const index_int = try ctx.builder.readNumberAsType(usize, index_typed_value);
    const base_type_key = ctx.builder.unwrapPointerType(base_inst.typed_value.type) orelse std.debug.panic("base_type is not a pointer type", .{});

    const base_type = ctx.builder.getComplexType(base_type_key);
    const base_ptr = try ctx.builder.readNumberAsType(usize, base_inst.typed_value);
    switch (base_type.data) {
        .@"struct" => {
            const fields = ctx.builder.sema.lists.getSlice(base_type.data.@"struct".fields);

            const field_type = ctx.builder.getComplexType(Sema.Type.Key.decode(fields[index_int])).data.struct_field.offset;
            // const ptr = base_int + field_type;
            const ptr = base_ptr + field_type;
            try ctx.setValue(inst_index, .{
                .type = inst.typed_value.type,
                .value = try ctx.builder.numberAsBytesValueKey(ptr),
            });
        },
        .array => {
            const array_type = ctx.builder.getComplexType(base_type_key).data.array;
            const element_size = ctx.builder.getTypeSize(array_type.child);
            const ptr = base_ptr + index_int * element_size;
            try ctx.setValue(inst_index, .{
                .type = inst.typed_value.type,
                .value = try ctx.builder.numberAsBytesValueKey(ptr),
            });
        },
        else => std.debug.panic("unhandled base type: {s}", .{@tagName(base_type.data)}),
    }
}
