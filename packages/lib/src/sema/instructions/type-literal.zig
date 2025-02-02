const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    const type_value_index = switch (std.meta.activeTag(hir_inst)) {
        .ty_usize => block.ctx.builder.getPointerType(.unsigned),

        .ty_i8 => Sema.Type.simple(.i8),
        .ty_i16 => Sema.Type.simple(.i16),
        .ty_i32 => Sema.Type.simple(.i32),
        .ty_i64 => Sema.Type.simple(.i64),
        .ty_u8 => Sema.Type.simple(.u8),
        .ty_u16 => Sema.Type.simple(.u16),
        .ty_u32 => Sema.Type.simple(.u32),
        .ty_u64 => Sema.Type.simple(.u64),

        .ty_f64 => Sema.Type.simple(.f64),
        .ty_f32 => Sema.Type.simple(.f32),
        .ty_boolean => Sema.Type.simple(.bool),
        .ty_void => Sema.Type.simple(.void),

        .ty_array => ty: {
            const ty_array = hir_inst.ty_array;
            const type_inst_id = block.ctx.getInstructionByHirIndex(ty_array.type);
            const type_inst = block.ctx.getInstruction(type_inst_id);
            const uncasted_size_inst_id = block.ctx.getInstructionByHirIndex(ty_array.size);
            const size_inst_id = (try block.pushMaybeCastInstructionToType(
                ty_array.size,
                uncasted_size_inst_id,
                block.ctx.builder.getPointerType(.unsigned),
            )) orelse uncasted_size_inst_id;
            // const size_inst_id = try scope.getInstructionAsTypeByHirInst(ty_array.size, Sema.Type.simple(.usize));
            const size_inst = block.ctx.getInstruction(size_inst_id);
            // const size = size_inst.value;
            // const size_value = self.builder.getValue(size) orelse std.debug.panic("Error: size_value is not a number", .{});
            const size_int = try block.ctx.builder.readNumberAsType(usize, size_inst.typed_value);
            const type_value_index = type_inst.typed_value.value;
            block.ctx.markDead(type_inst_id);
            block.ctx.markDead(size_inst_id);

            const type_index = try block.ctx.builder.internTypeData(.{ .array = .{
                .child = block.ctx.builder.unwrapTypeValue(type_value_index),
                .len = size_int,
            } });

            break :ty type_index;
        },
        .ty_pointer => ty: {
            const ty_pointer = hir_inst.ty_pointer;
            const type_inst_id = block.ctx.getInstructionByHirIndex(ty_pointer.operand);
            const type_inst = block.ctx.getInstruction(type_inst_id);
            // const type_value_index = type_inst.value;
            const type_index = try block.ctx.builder.internTypeData(.{ .pointer = .{
                .child = block.ctx.builder.unwrapTypeValue(type_inst.typed_value.value),
            } });
            break :ty type_index;
        },
        else => std.debug.panic("unhandled type_literal: {s}", .{@tagName(hir_inst)}),
    };

    const value = try block.ctx.builder.internValueData(.{ .type = type_value_index });
    // std.debug.print("value: {} type {}\n", .{ value, type_value_index });
    return try block.appendInstruction(
        hir_inst_index,
        .{
            .op = .type,
            .typed_value = .{
                .value = value,
                .type = try block.ctx.builder.internTypeData(.{ .typeof = .{ .child = type_value_index } }),
            },
            .data = .void,
        },
    );
    // @panic("not implemented");
}

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const type_value_index = switch (std.meta.activeTag(hir_inst)) {
        .ty_usize => scope.builder.getPointerType(.unsigned),

        .ty_i8 => Sema.Type.simple(.i8),
        .ty_i16 => Sema.Type.simple(.i16),
        .ty_i32 => Sema.Type.simple(.i32),
        .ty_i64 => Sema.Type.simple(.i64),
        .ty_u8 => Sema.Type.simple(.u8),
        .ty_u16 => Sema.Type.simple(.u16),
        .ty_u32 => Sema.Type.simple(.u32),
        .ty_u64 => Sema.Type.simple(.u64),

        .ty_f64 => Sema.Type.simple(.f64),
        .ty_f32 => Sema.Type.simple(.f32),
        .ty_boolean => Sema.Type.simple(.bool),
        .ty_void => Sema.Type.simple(.void),

        .ty_array => ty: {
            const ty_array = hir_inst.ty_array;
            const type_inst_id = scope.getInstructionIndex(ty_array.type);
            const type_inst = ctx.getInstruction(type_inst_id);
            const uncasted_size_inst_id = scope.getInstructionIndex(ty_array.size);
            const size_inst_id = (try ctx.pushMaybeCastInstructionToType(
                ty_array.size,
                uncasted_size_inst_id,
                ctx.builder.getPointerType(.unsigned),
            )) orelse uncasted_size_inst_id;
            // const size_inst_id = try scope.getInstructionAsTypeByHirInst(ty_array.size, Sema.Type.simple(.usize));
            const size_inst = ctx.getInstruction(size_inst_id);
            // const size = size_inst.value;
            // const size_value = self.builder.getValue(size) orelse std.debug.panic("Error: size_value is not a number", .{});
            const size_int = try scope.builder.readNumberAsType(usize, size_inst.typed_value);
            const type_value_index = type_inst.typed_value.value;
            ctx.markDead(type_inst_id);
            ctx.markDead(size_inst_id);

            const type_index = try scope.builder.internTypeData(.{ .array = .{
                .child = scope.builder.unwrapTypeValue(type_value_index),
                .len = size_int,
            } });

            break :ty type_index;
        },
        .ty_pointer => ty: {
            const ty_pointer = hir_inst.ty_pointer;
            const type_inst_id = scope.getInstructionIndex(ty_pointer.operand);
            const type_inst = ctx.getInstruction(type_inst_id);
            // const type_value_index = type_inst.value;
            const type_index = try scope.builder.internTypeData(.{ .pointer = .{
                .child = scope.builder.unwrapTypeValue(type_inst.typed_value.value),
            } });
            break :ty type_index;
        },
        else => std.debug.panic("unhandled type_literal: {s}", .{@tagName(hir_inst)}),
    };

    const value = try scope.builder.internValueData(.{ .type = type_value_index });
    // std.debug.print("value: {} type {}\n", .{ value, type_value_index });
    return ctx.pushInstruction(
        hir_inst_index,
        .{
            .op = .type,
            .typed_value = .{
                .value = value,
                .type = try scope.builder.internTypeData(.{ .typeof = .{ .child = type_value_index } }),
            },
            .data = .void,
        },
    );
}
pub fn exec(block: *InstContext.Block, inst_index: Sema.Instruction.Index) !void {
    _ = block; // autofix
    _ = inst_index; // autofix
    // @panic("not implemented");
}
