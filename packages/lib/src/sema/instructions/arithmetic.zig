const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");

pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    const op: Sema.Instruction.Op, const bin_op = switch (hir_inst) {
        .add => .{ .add, hir_inst.add },
        .sub => .{ .sub, hir_inst.sub },
        .mul => .{ .mul, hir_inst.mul },
        .div => .{ .div, hir_inst.div },
        else => unreachable,
    };

    var lhs_index = block.ctx.getInstructionByHirIndex(bin_op.lhs);
    var rhs_index = block.ctx.getInstructionByHirIndex(bin_op.rhs);
    const lhs_inst = block.ctx.getInstruction(lhs_index);
    const rhs_inst = block.ctx.getInstruction(rhs_index);

    const lhs_is_float = lhs_inst.typed_value.type.isEqualSimple(.float);
    const lhs_is_int = lhs_inst.typed_value.type.isEqualSimple(.int);
    const rhs_is_float = rhs_inst.typed_value.type.isEqualSimple(.float);
    const rhs_is_int = rhs_inst.typed_value.type.isEqualSimple(.int);
    const lhs_is_number = lhs_is_float or lhs_is_int;
    const rhs_is_number = rhs_is_float or rhs_is_int;

    if (lhs_is_number and !rhs_is_number) {
        const index = try block.appendInstruction(hir_inst_index, .{
            .op = op,
            .typed_value = .{
                .type = rhs_inst.typed_value.type,
                .value = Sema.Value.simple(.exec_time),
            },
            .data = .{ .bin_op = .{
                .lhs = try block.pushCastInstruction(hir_inst_index, lhs_index, rhs_inst.typed_value.type),
                .rhs = rhs_index,
            } },
        });
        try maybeInline(block, index);
        return index;
    } else if (rhs_is_number and !lhs_is_number) {
        const index = try block.appendInstruction(hir_inst_index, .{
            .op = op,
            .typed_value = .{
                .type = lhs_inst.typed_value.type,
                .value = Sema.Value.simple(.exec_time),
            },
            .data = .{ .bin_op = .{
                .lhs = lhs_index,
                .rhs = try block.pushCastInstruction(hir_inst_index, rhs_index, lhs_inst.typed_value.type),
            } },
        });
        try maybeInline(block, index);
        return index;
    }
    // float has higher priority than int
    if (lhs_is_float and rhs_is_int) {
        const index = try block.appendInstruction(hir_inst_index, .{
            .op = op,
            .typed_value = .{
                .type = lhs_inst.typed_value.type,
                .value = Sema.Value.simple(.exec_time),
            },
            .data = .{ .bin_op = .{
                .lhs = lhs_index,
                .rhs = try block.pushCastInstruction(hir_inst_index, rhs_index, lhs_inst.typed_value.type),
            } },
        });
        try maybeInline(block, index);
        return index;
    } else if (lhs_is_int and rhs_is_float) {
        const index = try block.appendInstruction(hir_inst_index, .{
            .op = op,
            .typed_value = .{
                .type = rhs_inst.typed_value.type,
                .value = Sema.Value.simple(.exec_time),
            },
            .data = .{ .bin_op = .{
                .lhs = try block.pushCastInstruction(hir_inst_index, lhs_index, rhs_inst.typed_value.type),
                .rhs = rhs_index,
            } },
        });
        try maybeInline(block, index);
        return index;
    }

    const lhs_is_signed = block.ctx.builder.isSigned(lhs_inst.typed_value.type);
    const rhs_is_signed = block.ctx.builder.isSigned(rhs_inst.typed_value.type);

    if (lhs_is_signed != rhs_is_signed) {
        std.debug.panic("error: arithmetic operands have different signedness", .{});
    }
    const lhs_bits = block.ctx.builder.numberBits(lhs_inst.typed_value.type);
    const rhs_bits = block.ctx.builder.numberBits(rhs_inst.typed_value.type);

    var ty = lhs_inst.typed_value.type;

    if (lhs_bits > rhs_bits) {
        ty = lhs_inst.typed_value.type;
        rhs_index = try block.pushCastInstruction(hir_inst_index, rhs_index, lhs_inst.typed_value.type);
    } else if (rhs_bits > lhs_bits) {
        ty = rhs_inst.typed_value.type;
        lhs_index = try block.pushCastInstruction(hir_inst_index, lhs_index, rhs_inst.typed_value.type);
    }

    const inst_index = try block.appendInstruction(hir_inst_index, .{
        .op = op,
        .typed_value = .{
            .type = ty,
            .value = Sema.Value.simple(.exec_time),
        },
        .data = .{ .bin_op = .{
            .lhs = lhs_index,
            .rhs = rhs_index,
        } },
    });
    try maybeInline(block, inst_index);
    return inst_index;
}

fn maybeInline(block: *InstContext.Block, inst_index: Sema.Instruction.Index) !void {
    const inst = block.ctx.getInstruction(inst_index);
    const lhs_inst_value = block.ctx.getTypedValue(inst.data.bin_op.lhs);
    const rhs_inst_value = block.ctx.getTypedValue(inst.data.bin_op.rhs);
    if (!lhs_inst_value.isComptimeKnown()) return;
    if (!rhs_inst_value.isComptimeKnown()) return;
    block.ctx.markDead(inst.data.bin_op.lhs);
    block.ctx.markDead(inst.data.bin_op.rhs);

    const op = inst.op;
    switch (inst.typed_value.type.simple) {
        inline .float, .int, .number, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .bchar => |type_| {
            const result = try block.ctx.builder.doArithmetic(
                type_.getNativeType(),
                op,
                lhs_inst_value,
                rhs_inst_value,
            );
            block.ctx.setValue(inst_index, result);
        },
        else => unreachable,
    }
}

const ExecContext = @import("./ExecContext.zig");
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    var lhs_inst_value = ctx.getInstruction(inst.data.bin_op.lhs).typed_value;
    var rhs_inst_value = ctx.getInstruction(inst.data.bin_op.rhs).typed_value;
    if (!lhs_inst_value.isComptimeKnown()) @panic("lhs is not comptime known");
    if (!rhs_inst_value.isComptimeKnown()) @panic("rhs is not comptime known");

    const op = inst.op;
    switch (inst.typed_value.type.simple) {
        inline .float, .int, .number, .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .bchar => |type_| {
            const result = try ctx.builder.doArithmetic(
                type_.getNativeType(),
                op,
                lhs_inst_value,
                rhs_inst_value,
            );
            try ctx.setValue(inst_index, result);
        },
        else => unreachable,
    }
}
