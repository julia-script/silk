const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const pointer_inst_index = scope.getInstructionIndex(hir_inst.load.operand);
    const pointer_inst = ctx.getInstruction(pointer_inst_index);
    const type_to_load = ctx.builder.unwrapPointerType(pointer_inst.typed_value.type) orelse {
        std.debug.panic("expected pointer type, got {any}", .{ctx.builder.getFormattableType(pointer_inst.typed_value.type)});
    };
    // switch (type_to_load) {
    //     .complex => |complex| {
    //         _ = complex; // autofix
    //         // switch (ctx.builder.getComplexType(complex).data) {
    //         //     .@"struct" => {
    //         const value = if (pointer_inst.typed_value.isComptimeKnown())
    //             try ctx.builder.sema.memory.stackDupe(pointer_inst.typed_value)
    //         else
    //             Sema.TypedValue{
    //                 .type = pointer_inst.typed_value.type,
    //                 .value = Sema.Value.simple(.exec_time),
    //             };

    //         const index = ctx.pushInstruction(hir_inst_index, .{
    //             .op = .memdupe,
    //             .typed_value = value,
    //             .data = .{ .operand = pointer_inst_index },
    //         });
    //         // // try maybeInline(ctx, index);
    //         // _ = ctx.pushInstruction(null, .{
    //         //     .op = .memcpy,
    //         //     .typed_value = Sema.TypedValue.VOID,
    //         //     .data = .{ .memcpy = .{
    //         //         .dest = index,
    //         //         .src = pointer_inst_index,
    //         //     } },
    //         // });
    //         return index;
    //         // },
    //         // else => {},
    //     },
    //     .simple => {},
    // }
    const index = ctx.pushInstruction(hir_inst_index, .{
        .op = .load,
        .typed_value = .{
            .type = type_to_load,
            .value = Sema.Value.simple(.exec_time),
        },
        .data = .{ .operand = pointer_inst_index },
    });
    if (pointer_inst.typed_value.isComptimeKnown()) try maybeInline(ctx, index);
    return index;
}
pub fn maybeInline(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const ptr_inst_value = ctx.getTypedValue(inst.data.operand);
    if (!ptr_inst_value.isComptimeKnown()) return;

    const ptr = try ctx.builder.readNumberAsType(usize, ptr_inst_value);

    const type_to_load = ctx.builder.unwrapPointerType(ptr_inst_value.type) orelse std.debug.panic("not a pointer type", .{});
    ctx.setValue(inst_index, .{
        .type = inst.typed_value.type,
        .value = switch (type_to_load) {
            .simple => try ctx.builder.sema.memory.load(type_to_load, ptr),
            .complex => ptr_inst_value.value,
        },
    });
}

pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    var ptr_inst_value = ctx.getTypedValue(inst.data.operand);
    if (!ptr_inst_value.isComptimeKnown()) @panic("not a comptime known value");
    const ptr = try ctx.builder.readNumberAsType(usize, ptr_inst_value);

    const type_to_load = ctx.builder.unwrapPointerType(ptr_inst_value.type) orelse std.debug.panic("not a pointer type", .{});
    const loaded = try ctx.builder.sema.memory.load(type_to_load, ptr);
    ctx.setValue(inst_index, .{
        .type = inst.typed_value.type,
        .value = loaded,
    });
}
