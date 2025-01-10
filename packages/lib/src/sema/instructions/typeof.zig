const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const operand_index = scope.getInstructionIndex(hir_inst.typeof.operand);
    const operand_inst = ctx.getInstruction(operand_index);
    _ = operand_inst; // autofix
    const operand_typed_value = ctx.getTypedValue(operand_index);
    const typed_value = Sema.TypedValue{
        .type = Sema.Type.simple(.type),
        .value = try ctx.builder.internValueData(.{ .type = operand_typed_value.type }),
    };

    const index = ctx.pushInstruction(hir_inst_index, .{
        .op = .typeof,
        .typed_value = typed_value,
        .data = .{ .operand = operand_index },
    });
    std.debug.print("%{d}: typeof %{d} {}\n", .{ index, operand_index, ctx.builder.getFormattableTypedValue(typed_value) });
    return index;
}

pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    _ = ctx; // autofix
    _ = inst_index; // autofix
    // @panic("not implemented");
    // const inst = ctx.getInstruction(inst_index);
    // const operand_typed_value = ctx.getTypedValue(inst.data.operand);
    // ctx.setValue(inst_index, operand_typed_value);
    // ctx.setValue(inst.data.operand, inst.typed_value);
}
