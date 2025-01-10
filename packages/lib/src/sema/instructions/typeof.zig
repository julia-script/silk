const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const operand_index = scope.getInstructionIndex(hir_inst.typeof.operand);
    const operand_inst = scope.getInstruction(operand_index);
    return ctx.pushInstruction(hir_inst_index, .{
        .op = .typeof,
        .typed_value = .{
            .type = Sema.Type.simple(.type),
            .value = try ctx.builder.internValueData(.{ .type = operand_inst.typed_value.type }),
        },
        .data = .{ .operand = operand_index },
    });
}

pub fn exec(b: *InstContext, inst_index: Sema.Instruction.Index) !void {
    _ = b; // autofix
    _ = inst_index; // autofix
    // @panic("not implemented");
}
