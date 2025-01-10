const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const index = ctx.pushInstruction(hir_inst_index, .{
        .op = .ret,
        .typed_value = Sema.TypedValue.VOID,
        .data = .{
            .maybe_operand = if (hir_inst.ret.operand) |operand| scope.getInstructionIndex(operand) else null,
        },
    });
    if (ctx.getInstruction(index).data.maybe_operand) |operand_inst_index| {
        const operand_inst_value = ctx.getTypedValue(operand_inst_index);
        ctx.setValue(0, operand_inst_value);
    }
    if (ctx.is_comptime) {
        ctx.active_node = null;
    }

    return index;
}

pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    if (inst.data.maybe_operand) |operand_inst_index| {
        const operand_inst_value = ctx.getTypedValue(operand_inst_index);
        ctx.setValue(0, operand_inst_value);
    }
    ctx.active_node = null;
}
