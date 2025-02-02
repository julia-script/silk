const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    const operand_index = block.ctx.getInstructionByHirIndex(hir_inst.typeof.operand);
    const operand_typed_value = block.ctx.getTypedValue(operand_index);
    const typed_value = Sema.TypedValue{
        .type = Sema.Type.simple(.type),
        .value = try block.ctx.builder.internValueData(.{ .type = operand_typed_value.type }),
    };

    const index = try block.appendInstruction(hir_inst_index, .{
        .op = .typeof,
        .typed_value = typed_value,
        .data = .{ .operand = operand_index },
    });
    return index;
}

pub fn exec(block: *InstContext.Block, inst_index: Sema.Instruction.Index) !void {
    _ = block; // autofix
    _ = inst_index; // autofix
    // @panic("not implemented");
    // const inst = ctx.getInstruction(inst_index);
    // const operand_typed_value = ctx.getTypedValue(inst.data.operand);
    // ctx.setValue(inst_index, operand_typed_value);
    // ctx.setValue(inst.data.operand, inst.typed_value);
}
