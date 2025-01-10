const InstContext = @import("./InstContext.zig");
const Sema = @import("../Sema.zig");

pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const operand = inst.data.operand;
    const operand_inst = ctx.getInstruction(operand);
    const operand_typed_value = operand_inst.typed_value;
    // const operand_type = ctx.builder.getFormattableTypedValue(operand_typed_value);
    ctx.setValue(
        inst_index,
        try ctx.maybeCoerceValue(operand_typed_value, inst.typed_value.type),
    );

    // @panic("not implemented");
}
