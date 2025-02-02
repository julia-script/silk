const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");

pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    const operand_inst_index = blk: {
        const operand_hir_inst = hir_inst.ret.operand orelse break :blk null;

        const operand_inst_index = block.ctx.getInstructionByHirIndex(operand_hir_inst);
        const operand_inst_value = block.ctx.getTypedValue(operand_inst_index);

        const ret_type = try block.ctx.getReturnType();
        std.debug.print("ret_type: {}, {}\n", .{ block.ctx.builder.getFormattableType(ret_type), block.ctx.builder.getFormattableTypedValue(operand_inst_value) });

        break :blk try block.pushMaybeCastInstructionToType(
            null,
            operand_inst_index,
            ret_type,
        ) orelse operand_inst_index;
    };
    const index = try block.appendInstruction(hir_inst_index, .{
        .op = .ret,
        .typed_value = Sema.TypedValue.VOID,
        .data = .{
            .maybe_operand = operand_inst_index,
        },
    });

    var payload = Sema.TypedValue.VOID;
    if (operand_inst_index) |i| {
        block.ctx.markDeadIfComptimeKnown(i);
        payload = block.ctx.getTypedValue(i);
    }
    block.breakTo(block.ctx.root_hir_index, payload);
    // if (ctx.getInstruction(index).data.maybe_operand) |operand_inst_index| {
    //     var operand_inst_index_ = operand_inst_index;
    //     if (scope.return_type) |return_type| {
    //         operand_inst_index_ = try ctx.pushMaybeCastInstructionToType(null, operand_inst_index, return_type) orelse operand_inst_index_;
    //     }
    //     const operand_inst_value = ctx.getTypedValue(operand_inst_index_);
    //     ctx.setValue(0, operand_inst_value);
    // }
    // if (block.is_comptime) {
    //     block.ctx.active_node = null;
    // }

    return index;
}

const ExecContext = @import("./ExecContext.zig");
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    if (inst.data.maybe_operand) |operand_inst_index| {
        const operand_inst_value = ctx.getInstruction(operand_inst_index);
        try ctx.setValue(0, operand_inst_value.typed_value);
    }
    ctx.active_node = null;
}
