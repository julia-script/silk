const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const operand_inst_index = blk: {
        const operand_hir_inst = hir_inst.ret.operand orelse break :blk null;

        const operand_inst_index = scope.getInstructionIndex(operand_hir_inst);
        const operand_inst_value = ctx.getTypedValue(operand_inst_index);

        const ret_type = scope.return_type orelse break :blk operand_inst_index;
        std.debug.print("ret_type: {}, {}\n", .{ ctx.builder.getFormattableType(ret_type), ctx.builder.getFormattableTypedValue(operand_inst_value) });

        break :blk try ctx.pushMaybeCastInstructionToType(
            null,
            operand_inst_index,
            ret_type,
        ) orelse operand_inst_index;
    };
    const index = ctx.pushInstruction(hir_inst_index, .{
        .op = .ret,
        .typed_value = Sema.TypedValue.VOID,
        .data = .{
            .maybe_operand = operand_inst_index,
        },
    });
    if (operand_inst_index) |i| {
        const operand_inst_value = ctx.getTypedValue(i);
        ctx.setValue(0, operand_inst_value);
    }
    // if (ctx.getInstruction(index).data.maybe_operand) |operand_inst_index| {
    //     var operand_inst_index_ = operand_inst_index;
    //     if (scope.return_type) |return_type| {
    //         operand_inst_index_ = try ctx.pushMaybeCastInstructionToType(null, operand_inst_index, return_type) orelse operand_inst_index_;
    //     }
    //     const operand_inst_value = ctx.getTypedValue(operand_inst_index_);
    //     ctx.setValue(0, operand_inst_value);
    // }
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
