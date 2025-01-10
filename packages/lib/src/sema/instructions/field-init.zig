const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const field_name = try scope.entity.internNode(hir_inst.field_init.name_node);
    const field_value_inst_index = scope.getInstructionIndex(hir_inst.field_init.value);
    // const field_value_inst = self.getInstruction(field_value_inst_index);
    return ctx.pushInstruction(hir_inst_index, .{
        .op = .field_init,
        // .type = field_value_inst.type,
        .typed_value = Sema.TypedValue.VOID,
        .data = .{
            .field_init = .{
                .field_name = field_name,
                .value_inst = field_value_inst_index,
            },
        },
    });
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    _ = ctx; // autofix
    _ = inst_index; // autofix
}
