const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const ty = try scope.builder.internTypeData(.{ .builtin_global = hir_inst.builtin_global_get.builtin });

    return ctx.pushInstruction(hir_inst_index, .{
        .op = .constant,
        .typed_value = .{
            .type = ty,
            .value = try scope.builder.internValueData(.{ .type = ty }),
        },
        .data = .void,
    });
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    _ = ctx; // autofix
    _ = inst_index; // autofix
    @panic("not implemented");
}
