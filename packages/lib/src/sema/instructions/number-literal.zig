const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const slice = scope.entity.getHir().ast.getNodeSlice(hir_inst.number_literal.node);
    const is_float = std.mem.indexOf(u8, slice, ".") != null;

    var bytes: [8]u8 = undefined;
    if (is_float) {
        const f = try std.fmt.parseFloat(f64, slice);
        std.mem.copyForwards(u8, &bytes, std.mem.asBytes(&f));
    } else {
        const i = try std.fmt.parseInt(i64, slice, 10);
        std.mem.copyForwards(u8, &bytes, std.mem.asBytes(&i));
    }

    const value_index = try ctx.builder.internValueData(.{ .bytes = bytes });
    return ctx.pushInstruction(hir_inst_index, .{
        .op = .constant,
        .typed_value = .{
            .type = if (is_float) Sema.Type.simple(.float) else Sema.Type.simple(.int),
            .value = value_index,
        },
        .data = .void,
    });
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    // @panic("not implemented");
    const inst = ctx.getInstruction(inst_index);
    ctx.setValue(inst_index, inst.typed_value);
}
