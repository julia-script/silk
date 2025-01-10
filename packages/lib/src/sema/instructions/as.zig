const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const lhs_inst_index = scope.getInstructionIndex(hir_inst.as.lhs);
    const rhs_inst_index = scope.getInstructionIndex(hir_inst.as.rhs);
    const rhs_inst = scope.getInstruction(rhs_inst_index);

    scope.markDeadIfComptimeKnown(rhs_inst_index);
    scope.markDeadIfComptimeKnown(lhs_inst_index);
    const rhs_type = scope.builder.unwrapTypeValue(rhs_inst.typed_value.value);

    return ctx.pushCastInstruction(hir_inst_index, lhs_inst_index, rhs_type);
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const value_inst_value = ctx.getTypedValue(inst.data.operand);
    if (!value_inst_value.isComptimeKnown()) @panic("not comptime known");
    const value = try ctx.builder.convertNumberType(value_inst_value, inst.typed_value.type);
    ctx.setValue(inst_index, .{
        .type = inst.typed_value.type,
        .value = value.value,
    });
}
const ErrorManager = @import("../../ErrorManager.zig");
const TreeWriter = @import("../../TreeWriter.zig");
test "instas" {
    const allocator = std.testing.allocator;
    var errors_manager = try ErrorManager.init(allocator);
    defer errors_manager.deinit();
    const source =
        \\ pub fn main() void {
        \\   const a:i32 = 1
        \\ }
        \\
    ;

    var sema = try Sema.init(allocator, &errors_manager, .{});
    defer sema.deinit();
    const root = try sema.makeRootSource(source, "root.sk");
    try sema.compileAll(root);
    const stderr = std.io.getStdErr().writer().any();
    // var tree_writer = TreeWriter.init(stderr);
    try sema.formatDeclaration(
        stderr,
        // &tree_writer,
        // sema.instructions.items,
        0,
    );
}
