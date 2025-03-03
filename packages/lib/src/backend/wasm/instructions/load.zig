const std = @import("std");
const Scope = @import("./Scope.zig");
const Sema = @import("../../../sema/Sema.zig");

pub fn emit(scope: *Scope, index: Sema.Instruction.Index) !void {
    const instruction = scope.getInstruction(index);
    const local = scope.getLocal(instruction.data.operand);
    try scope.function.pushInstruction(.{ .@"local.get" = local.index });
}
