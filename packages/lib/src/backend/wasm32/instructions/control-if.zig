const std = @import("std");
const Scope = @import("./Scope.zig");
const Sema = @import("../../../sema/Sema.zig");

pub fn emit(scope: *Scope, index: Sema.Instruction.Index) !void {
    const instruction = scope.getInstruction(index);
    try scope.function.pushInstruction(.{ .@"if" = .empty });
    try scope.emit(instruction.data.@"if".then_block);
    if (instruction.data.@"if".else_block) |else_index| {
        try scope.emit(else_index);
    }
    try scope.function.pushInstruction(.end);
    _ = type;
}
