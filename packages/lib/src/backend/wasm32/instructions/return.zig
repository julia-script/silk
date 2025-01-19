const std = @import("std");
const Scope = @import("./Scope.zig");
const Sema = @import("../../../sema/Sema.zig");

pub fn emit(scope: *Scope, _: Sema.Instruction.Index) !void {
    try scope.function.pushInstruction(.@"return");
}
