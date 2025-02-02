const std = @import("std");
const Sema = @import("../sema/Sema.zig");
const Program = @import("../program/Program.zig");
const Self = @This();

context: *anyopaque,
program: *Program,
emitDeclarationFn: *const fn (context: *anyopaque, declaration: Sema.Declaration.Index) anyerror!usize,
deinitFn: *const fn (context: *anyopaque) void,
dumpFn: *const fn (context: *anyopaque) void,
// emitInstructionFn: *const fn (context: *anyopaque, instruction: Sema.Instruction.Index) anyerror!void,

pub fn emitDeclaration(self: *Self, declaration_index: Sema.Declaration.Index) !usize {
    const declaration = self.program.getDeclaration(declaration_index);
    std.debug.print("declaration: {}\n", .{declaration});

    return self.emitDeclarationFn(self.context, declaration_index);
}
pub fn deinit(self: *Self) void {
    self.deinitFn(self.context);
}

pub fn dump(self: *Self) void {
    self.dumpFn(self.context);
}

// pub fn getInstruction(self: *Self, instruction_id: Sema.Instruction.Index) !Sema.Instruction {
//     return self.getInstructionFn(self.context, instruction_id);
// }
