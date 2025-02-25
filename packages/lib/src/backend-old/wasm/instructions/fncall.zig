const std = @import("std");
const Scope = @import("./Scope.zig");
const Sema = @import("../../../sema/Sema.zig");

pub fn emit(scope: *Scope, index: Sema.Instruction.Index) !void {
    const instruction = scope.getInstruction(index);
    // const local = scope.getLocal(instruction.data.operand);
    // const callee_inst = scope.getInstruction(instruction.data.operand);
    // const decl = scope.program.sema.getDeclaration(instruction.data.fn_call.callee_declaration);

    // const decl =  scope.backend.declarations.get()
    // const existing = scope.backend.declarations.get(instruction.data.fn_call.callee_declaration);
    // if (existing) |decl| {
    //     try scope.function.pushInstruction(.{ .call = @intCast(decl.function) });
    //     return;
    // }

    const decl = try scope.backend.emitDeclaration(instruction.data.fn_call.callee_declaration);
    try scope.function.pushInstruction(.{ .call = @intCast(decl) });

    // try scope.function.pushInstruction(.{ .call = decl.});
    // std.meta.declarations(comptime T: type)
    // @compileLog(std.meta.declarations(@TypeOf(scope.backend.*)));
}
