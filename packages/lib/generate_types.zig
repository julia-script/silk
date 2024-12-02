const std = @import("std");
const Ast = @import("src/Ast.zig");
const Hir = @import("src/Hir.zig");
const Mir = @import("src/Mir.zig");
const serializer = @import("src/serializer.zig");

pub fn main() !void {
    const file = try std.fs.cwd().createFile("src/types.ts", .{});
    defer file.close();
    const writer = file.writer();
    try serializer.writeTsType(Ast.Node, "AstNode", writer.any(), true);
    try serializer.writeTsType(Hir.Inst, "HirInstruction", writer.any(), true);
    try serializer.writeTsType(Mir.Instruction, "MirInstruction", writer.any(), true);
    try serializer.writeTsType(Mir.Type, "MirType", writer.any(), false);
    try serializer.writeTsType(Mir.Value, "MirValue", writer.any(), false);
}
