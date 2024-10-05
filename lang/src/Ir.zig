const std = @import("std");
const Array = std.ArrayListUnmanaged;
const Ast = @import("Ast.zig");

const Ir = @This();

inst: Array(Inst),
ast: *Ast,
allocator: std.mem.Allocator,

const Inst = struct {
    args: Array(u32),
};
pub fn init(allocator: std.mem.Allocator, ast: *Ast) !Ir {
    return .{
        .allocator = allocator,
        .ast = ast,
        .inst = try Array(Inst).initCapacity(allocator, 0),
    };
}
