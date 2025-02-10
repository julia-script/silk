const Ty = @import("./ty.zig").Ty;
const utils = @import("./utils.zig");
const Function = @import("./Function.zig");
const std = @import("std");
const FunctionDeclaration = @import("./FunctionDeclaration.zig");
const Set = @import("../data_structures.zig").AutoSet;
const Module = @import("./Module.zig");

const Self = @This();
pub const Ref = utils.MakeRef(.namespace, Self, "ns{d}");

name: []const u8,
ty: ?Ty,
declarations: Set(Module.Decl.Ref),
allocator: std.mem.Allocator,

pub fn init(allocator: std.mem.Allocator, name: []const u8) !Self {
    return .{
        .name = try allocator.dupe(u8, name),
        .ty = null,
        .declarations = Set(Module.Decl.Ref).init(allocator),
        .allocator = allocator,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.name);
}
