const std = @import("std");

const Module = @import("Module.zig");
const TypedValue = Module.TypedValue;
const Namespace = @import("Namespace.zig");
const Ty = @import("ty.zig").Ty;

const Self = @This();
name: []const u8,
parent_namespace: Namespace.Ref,
namespace: Namespace.Ref,
allocator: std.mem.Allocator,
ty: Module.Ty,

pub fn init(
    allocator: std.mem.Allocator,
    parent_namespace: Namespace.Ref,
    namespace: Namespace.Ref,
    name: []const u8,
    ty: Module.Ty,
) !Self {
    return .{
        .parent_namespace = parent_namespace,
        .namespace = namespace,
        .allocator = allocator,
        .name = try allocator.dupe(u8, name),
        .ty = ty,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.name);
}
