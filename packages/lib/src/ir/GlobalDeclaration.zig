const std = @import("std");
const Self = @This();
const Ty = @import("./tyval.zig").Ty;
const Namespace = @import("./Namespace.zig");

name: []const u8,
namespace: Namespace.Ref,
ty: Ty,
allocator: std.mem.Allocator,
pub fn init(
    allocator: std.mem.Allocator,
    namespace: Namespace.Ref,
    name: []const u8,
    ty: Ty,
) !Self {
    return .{
        .namespace = namespace,
        .allocator = allocator,
        .name = try allocator.dupe(u8, name),
        .ty = ty,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.name);
}
