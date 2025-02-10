const std = @import("std");
const Self = @This();
const Ty = @import("./ty.zig").Ty;
const Namespace = @import("./Namespace.zig");
const Module = @import("./Module.zig");
const TypedValue = Module.TypedValue;

name: []const u8,
namespace: Namespace.Ref,
value: TypedValue,
allocator: std.mem.Allocator,
pub fn init(
    allocator: std.mem.Allocator,
    namespace: Namespace.Ref,
    name: []const u8,
    value: TypedValue,
) !Self {
    return .{
        .namespace = namespace,
        .allocator = allocator,
        .name = try allocator.dupe(u8, name),
        .value = value,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.name);
}
