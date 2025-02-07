const Utils = @import("./utils.zig");
const Signature = @import("./Signature.zig");
const std = @import("std");
const Namespace = @import("./Namespace.zig");
pub const Linkage = enum {
    Import,
    Local,
    Export,
};
namespace: Namespace.Ref,
name: []const u8,
linkage: Linkage,
signature: Signature.Ref,
allocator: std.mem.Allocator,

const Self = @This();
pub fn init(
    allocator: std.mem.Allocator,
    namespace: Namespace.Ref,
    name: []const u8,
    linkage: Linkage,
    signature: Signature.Ref,
) !Self {
    return .{
        .namespace = namespace,
        .allocator = allocator,
        .name = try allocator.dupe(u8, name),
        .linkage = linkage,
        .signature = signature,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.name);
}
