const Utils = @import("./utils.zig");
const Signature = @import("./Signature.zig");
const std = @import("std");
const Namespace = @import("./Namespace.zig");
const Module = @import("./Module.zig");
const TypedValue = Module.TypedValue;
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
ty: Module.Ty,

const Self = @This();
pub fn init(
    allocator: std.mem.Allocator,
    namespace: Namespace.Ref,
    name: []const u8,
    linkage: Linkage,
    signature: Signature.Ref,
    ty: Module.Ty,
) !Self {
    return .{
        .namespace = namespace,
        .allocator = allocator,
        .name = try allocator.dupe(u8, name),
        .linkage = linkage,
        .signature = signature,
        .ty = ty,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.name);
}
