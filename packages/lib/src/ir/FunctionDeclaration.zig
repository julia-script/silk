const Utils = @import("./utils.zig");
const Signature = @import("./Signature.zig");
const std = @import("std");
pub const Ref = Utils.MakeRef(.func_decl, u32);
pub const Linkage = enum {
    Import,
    Local,
    Export,
};
name: []const u8,
linkage: Linkage,
signature: Signature.Ref,
allocator: std.mem.Allocator,

const Self = @This();
pub fn init(allocator: std.mem.Allocator, name: []const u8, linkage: Linkage, signature: Signature.Ref) !Self {
    return .{
        .allocator = allocator,
        .name = try allocator.dupe(u8, name),
        .linkage = linkage,
        .signature = signature,
    };
}

pub fn deinit(self: *Self) void {
    self.allocator.free(self.name);
}
