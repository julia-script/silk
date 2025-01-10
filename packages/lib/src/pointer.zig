const std = @import("std");

pub fn Box(comptime T: type) type {
    return struct {
        ptr: *T,
        allocator: std.mem.Allocator,
        const Self = @This();
        pub fn init(allocator: std.mem.Allocator) !Self {
            const ptr = try allocator.create(T);
            return Self{ .ptr = ptr, .allocator = allocator };
        }
        pub fn deinit(self: *Self) void {
            self.allocator.destroy(self);
        }
    };
}
