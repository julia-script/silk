const std = @import("std");

pub fn Set(comptime T: type, comptime Context: type, comptime max_load_percentage: u64) type {
    return struct {
        hashmap: std.HashMap(T, void, Context, max_load_percentage),
        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{ .hashmap = std.HashMap(T, void, Context, max_load_percentage).init(allocator) };
        }

        pub fn deinit(self: *Self) void {
            self.hashmap.deinit();
        }

        pub fn insert(self: *Self, value: T) !void {
            try self.hashmap.put(value, void);
        }

        pub fn contains(self: Self, value: T) bool {
            return self.hashmap.contains(value);
        }

        pub fn remove(self: *Self, value: T) bool {
            return self.hashmap.remove(value);
        }

        pub fn count(self: Self) usize {
            return self.hashmap.count();
        }
    };
}

pub fn AutoSet(comptime T: type) type {
    return Set(T, std.hash_map.AutoContext(T), std.hash_map.default_max_load_percentage);
}

pub fn SetUnmanaged(comptime T: type, comptime Context: type, comptime max_load_percentage: u64) type {
    return struct {
        hashmap: std.HashMapUnmanaged(T, void, Context, max_load_percentage) = .{},
        const Self = @This();

        pub fn init() Self {
            return .{};
        }

        pub fn deinit(self: *Self, allocator: std.mem.Allocator) void {
            self.hashmap.deinit(allocator);
        }

        pub fn insert(self: *Self, allocator: std.mem.Allocator, value: T) !void {
            try self.hashmap.put(allocator, value, {});
        }

        pub fn contains(self: Self, value: T) bool {
            return self.hashmap.contains(value);
        }

        pub fn remove(self: *Self, value: T) bool {
            return self.hashmap.remove(value);
        }

        pub fn count(self: Self) usize {
            return self.hashmap.count();
        }
    };
}

pub fn AutoSetUnmanaged(comptime T: type) type {
    return SetUnmanaged(T, std.hash_map.AutoContext(T), std.hash_map.default_max_load_percentage);
}
