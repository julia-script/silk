const std = @import("std");
const activeTag = std.meta.activeTag;
const Module = @import("Module.zig");

pub fn MakeRef(label: anytype, comptime T: type, comptime maybe_format_str: ?[]const u8) type {
    _ = T; // autofix
    return struct {
        idx: u32,
        pub const Ref = @This();
        pub fn ListUnmanaged(comptime K: type) type {
            return struct {
                items: std.ArrayListUnmanaged(K) = .{},
                pub fn init() @This() {
                    return .{};
                }
                pub fn deinit(self: *@This(), allocator: std.mem.Allocator) void {
                    self.items.deinit(allocator);
                }
                pub fn deinitRecursive(self: *@This(), allocator: std.mem.Allocator) void {
                    if (@hasDecl(K, "deinit")) {
                        for (self.items.items) |*item| {
                            item.deinit(allocator);
                        }
                    }
                    self.deinit(allocator);
                }
                pub fn reserve(self: *@This(), allocator: std.mem.Allocator) !Ref {
                    return try self.append(allocator, undefined);
                }
                pub fn slice(self: @This()) []K {
                    return self.items.items;
                }
                const Entry = struct {
                    ref: Ref,
                    item: *K,
                };
                const Iter = struct {
                    items: []K,
                    i: u32,
                    pub fn next(self: *@This()) ?Entry {
                        if (self.i >= self.items.len) return null;
                        const i = self.i;
                        self.i += 1;
                        return .{ .ref = .{ .idx = i }, .item = &self.items[i] };
                    }
                };
                pub fn iter(self: @This()) Iter {
                    return .{ .items = self.items.items, .i = 0 };
                }
                pub fn append(self: *@This(), allocator: std.mem.Allocator, item: K) !Ref {
                    const idx = self.items.items.len;
                    try self.items.append(allocator, item);
                    return .{ .idx = @intCast(idx) };
                }
                pub fn get(self: @This(), ref: Ref) K {
                    return self.items.items[@intCast(ref.idx)];
                }
                pub fn getPtr(self: @This(), ref: Ref) *K {
                    return &self.items.items[@intCast(ref.idx)];
                }
                pub fn set(self: *@This(), ref: Ref, item: K) void {
                    self.items.items[@intCast(ref.idx)] = item;
                }
                pub fn count(self: @This()) usize {
                    return self.items.items.len;
                }
            };
        }
        pub fn _List(comptime K: type) type {
            return struct {
                items: std.ArrayList(K),
                pub fn init(allocator: std.mem.Allocator) @This() {
                    return .{ .items = std.ArrayList(K).init(allocator) };
                }
                pub fn deinit(self: *@This()) void {
                    self.items.deinit();
                }
                pub fn deinitRecursive(self: *@This()) void {
                    if (@hasDecl(K, "deinit")) {
                        for (self.items.items) |*item| {
                            item.deinit();
                        }
                    }
                    self.deinit();
                }
                pub fn reserve(self: *@This()) !Ref {
                    return try self.append(undefined);
                }
                pub fn slice(self: @This()) []K {
                    return self.items.items;
                }
                const Entry = struct {
                    ref: Ref,
                    item: *K,
                };
                const Iter = struct {
                    items: []K,
                    i: u32,
                    pub fn next(self: *@This()) ?Entry {
                        if (self.i >= self.items.len) return null;
                        const i = self.i;
                        self.i += 1;
                        return .{ .ref = .{ .ref = i }, .item = &self.items[i] };
                    }
                };
                pub fn iter(self: @This()) Iter {
                    return .{ .items = self.items.items, .i = 0 };
                }
                pub fn append(self: *@This(), item: K) !Ref {
                    const ref = self.items.items.len;
                    try self.items.append(item);
                    return .{ .ref = @intCast(ref) };
                }
                pub fn get(self: @This(), ref: Ref) K {
                    return self.items.items[@intCast(ref.ref)];
                }
                pub fn getPtr(self: @This(), ref: Ref) *K {
                    return &self.items.items[@intCast(ref.ref)];
                }
                pub fn set(self: *@This(), ref: Ref, item: K) void {
                    self.items.items[@intCast(ref.ref)] = item;
                }
                pub fn count(self: @This()) usize {
                    return self.items.items.len;
                }
            };
        }
        pub fn ArrayMap(V: type) type {
            return std.AutoArrayHashMap(Ref, V);
        }
        pub fn Map(V: type) type {
            return std.AutoHashMap(Ref, V);
        }
        pub fn MapUnmanaged(V: type) type {
            return std.AutoHashMapUnmanaged(Ref, V);
        }
        pub fn from(value: u32) @This() {
            return .{ .idx = value };
        }

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: std.io.AnyWriter,
        ) !void {
            if (maybe_format_str) |format_str| {
                try writer.print(format_str, .{self.idx});
            } else {
                try writer.print("{s}#{d}", .{ @tagName(label), self.idx });
            }
        }
    };
}

test "MakeRef" {
    const A = MakeRef(.a, u32);
    const B = MakeRef(.b, u32);
    const AnotherA = MakeRef(.a, u32);
    try testing.assertFormatsAs(A.from(1), "a#1");
    try testing.assertFormatsAs(B.from(2), "b#2");
    try std.testing.expect(A != B);
    try std.testing.expect(A == AnotherA);
}
// pub fn List(comptime K: type) type {
//     return struct {
//         items: std.ArrayList(K),
//         pub fn init(allocator: std.mem.Allocator) @This() {
//             return .{ .items = std.ArrayList(K).init(allocator) };
//         }
//         pub fn deinit(self: *@This()) void {
//             self.items.deinit();
//         }
//         pub fn deinitRecursive(self: *@This()) void {
//             if (@hasDecl(K, "deinit")) {
//                 for (self.items.items) |*item| {
//                     item.deinit();
//                 }
//             }
//             self.deinit();
//         }
//         pub fn reserve(self: *@This()) !Ref {
//             return try self.append(undefined);
//         }
//         pub fn slice(self: @This()) []K {
//             return self.items.items;
//         }
//         const Entry = struct {
//             ref: Ref,
//             item: *K,
//         };
//         const Iter = struct {
//             items: []K,
//             i: u32,
//             pub fn next(self: *@This()) ?Entry {
//                 if (self.i >= self.items.len) return null;
//                 const i = self.i;
//                 self.i += 1;
//                 return .{ .ref = .{ .ref = i }, .item = &self.items[i] };
//             }
//         };
//         pub fn iter(self: @This()) Iter {
//             return .{ .items = self.items.items, .i = 0 };
//         }
//         pub fn append(self: *@This(), item: K) !Ref {
//             const ref = self.items.items.len;
//             try self.items.append(item);
//             return .{ .ref = @intCast(ref) };
//         }
//         pub fn get(self: @This(), ref: Ref) K {
//             return self.items.items[@intCast(ref.ref)];
//         }
//         pub fn getPtr(self: @This(), ref: Ref) *K {
//             return &self.items.items[@intCast(ref.ref)];
//         }
//         pub fn set(self: *@This(), ref: Ref, item: K) void {
//             self.items.items[@intCast(ref.ref)] = item;
//         }
//         pub fn len(self: @This()) usize {
//             return self.items.items.len;
//         }
//     };
// }
pub const testing = struct {
    pub fn assertFormatsAs(value: anytype, comptime expected: []const u8) !void {
        var buf: [expected.len * 2]u8 = undefined;
        var fbo = std.io.fixedBufferStream(&buf);
        const writer = fbo.writer();
        try writer.print("{}", .{value});
        const actual = fbo.getWritten();
        return try std.testing.expectEqualStrings(actual, expected);
    }
};

// fn displayFn(self: Self, writer: std.io.AnyWriter, module: *const Module) anyerror!void {
//     _ = module; // autofix
//     try writer.print("{}", .{self});
// }
// pub fn display(self: Self, module: *const Module) Module.utils.MakeDisplay(Self, displayFn) {
//     return .{ .value = &self, .module = module };
// }

pub fn MakeDisplay(comptime T: type, comptime display_fn: fn (
    T,
    std.io.AnyWriter,
    *const Module,
) anyerror!void) type {
    return struct {
        value: T,
        module: *const Module,
        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: std.io.AnyWriter,
        ) !void {
            try display_fn(self.value, writer, self.module);
        }
    };
}
