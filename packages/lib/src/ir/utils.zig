const std = @import("std");
const activeTag = std.meta.activeTag;

pub fn MakeRef(label: anytype, comptime T: type) type {
    _ = T; // autofix
    return struct {
        ref: u32,
        pub const Ref = @This();
        pub fn List(comptime K: type) type {
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
        pub fn from(value: u32) @This() {
            return .{ .ref = value };
        }

        pub fn format(
            self: @This(),
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: std.io.AnyWriter,
        ) !void {
            try writer.print("{s}#{d}", .{ @tagName(label), self.ref });
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
