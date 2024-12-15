const std = @import("std");
const Allocator = std.mem.Allocator;
const HashMap = std.AutoArrayHashMapUnmanaged;

const ArrayUnmanaged = std.ArrayListUnmanaged;
const Array = std.ArrayList;

pub fn InternedLists(T: type) type {
    return struct {
        lists: ArrayUnmanaged(T) = .{},
        interned_map: HashMap(u64, Range) = .{},
        allocator: Allocator,
        const Self = @This();
        pub const Range = struct {
            start: u32,
            len: u32,
            pub const empty = Range{ .start = 0, .len = 0 };
        };
        pub const WorkingList = struct {
            list: Array(T),
            parent: *Self,
            pub fn slice(self: *WorkingList) []const T {
                return self.list.items;
            }
            pub fn len(self: *WorkingList) usize {
                return self.list.items.len;
            }
            pub fn commit(self: *WorkingList) !Range {
                defer self.deinit();
                if (self.list.items.len == 0) {
                    return Range.empty;
                }
                const hash = std.hash.Wyhash.hash(0, std.mem.sliceAsBytes(self.list.items));
                const existing = try self.parent.interned_map.getOrPut(self.parent.allocator, hash);
                if (existing.found_existing) {
                    return existing.value_ptr.*;
                }
                existing.value_ptr.* = .{
                    .start = @intCast(self.parent.lists.items.len),
                    .len = @intCast(self.list.items.len),
                };

                try self.parent.lists.appendSlice(self.parent.allocator, self.list.items);
                return existing.value_ptr.*;
            }
            pub fn append(self: *WorkingList, item: T) !void {
                try self.list.append(item);
            }
            pub fn appendSlice(self: *WorkingList, items: []const T) !void {
                try self.list.appendSlice(items);
            }

            pub fn deinit(self: *WorkingList) void {
                self.list.deinit();
            }
        };

        pub fn init(allocator: Allocator) Self {
            return .{
                .allocator = allocator,
            };
        }
        pub fn new(self: *Self) WorkingList {
            return .{
                .list = Array(T).init(self.allocator),
                .parent = self,
            };
        }
        pub fn getSlice(self: *Self, range: Range) []T {
            const start: usize = @intCast(range.start);
            const len: usize = @intCast(range.len);
            if (len == 0) {
                return &.{};
            }
            return self.lists.items[start .. start + len];
        }
        pub fn deinit(self: *Self) void {
            self.lists.deinit(self.allocator);
            self.interned_map.deinit(self.allocator);
        }
    };
}

const expectEqualStrings = std.testing.expectEqualStrings;
const expectEqual = std.testing.expectEqual;
test "InternedLists" {
    const StringsList = InternedLists(u8);
    var lists = StringsList.init(std.testing.allocator);
    defer lists.deinit();
    var list_a = lists.new();
    try list_a.appendSlice("hello ");
    try list_a.appendSlice("world");
    const range = try list_a.commit();
    const slice = lists.getSlice(range);
    try expectEqualStrings(slice, "hello world");

    var list_b = lists.new();
    try list_b.appendSlice("hello ");
    try list_b.appendSlice("world");
    const range2 = try list_b.commit();
    const slice2 = lists.getSlice(range2);

    try expectEqualStrings(slice2, "hello world");
    try expectEqual(range.start, range2.start);
    try expectEqual(range.len, range2.len);

    var list_c = lists.new();
    try list_c.appendSlice("hello ");
    try list_c.appendSlice("universe");
    const range3 = try list_c.commit();
    const slice3 = lists.getSlice(range3);
    try expectEqualStrings(slice3, "hello universe");
}
