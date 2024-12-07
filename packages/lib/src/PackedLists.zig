const std = @import("std");
const Array = std.ArrayListUnmanaged;
const Allocator = std.mem.Allocator;

pub fn new(comptime T: type, comptime sentinel: T) type {
    return struct {
        list: Array(T) = .{},

        pub const Index = usize;
        const Self = @This();

        pub const List = struct {
            list: Array(T),
            parent: *Self,
            allocator: Allocator,
            pub fn commit(self: *List) !usize {
                defer self.deinit();
                // var parentList = self.parent.list;
                var list = self.list;
                const index = self.parent.list.items.len;
                if (list.items.len == 0) {
                    return index;
                }
                try self.parent.list.ensureUnusedCapacity(self.allocator, list.items.len + 1);
                self.parent.list.appendAssumeCapacity(sentinel);
                self.parent.list.appendSliceAssumeCapacity(list.items[0..list.items.len]);
                return index + 1;
            }

            pub fn deinit(self: *List) void {
                self.list.deinit(self.allocator);
            }
            pub fn append(self: *List, item: T) !void {
                try self.list.append(self.allocator, item);
            }
            pub fn appendSlice(self: *List, items: []const T) !void {
                try self.list.appendSlice(self.allocator, items);
            }
            pub fn ensureUnusedCapacity(self: *List, capacity: usize) !void {
                try self.list.ensureUnusedCapacity(self.allocator, capacity);
            }
            pub fn ensureTotalCapacity(self: *List, capacity: usize) !void {
                try self.list.ensureTotalCapacity(self.allocator, capacity);
            }
            pub fn appendAssumeCapacity(self: *List, item: T) void {
                self.list.appendAssumeCapacity(item);
            }
            pub fn appendSliceAssumeCapacity(self: *List, items: []const T) void {
                self.list.appendSliceAssumeCapacity(items);
            }
        };

        pub const ListIter = struct {
            index: usize = 0,
            slice: []T,
            pub fn next(self: *ListIter) ?T {
                if (self.index >= self.slice.len) {
                    return null;
                }
                const item = self.slice[self.index];
                if (item == sentinel) {
                    return null;
                }
                self.index += 1;
                return item;
            }
            pub inline fn nextPtr(self: *ListIter) ?*const T {
                if (self.index >= self.slice.len) {
                    return null;
                }
                // const item = @constCast(&self.slice[self.index]);
                if (self.slice[self.index] == sentinel) {
                    return null;
                }
                return &self.slice[self.index];
            }
        };
        pub fn initCapacity(allocator: std.mem.Allocator, capacity: usize) !Self {
            return .{
                .list = try Array(T).initCapacity(allocator, capacity),
            };
        }
        pub fn slice(self: *Self) []T {
            return self.list.items[0..self.list.items.len];
        }
        pub fn deinit(self: *Self, allocator: Allocator) void {
            self.list.deinit(allocator);
        }
        pub fn new(self: *Self, allocator: Allocator) List {
            return .{
                .list = .{},
                .parent = self,
                .allocator = allocator,
            };
        }
        pub fn commitSlice(self: *Self, allocator: Allocator, slice_: []const T) !usize {
            var list = self.new(allocator);
            try list.appendSlice(slice_);
            return try list.commit();
        }
        // pub fn fromSlice(slice: []const T) Self {
        //     return .{
        //         .list = .{ .items = slice },
        //     };
        // }
        pub fn iterList(self: *Self, index: usize) ListIter {
            return .{
                .slice = self.slice()[index..],
            };
        }
    };
}

const expectEqualSlices = std.testing.expectEqualSlices;
const expectEqual = std.testing.expectEqual;
test "packed lists" {
    const test_allocator = std.testing.allocator;
    var packed_list = try new(i32, 0).initCapacity(test_allocator, 10);
    defer packed_list.deinit(test_allocator);
    var new_list = packed_list.new(test_allocator);

    try new_list.append(1);
    try new_list.append(2);
    try new_list.append(3);

    const index = try new_list.commit();
    // try expectEqual(index, 1);
    try expectEqualSlices(i32, &.{
        1,
        2,
        3,
    }, packed_list.slice()[index..]);

    var new_list2 = packed_list.new(test_allocator);
    try new_list2.append(4);
    try new_list2.append(5);
    try new_list2.append(6);

    const index2 = try new_list2.commit();
    try expectEqual(index2, 5);
    try expectEqualSlices(i32, &.{
        4,
        5,
        6,
    }, packed_list.slice()[index2..]);

    new_list = packed_list.new(test_allocator);
    const index_empty = try new_list.commit();
    try expectEqualSlices(i32, &.{}, packed_list.slice()[index_empty..]);
    new_list = packed_list.new(test_allocator);
    try new_list.append(7);
    try new_list.append(8);
    try new_list.append(9);
    const index3 = try new_list.commit();
    try expectEqualSlices(i32, &.{ 7, 8, 9 }, packed_list.slice()[index3..]);
    try expectEqualSlices(
        i32,
        &.{ 0, 1, 2, 3, 0, 4, 5, 6, 0, 7, 8, 9 },
        packed_list.slice(),
    );
    try expectEqualSlices(i32, &.{0}, packed_list.slice()[index_empty .. index_empty + 1]);
}

test "packed lists iter" {
    const test_allocator = std.testing.allocator;
    var packed_list = try new(i32, 0).initCapacity(test_allocator, 10);
    defer packed_list.deinit(test_allocator);
    var new_list = packed_list.new(test_allocator);

    try new_list.append(1);
    try new_list.append(2);
    try new_list.append(3);
    const index = try new_list.commit();
    new_list = packed_list.new(test_allocator);
    try new_list.append(4);
    try new_list.append(5);
    try new_list.append(6);
    const index2 = try new_list.commit();

    new_list = packed_list.new(test_allocator);
    const index3 = try new_list.commit();

    new_list = packed_list.new(test_allocator);
    try new_list.append(7);
    try new_list.append(8);
    try new_list.append(9);
    const index4 = try new_list.commit();

    var iter = packed_list.iterList(index);
    try expectEqual(iter.next(), 1);
    try expectEqual(iter.next(), 2);
    try expectEqual(iter.next(), 3);
    try expectEqual(iter.next(), null);

    var iter2 = packed_list.iterList(index2);
    try expectEqual(iter2.next(), 4);
    try expectEqual(iter2.next(), 5);
    try expectEqual(iter2.next(), 6);
    try expectEqual(iter2.next(), null);

    var iter3 = packed_list.iterList(index3);

    try expectEqual(iter3.next(), null);
    var iter4 = packed_list.iterList(index4);
    try expectEqual(iter4.next(), 7);
    try expectEqual(iter4.next(), 8);
    try expectEqual(iter4.next(), 9);
    try expectEqual(iter4.next(), null);
}

test "packed list single item" {
    const test_allocator = std.testing.allocator;
    // var packed_list = try new(i32, 0).initCapacity(test_allocator, 10);
    var packed_list: new(i32, 0) = .{};
    defer packed_list.deinit(test_allocator);
    var new_list = packed_list.new(test_allocator);

    try new_list.append(16);
    const index = try new_list.commit();

    try expectEqualSlices(i32, &.{16}, packed_list.slice()[index..]);
    try expectEqual(index, 1);
}

test "packed lists sentinel" {
    const T = struct {
        value: i32,
        tag: Tag,
        const Tag = enum {
            a,
            b,
        };
    };

    try expectEqual(T{
        .value = 2,
        .tag = .a,
    }, T{
        .value = 2,
        .tag = .a,
    });
}
