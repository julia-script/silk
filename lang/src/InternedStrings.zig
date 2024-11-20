const std = @import("std");
const Allocator = std.mem.Allocator;
const Array = std.ArrayListUnmanaged;
const HashMap = std.StringHashMapUnmanaged;
const InternedStrings = @This();

data: Array(u8),
allocator: Allocator,
interned: HashMap(InternedSlice),

pub const InternedSlice = struct {
    start: usize,
    end: usize,
};
pub fn init(allocator: Allocator) !InternedStrings {
    return .{
        .data = Array(u8){},
        .allocator = allocator,
        .interned = HashMap(InternedSlice){},
    };
}
pub fn deinit(self: *InternedStrings) void {
    self.data.deinit(self.allocator);
    self.interned.deinit(self.allocator);
}
pub fn intern(self: *InternedStrings, str: []const u8) !InternedSlice {
    const existing = try self.interned.getOrPut(self.allocator, str);
    if (!existing.found_existing) {
        const slice = try self.push(str);
        existing.value_ptr.* = slice;
    }
    return existing.value_ptr.*;
}
pub fn getSlice(self: *InternedStrings, slice: InternedSlice) []const u8 {
    return self.data.items[slice.start..slice.end];
}
pub fn push(self: *InternedStrings, str: []const u8) !InternedSlice {
    const start = self.data.items.len;
    try self.data.ensureUnusedCapacity(self.allocator, str.len + 1);
    self.data.appendSliceAssumeCapacity(str);
    self.data.appendAssumeCapacity(0);
    const end = self.data.items.len;
    return InternedSlice{ .start = start, .end = end - 1 };
}
test "strings" {
    const allocator = std.testing.allocator;
    var strings = try InternedStrings.init(allocator);
    defer strings.deinit();
    const interned = try strings.intern("hello");
    const interned2 = try strings.intern("hello");
    try std.testing.expectEqual(interned, interned2);
    const interned3 = try strings.intern(" world");

    try std.testing.expectEqualStrings("hello", strings.getSlice(interned));
    try std.testing.expectEqualStrings("hello", strings.getSlice(interned2));
    try std.testing.expectEqualStrings(" world", strings.getSlice(interned3));
    try std.testing.expectEqualStrings("hello" ++ "\x00" ++ " world" ++ "\x00", strings.data.items);
}
