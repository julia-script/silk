const std = @import("std");

// This is an expandable array that instead of allocating a bigger array whenever it needs to grow, it allocates a new chunk.
// The main reason I created this is because I needed the pointers to be stable on MirBuilder, because everytime the array grew,
// all the pointers would change and I'd get segfaults everywhere.
// Maybe the std library has something like this, but I couldn't find it.
pub fn ChunkedArray(comptime T: type, chunk_size: usize) type {
    return struct {
        chunks: std.ArrayListUnmanaged(std.BoundedArray(T, chunk_size)) = .{},
        arena: std.heap.ArenaAllocator,
        len: usize = 0,
        const Self = @This();

        pub fn init(allocator: std.mem.Allocator) Self {
            const arena = std.heap.ArenaAllocator.init(allocator);
            return Self{
                .arena = arena,
                .chunks = .{},
            };
        }
        pub fn getCapacity(self: *Self) usize {
            return self.chunks.capacity * chunk_size;
        }
        pub fn deinit(self: *Self) void {
            self.arena.deinit();
        }
        pub fn append(self: *Self, item: T) !void {
            const chunk_index = getChunkIndex(self.len);
            std.debug.print("chunk_index: {} self.chunks.items.len: {}\n", .{ chunk_index, self.chunks.items.len });
            while (chunk_index >= self.chunks.items.len)
                try self.chunks.append(self.arena.allocator(), .{});

            try self.chunks.items[chunk_index].append(item);
            self.len += 1;
        }
        inline fn getChunkIndex(index: usize) usize {
            return @divTrunc(index, chunk_size);
        }
        pub fn get(self: *Self, index: usize) T {
            const chunk_index = getChunkIndex(index);
            return self.chunks.items[chunk_index].buffer[index % chunk_size];
        }
        pub fn getPtr(self: *Self, index: usize) *T {
            const chunk_index = getChunkIndex(index);
            return &self.chunks.items[chunk_index].buffer[index % chunk_size];
        }
        pub fn set(self: *Self, index: usize, item: T) void {
            const chunk_index = getChunkIndex(index);
            self.chunks.items[chunk_index].buffer[index % chunk_size] = item;
        }
        const Iterator = struct {
            index: usize,
            chunked_array: *Self,
            pub fn next(self: *Iterator) ?*T {
                if (self.index >= self.chunked_array.len) return null;
                const item = self.chunked_array.getPtr(self.index);
                self.index += 1;
                return item;
            }
        };
        pub fn iterator(self: *Self) Iterator {
            return Iterator{
                .index = 0,
                .chunked_array = self,
            };
        }
    };
}

const expectEqual = std.testing.expectEqual;
test "ChunkedArray" {
    const Array = ChunkedArray(u8, 4);
    var chunked_array = Array.init(std.testing.allocator);

    defer chunked_array.deinit();

    try expectEqual(0, chunked_array.getCapacity());

    try chunked_array.append(0);
    try chunked_array.append(1);
    try chunked_array.append(2);
    try chunked_array.append(3);
    try chunked_array.append(4);
    try chunked_array.append(5);

    std.debug.print("{}", .{chunked_array.getCapacity()});

    try expectEqual(0, chunked_array.get(0));
    try expectEqual(1, chunked_array.get(1));
    try expectEqual(2, chunked_array.get(2));
    try expectEqual(3, chunked_array.get(3));
    try expectEqual(4, chunked_array.get(4));
}
