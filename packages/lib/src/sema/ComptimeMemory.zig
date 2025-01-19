const std = @import("std");
const Sema = @import("Sema.zig");
const Builder = @import("gen.zig").Builder;

pub const STACK_SIZE = 1024 * 1024;
const Self = @This();
memory: std.ArrayListUnmanaged(u8) = .{},
stack_pointer: usize = STACK_SIZE,
allocator: std.mem.Allocator,

// comp_memory: std.ArrayListUnmanaged(u8) = .{},

sema: *Sema,
// allocator: std.mem.Allocator,
// const CompAllocator = struct {
//     memory: std.ArrayListUnmanaged(u8),
//     sema: *Sema,
//     allocator: std.mem.Allocator,
//     pub fn alloc(self: *Self, size: usize) usize {
//         std.debug.assert(size <= self.stack_pointer);
//         if (self.stack_pointer < size) {
//             std.debug.panic("Comptime stack overflow", .{});
//         }
//         self.stack_pointer -= size;
//         return self.stack_pointer;
//     }
//     pub fn create(self: *CompAllocator, ty: Sema.Type.Key) !usize {
//         const size = self.sema.builder.getTypeSize(ty);
//         const pointer = self.alloc(size);

//         self.memory.items.len += size;
//         std.debug.print("creating type: {any} size: {d} capacity: {d}\n", .{
//             self.sema.builder.getFormattableType(ty),
//             size,
//             self.memory.items.len,
//         });

//         return pointer;
//     }
// };

pub fn init(allocator: std.mem.Allocator, sema: *Sema) !Self {
    var mem = Self{
        .memory = .{},
        .sema = sema,
        .allocator = allocator,
    };

    // const gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // gpa.deinit();
    try mem.memory.ensureTotalCapacity(allocator, STACK_SIZE);
    mem.memory.items.len = STACK_SIZE;

    return mem;
}
pub fn deinit(self: *Self) void {
    self.memory.deinit(self.allocator);
}
// pub fn alloc(self: *Self, ty: Sema.Type.Key, count: usize) !usize {
//     const pointer = self.memory.items.len;
//     const size = self.sema.builder.getTypeSize(ty);
//     _ = try self.memory.ensureUnusedCapacity(self.allocator, size * count);
//     self.memory.items.len += size * count;
//     return pointer;
// }
pub fn create(self: *Self, ty: Sema.Type.Key) !usize {
    const size = self.sema.builder.getTypeSize(ty);
    const pointer = self.memory.items.len;
    try self.memory.ensureUnusedCapacity(self.allocator, size);
    self.memory.items.len += size;
    return pointer;
}
pub fn destroy(self: *Self, typed_value: Sema.TypedValue) void {
    _ = self; // autofix
    _ = typed_value; // autofix

    // const size = self.sema.builder.getTypeSize(typed_value.type);
    // self.memory.items.len -= size;
}
pub fn stackAlloc(self: *Self, size: usize) usize {
    std.debug.assert(size <= self.stack_pointer);
    if (self.stack_pointer < size) {
        std.debug.panic("Comptime stack overflow", .{});
    }
    self.stack_pointer -= size;
    return self.stack_pointer;
}

pub fn stackCreate(self: *Self, ty: Sema.Type.Key) usize {
    const size = self.sema.builder.getTypeSize(ty);
    const pointer = self.stackAlloc(size);

    self.memory.items.len += size;
    std.debug.print("creating type: {any} size: {d} capacity: {d}\n", .{
        self.sema.builder.getFormattableType(ty),
        size,
        self.memory.items.len,
    });

    return pointer;
}

pub fn storeAt(self: *Self, T: type, ptr: usize, value: T) void {
    const bytes = switch (T) {
        []const u8 => value,
        else => std.mem.asBytes(&value),
    };
    std.mem.copyForwards(u8, self.memory.items[ptr..], bytes);
}
// pub fn memcpy(self: *Self, typed_value: Sema.TypedValue, ptr: usize) void {
//     const bytes = std.mem.asBytes(&self.sema.builder.getNumberValueKeyAs(typed_value.type_key, typed_value.value));
//     std.mem.copyForwards(u8, self.memory.items[ptr..], bytes);
// }
fn storeType(self: *Self, T: type, ptr: usize, typed_value: Sema.TypedValue) !void {
    std.debug.print("[STORE] type: {any} at ptr {d} capacity {d}\n", .{
        self.sema.builder.getFormattableTypedValue(typed_value),
        ptr,
        self.memory.items.len,
    });
    const bytes = try self.sema.builder.readBytes(typed_value);
    std.mem.copyForwards(u8, self.memory.items[ptr..], bytes[0..@sizeOf(T)]);
}
pub fn store(self: *Self, ptr: usize, typed_value: Sema.TypedValue) !void {

    // const type_size = self.sema.builder.getTypeSize(typed_value.type);
    switch (typed_value.type) {
        .simple => |simple| switch (simple) {
            .i8 => return try self.storeType(i8, ptr, typed_value),
            .i16 => return try self.storeType(i16, ptr, typed_value),
            .i32 => return try self.storeType(i32, ptr, typed_value),
            .i64 => return try self.storeType(i64, ptr, typed_value),
            .u8 => return try self.storeType(u8, ptr, typed_value),
            .u16 => return try self.storeType(u16, ptr, typed_value),
            .u32 => return try self.storeType(u32, ptr, typed_value),
            .u64 => return try self.storeType(u64, ptr, typed_value),
            .f32 => return try self.storeType(f32, ptr, typed_value),
            .f64 => return try self.storeType(f64, ptr, typed_value),
            .number => return try self.storeType(i64, ptr, typed_value),
            .bchar => return try self.storeType(u8, ptr, typed_value),
            .bool => return try self.storeType(bool, ptr, typed_value),
            .boolean => return try self.storeType(bool, ptr, typed_value),
            .int => return try self.storeType(i64, ptr, typed_value),
            .float => return try self.storeType(f64, ptr, typed_value),
            // .void => return try self.storeType(void, ptr, typed_value),
            else => {
                std.debug.print("unsupported type: {any}\n", .{self.sema.builder.getFormattableTypedValue(typed_value)});
                @panic("unsupported type");
            },
        },

        .complex => |complex| switch (self.sema.builder.getComplexType(complex).data) {
            // .string => {
            //     const ty = self.sema.builder.getType(type_key).?;
            //     std.debug.panic("unsupported type: {any}", .{ty});
            // },
            .any => |any| {
                return try self.store(ptr, .{
                    .type = any.concrete,
                    .value = typed_value.value,
                });
            },
            .array => |array| {
                switch (typed_value.value) {
                    .complex => |complex_value| switch (self.sema.builder.getComplexValue(complex_value).data) {
                        .string_literal => |range| {
                            const slice = self.sema.builder.getSlice(range);
                            std.debug.print("storing string_literal '{s}'\n", .{slice});
                            std.mem.copyForwards(u8, self.memory.items[ptr..], slice);
                            return;
                        },
                        else => {},
                    },
                    else => {},
                }
                _ = array; // autofix
                // return try self.store(ptr, .{
                //     .type = array.child,
                //     .value = typed_value.value,
                // });
            },
            else => {
                std.debug.panic("unsupported type: {any}\n", .{self.sema.builder.getFormattableTypedValue(typed_value)});
            },
        },
    }
}

fn loadType(self: *Self, T: type, ptr: usize) !Sema.Value.Key {
    switch (T) {
        f32, f64, i8, i16, i32, i64, u8, u16, u32, u64, usize, bool => {
            var bytes: [8]u8 = [1]u8{0} ** 8;
            std.debug.print("size: {d} len {d} {d}\n", .{ ptr, self.memory.items.len, @sizeOf(T) });
            std.mem.copyForwards(u8, bytes[0..], self.memory.items[ptr .. ptr + @sizeOf(T)]);
            return try self.sema.builder.internValueData(.{ .bytes = bytes });
        },
        else => {
            std.debug.panic("unsupported type: {any}", .{T});
        },
    }
}

pub fn load(self: *Self, ty: Sema.Type.Key, ptr: usize) !Sema.Value.Key {
    std.debug.print("[LOAD] type: {any} from ptr {d}\n", .{ self.sema.builder.getFormattableType(ty), ptr });
    const loaded = switch (ty) {
        .simple => |simple| switch (simple) {
            .i8 => try self.loadType(i8, ptr),
            .i16 => try self.loadType(i16, ptr),
            .i32 => try self.loadType(i32, ptr),
            .i64 => try self.loadType(i64, ptr),
            .u8 => try self.loadType(u8, ptr),
            .u16 => try self.loadType(u16, ptr),
            .u32 => try self.loadType(u32, ptr),
            .u64 => try self.loadType(u64, ptr),
            .f32 => try self.loadType(f32, ptr),
            .f64 => try self.loadType(f64, ptr),
            .number => try self.loadType(i64, ptr),
            .bchar => try self.loadType(u8, ptr),
            .bool => try self.loadType(bool, ptr),
            .int => try self.loadType(i64, ptr),
            .float => try self.loadType(f64, ptr),
            else => std.debug.panic("unsupported type: {any}", .{simple}),
        },
        .complex => {
            return try self.sema.builder.numberAsBytesValueKey(ptr);
            // return try self.sema.builder.internValueData(.{ .deref = .{ .ptr = .{
            //     .type = ty,
            //     .value = try self.sema.builder.numberAsBytesValueKey(ptr),
            // } } });
            // std.debug.print("unsupported type: ", .{});
            // try self.sema.formatType(std.io.getStdErr().writer().any(), ty);
            // std.debug.print("\n", .{});
            // @panic("unsupported type");
            // const loaded_type = self.sema.builder.getType(ty).?;
            // const loaded_value = try self.load(loaded_type, ptr);
            // return try self.sema.builder.internValueData(.{ .complex = loaded_type });
            // std.debug.panic("unsupported type: {any}", .{ty});
        },
        // else => {
        // },
    };
    std.debug.print("Loaded type: {any} from ptr {d}\n", .{
        self.sema.builder.getFormattableTypedValue(Sema.TypedValue{
            .type = ty,
            .value = loaded,
        }),
        ptr,
    });
    return loaded;
}
pub fn stackDupe(self: *Self, typed_value: Sema.TypedValue) !Sema.TypedValue {
    const type_to_dupe = self.sema.builder.unwrapPointerType(typed_value.type) orelse std.debug.panic("not a pointer type", .{});
    const ptr = try self.sema.builder.readNumberAsType(usize, typed_value);
    const size = self.sema.builder.getTypeSize(type_to_dupe);
    const new_ptr = self.stackAlloc(size);
    std.mem.copyForwards(u8, self.memory.items[new_ptr..], self.memory.items[ptr .. ptr + size]);
    return .{
        .type = typed_value.type,
        .value = try self.sema.builder.numberAsBytesValueKey(new_ptr),
    };
}
pub fn memcpy(self: *Self, ty: Sema.Type.Key, src: usize, dest: usize) !void {
    const size = self.sema.builder.getTypeSize(ty);
    const src_ptr = self.memory.items[src .. src + size];
    const dest_ptr = self.memory.items[dest .. dest + size];
    std.debug.print("memcpy {any} {any} {d}\n", .{ src_ptr, dest_ptr, size });
    std.mem.copyForwards(u8, dest_ptr, src_ptr);
}
