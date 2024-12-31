const std = @import("std");
const Sema = @import("Sema.zig");
const Builder = @import("gen.zig").Builder;

const Self = @This();
memory: std.ArrayListUnmanaged(u8) = .{},
sema: *Sema,
allocator: std.mem.Allocator,
pub fn init(allocator: std.mem.Allocator, sema: *Sema) Self {
    return Self{
        .memory = .{},
        .sema = sema,
        .allocator = allocator,
    };
}
pub fn deinit(self: *Self) void {
    self.memory.deinit(self.allocator);
}

pub fn alloc(self: *Self, ty: Sema.Type.Key, count: u32) !u32 {
    const pointer = self.memory.items.len;
    const size = self.sema.builder.getTypeSize(ty);
    _ = try self.memory.ensureUnusedCapacity(self.allocator, size * count);
    self.memory.items.len += size * count;
    return @intCast(pointer);
}
pub fn create(self: *Self, ty: Sema.Type.Key) !u32 {
    const pointer = self.memory.items.len;
    const size = self.sema.builder.getTypeSize(ty);
    _ = try self.memory.ensureUnusedCapacity(self.allocator, size);
    self.memory.items.len += size;
    // const size = self.sema.builder.getSimpletTypeSize(ty) catch unreachable;

    // _ = self; // autofix
    // _ = ty; // autofix
    return @intCast(pointer);
}

pub fn destroy(self: *Self, ty: Sema.Type.Key, ptr: usize) void {
    _ = ty; // autofix
    _ = self; // autofix
    _ = ptr; // autofix
}

pub fn readSliceAsType(self: *Self, T: type, slice: []const u8) T {
    _ = self; // autofix
    _ = slice; // autofix
    // std.mem.readInt
    // switch (ty) {
    //     .simple => |simple| {
    //         return Sema.Value{
    //             .hash = std.hash.Wyhash.hash(0, slice),
    //             .data = .{ .integer = try std.fmt.parseInt(i64, slice, 10) },
    //         };
    //     },
    // }
}
fn storeType(self: *Self, T: type, ptr: usize, value: Sema.Value.Key) void {
    const bytes = std.mem.asBytes(&self.sema.builder.getNumberValueKeyAs(T, value));
    std.mem.copyForwards(u8, self.memory.items[ptr..], bytes[0..]);
}
pub fn storeAt(self: *Self, T: type, ptr: u32, value: T) void {
    const bytes = switch (T) {
        []const u8 => value,
        else => std.mem.asBytes(&value),
    };
    std.mem.copyForwards(u8, self.memory.items[ptr..], bytes);
}
pub fn store(self: *Self, type_key: Sema.Type.Key, ptr: usize, value: Sema.Value.Key) !void {
    const type_size = self.sema.builder.getTypeSize(type_key);
    _ = type_size; // autofix
    switch (type_key) {
        .simple => |simple| switch (simple) {
            .i8 => self.storeType(i8, ptr, value),
            .i16 => self.storeType(i16, ptr, value),
            .i32 => self.storeType(i32, ptr, value),
            .i64 => self.storeType(i64, ptr, value),
            .u8 => self.storeType(u8, ptr, value),
            .u16 => self.storeType(u16, ptr, value),
            .u32 => self.storeType(u32, ptr, value),
            .u64 => self.storeType(u64, ptr, value),
            .f32 => self.storeType(f32, ptr, value),
            .f64 => self.storeType(f64, ptr, value),
            .number => self.storeType(i64, ptr, value),
            .usize => self.storeType(u64, ptr, value),
            .bchar => self.storeType(u8, ptr, value),
            else => std.debug.panic("unsupported type: {any}", .{simple}),
        },

        .complex => |complex| switch (self.sema.types.entries.items(.value)[complex].data) {
            // .string => {
            //     const ty = self.sema.builder.getType(type_key).?;
            //     std.debug.panic("unsupported type: {any}", .{ty});
            // },
            else => {
                std.debug.panic("unsupported type: {any}", .{complex});
            },
        },
    }
}

fn loadType(self: *Self, T: type, ptr: usize) !Sema.Value.Key {
    switch (T) {
        f32, f64 => {
            const bytes = self.memory.items[ptr..];
            return try self.sema.builder.internValueData(.{ .float = @floatCast(std.mem.bytesToValue(T, bytes)) });
        },
        i8, i16, i32, i64, u8, u16, u32, u64, usize => {
            const bytes = self.memory.items[ptr..];
            return try self.sema.builder.internValueData(.{ .integer = @intCast(std.mem.bytesToValue(T, bytes)) });
        },
        else => {
            std.debug.panic("unsupported type: {any}", .{T});
        },
    }
}
pub fn load(self: *Self, ty: Sema.Type.Key, ptr: usize) !Sema.Value.Key {
    return switch (ty) {
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
            .usize => try self.loadType(u64, ptr),
            .number => try self.loadType(i64, ptr),
            else => std.debug.panic("unsupported type: {any}", .{simple}),
        },
        .complex => {
            const loaded_type = self.sema.builder.getType(ty).?;
            // const loaded_value = try self.load(loaded_type, ptr);
            std.debug.panic("unsupported type: {any}", .{loaded_type});
            // return try self.sema.builder.internValueData(.{ .complex = loaded_type });
        },
        // else => {
        //     std.debug.panic("unsupported type: {any}", .{ty});
        // },
    };
}
