const Module = @import("./Module.zig");
const Ty = Module.Ty;
const utils = Module.utils;
const InstData = Module.InstData;
const Dfg = Module.Dfg;
const std = @import("std");

pub const Value = union(enum) {
    // stores any value that can be represented in 8 bytes
    // reading and writing is based on the paired type in TypedValue
    bytes: [8]u8,
    // pointer to a value in the comptime linear memory
    // size is determined by the type in TypedValue
    pointer: u32,
    ty: Ty,
    // because types are also values, we need way to represent that the type
    // expected is a certain type.
    // for example, a function parameter that takes either an i32 or a i64, has a signature of
    // `fn(i32 | i64)` -> Value.ty(i32) | Value.ty(i64)
    // But a function that takes the either the type i32 or i64
    // has the signature
    // `fn(Type(i32) | Type(i64))` -> Value.tyty(i32) | Value.tyty(i64)
    // Note: thats the maximum resolution we get though, a tyty of a ty is  Value.ty(type).
    tyty: Ty,

    inst: InstData.Ref,
    void,
    undefined,
    runtime,
    local: Dfg.Local.Ref,
    global: Module.Decl.Ref,

    ref: Ref,

    pub const Ref = utils.MakeRef(.value, Value, "\x1b[32mval{d}\x1b[0m");

    fn bytesOfT(T: type, value: T) Value {
        var val: Value = .{ .bytes = undefined };
        std.mem.copyForwards(u8, &val.bytes, std.mem.asBytes(&value));
        return val;
    }

    pub fn comptimeFromBytes(comptime ty: std.meta.Tag(Ty), value: anytype) Value {
        switch (ty) {
            .i8, .i16, .i32, .i64, .int => {
                return bytesOfT(i64, value);
            },
            .u8, .u16, .u32, .u64 => {
                return bytesOfT(u64, value);
            },
            .f32, .f64, .float => {
                return bytesOfT(f64, value);
            },
            .bool => {
                return bytesOfT(bool, value);
            },
            else => std.debug.panic("type {} can't be stored as Value.bytes", .{ty}),
        }
    }

    pub fn fromBytes(ty: Ty, value: anytype) Value {
        const T = @TypeOf(value);

        switch (ty) {
            inline .i8,
            .i16,
            .i32,
            .i64,
            .int,
            => switch (T) {
                i8, i16, i32, i64, comptime_int => return bytesOfT(i64, value),
                else => std.debug.panic("type '{s}' can't be stored as Value.bytes", .{@typeName(T)}),
            },
            .u8,
            .u16,
            .u32,
            .u64,
            => switch (T) {
                u8, u16, u32, u64, comptime_int => return bytesOfT(u64, value),
                else => std.debug.panic("type '{s}' can't be stored as Value.bytes", .{@typeName(T)}),
            },
            .f32,
            .f64,
            .float,
            => switch (T) {
                f32, f64, comptime_float, comptime_int => return bytesOfT(f64, value),
                else => std.debug.panic("type '{s}' can't be stored as Value.bytes", .{@typeName(T)}),
            },
            inline .bool,
            => switch (T) {
                bool => return bytesOfT(bool, value),
                else => std.debug.panic("type '{s}' can't be stored as bool", .{@typeName(T)}),
            },

            else => std.debug.panic("type {} can't be stored as Value.bytes", .{ty}),
        }
    }
    // fn readBytesAsT(self: Value, T: type) T {
    //     return std.mem.bytesToValue(T, self.bytes);
    // }
    pub fn readAs(self: Value, T: type) T {
        return std.mem.bytesToValue(T, &self.bytes);
    }
    pub fn acceptLocal(self: Value) ?Dfg.Local.Ref {
        return switch (self) {
            .local => self.local,
            else => null,
        };
    }
    pub fn isTag(self: Value, tag: std.meta.Tag(Value)) bool {
        return std.meta.activeTag(self) == tag;
    }
    pub fn acceptGlobal(self: Value) ?Module.Decl.Ref {
        return switch (self) {
            .global => self.global,
            else => null,
        };
    }
};
