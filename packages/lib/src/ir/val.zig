const Ty = @import("./tyval.zig").Ty;
const std = @import("std");
const Module = @import("./Module.zig");
pub const Value = union(enum) {
    local: Data,
    global: Data,
    inst: Data,
    immediate: struct {
        ty: Ty,
        data: ValData,
    },

    pub const Data = struct {
        index: u32,
        ty: Ty,
        data: ValData,
        /// is_comptime is a marker that it's the compiler's intention that should be resolved at comptime
        /// or that the value can only be executed at comptime
        /// This doesn't mean that the value is actually known at compile time
        /// Ex. A param has runtime value by definition. A comptime param means that
        /// this param can only be evaluated if called in comptime context
        ///
        is_comptime: bool,
    };
    pub const ValData = union(enum) {
        undefined,
        bytes: [8]u8,
        slice: Slice,
        ty: Ty,
        void,
        runtime,

        pub fn fromBytes(value: anytype) ValData {
            var out = ValData{
                .bytes = undefined,
            };
            const slice = switch (@TypeOf(value)) {
                comptime_float => std.mem.asBytes(&@as(f64, value)),
                comptime_int => std.mem.asBytes(&@as(i64, value)),

                else => std.mem.asBytes(&value),
            };

            if (slice.len > 8) {
                @compileError("ValTooLarge");
            }
            std.mem.copyForwards(u8, out.bytes[0..], slice);
            return out;
        }
        pub fn readBytesAs(self: ValData, T: type) T {
            const bytes = self.bytes;
            return std.mem.bytesToValue(T, bytes[0..]);
        }
    };
    const Slice = struct {
        start: u32,
        len: u32,
    };

    pub fn Imm(ty: Ty, data: ValData) Value {
        return .{ .immediate = .{ .ty = ty, .data = data } };
    }
    pub fn ImmTy(ty: Ty) Value {
        return .{ .immediate = .{ .ty = .type, .data = .{ .ty = ty } } };
    }
    pub fn Inst(index: u32, ty: Ty, data: ValData, is_comptime: bool) Value {
        return .{ .inst = .{ .index = index, .ty = ty, .data = data, .is_comptime = is_comptime } };
    }

    pub fn Local(index: u32, ty: Ty, data: ValData, is_comptime: bool) Value {
        return .{ .local = .{ .index = index, .ty = ty, .data = data, .is_comptime = is_comptime } };
    }
    pub fn Undefined(ty: Ty, is_comptime: bool) Value {
        return .{ .local = .{ .ty = ty, .data = .undefined, .is_comptime = is_comptime } };
    }

    pub fn Const(comptime ty: Ty, value: anytype) Value {
        switch (ty) {
            .i8 => return Imm(ty, ValData.fromBytes(@as(i8, @intCast(value)))),
            .i16 => return Imm(ty, ValData.fromBytes(@as(i16, @intCast(value)))),
            .i32 => return Imm(ty, ValData.fromBytes(@as(i32, @intCast(value)))),
            .i64 => return Imm(ty, ValData.fromBytes(@as(i64, @intCast(value)))),
            .u8 => return Imm(ty, ValData.fromBytes(@as(u8, @intCast(value)))),
            .u16 => return Imm(ty, ValData.fromBytes(@as(u16, @intCast(value)))),
            .u32 => return Imm(ty, ValData.fromBytes(@as(u32, @intCast(value)))),
            .u64 => return Imm(ty, ValData.fromBytes(@as(u64, @intCast(value)))),
            .f16 => return Imm(ty, ValData.fromBytes(@as(f16, @floatCast(value)))),
            .f32 => return Imm(ty, ValData.fromBytes(@as(f32, @floatCast(value)))),
            .f64 => return Imm(ty, ValData.fromBytes(@as(f64, @floatCast(value)))),
            .float => return Imm(ty, ValData.fromBytes(@as(f64, @floatCast(value)))),
            .int => return Imm(ty, ValData.fromBytes(@as(i64, @intCast(value)))),
            // .bool => return from(ty, try Val.fromBytes(@as(bool, value))),

            inline else => |_ty| {
                std.debug.panic("Unsupported type: {}", .{_ty});
            },
        }
    }

    pub fn getTy(self: Value) Ty {
        return switch (self) {
            .local, .inst => |local| local.ty,
            .global => |global| global.ty,
            .immediate => |immediate| immediate.ty,
        };
    }
    pub fn getData(self: Value) ValData {
        return switch (self) {
            .local, .inst => |local| local.data,
            .immediate => |immediate| immediate.data,
        };
    }
    pub fn isComptime(self: Value) bool {
        return switch (self) {
            .local, .inst => |local| local.is_comptime,
            .immediate => true,
        };
    }

    // fn fromBytes(value: anytype) !Value {
    //     const T = @TypeOf(value);
    //     switch (T) {
    //         comptime_float => return Imm(Ty.f64, try ValData.fromBytes(@as(f64, value))),
    //         comptime_int => return Imm(Ty.i64, try ValData.fromBytes(@as(i64, value))),

    //         i8 => return Imm(Ty.i8, try ValData.fromBytes(value)),
    //         i16 => return Imm(Ty.i16, try ValData.fromBytes(value)),
    //         i32 => return Imm(Ty.i32, try ValData.fromBytes(value)),
    //         i64 => return Imm(Ty.i64, try ValData.fromBytes(value)),
    //         u8 => return Imm(Ty.u8, try ValData.fromBytes(value)),
    //         u16 => return Imm(Ty.u16, try ValData.fromBytes(value)),
    //         u32 => return Imm(Ty.u32, try ValData.fromBytes(value)),
    //         u64 => return Imm(Ty.u64, try ValData.fromBytes(value)),

    //         usize => return Imm(Ty.u64, try ValData.fromBytes(value)),
    //         isize => return Imm(Ty.i64, try ValData.fromBytes(value)),

    //         bool => return Imm(Ty.bool, try ValData.fromBytes(value)),
    //         else => return error.UnsupportedType,
    //     }
    // }
    pub fn format(
        self: Value,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        switch (self) {
            .global => |global| {
                try writer.print("{}", .{Module.Decl.Ref.from(global.index)});
            },
            // .param => |local| {
            //     try writer.print("p{}", .{local.index});
            // },
            .local => |local| {
                try writer.print("l{}", .{local.index});
            },
            .inst => |local| {
                try writer.print("val{}", .{local.index});
            },
            .immediate => |immediate| {
                switch (immediate.ty) {
                    .i8, .i16, .i32, .i64, .int => {
                        try writer.print("{}{{{d}}}", .{ immediate.ty, immediate.data.readBytesAs(i64) });
                    },
                    .u8, .u16, .u32, .u64 => {
                        try writer.print("{}{{{d}}}", .{ immediate.ty, immediate.data.readBytesAs(u64) });
                    },
                    .f16, .f32, .f64, .float => {
                        try writer.print("{}{{{d}}}", .{ immediate.ty, immediate.data.readBytesAs(f64) });
                    },
                    .bool => {
                        try writer.print("{}{{{}}}", .{ immediate.ty, immediate.data.readBytesAs(bool) });
                    },
                    .void => {
                        try writer.print("{}", .{immediate.ty});
                    },
                    else => {
                        std.debug.panic("Can't format Value of type: {}", .{immediate.ty});
                    },
                }

                // try writer.print("{}", .{immediate.data});
            },
        }
    }
};
