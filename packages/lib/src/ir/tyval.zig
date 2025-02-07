const Op = @import("./opcodes.zig").Op;
const Value = @import("./val.zig").Value;
const std = @import("std");
const activeTag = std.meta.activeTag;
const utils = @import("./utils.zig");
const Block = @import("./Block.zig");
const Definition = @import("./Definition.zig");
const InstData = @import("./inst.zig").InstData;
const Module = @import("./Module.zig");

// pub const Val = union(ValKind) {
//     /// Can represent any value that fits in 8 bytes,
//     /// it should always be paired with a Type to know how to interpret the bytes
//     bytes: [8]u8,

//     /// Bigger values are stored on DFG's bytes buffer
//     slice: Slice,

//     // Types can also be values
//     ty: Ty,

//     /// A reference to a map of values indexed by the instruction ref
//     // ref: Inst.Ref,
//     result: Definition.Ref,
//     runtime,
//     unresolved,
//     pub const UNRESOLVED = Val{ .unresolved = {} };
//     pub const RUNTIME = Val{ .runtime = {} };
//     // pub const UNRESOLVED_TY = Val{ .ty = Ty.unresolved };

//     const Slice = struct {
//         start: u32,
//         len: u32,
//     };

//     pub const Ref = utils.MakeRef(.val, Val);

//     pub const ValKind = enum {
//         bytes,
//         slice,
//         ty,
//         ref,
//         result,
//         runtime,
//         unresolved,
//     };

//     pub fn makeRef(value: u32) Val {
//         return .{ .ref = Ref.from(value) };
//     }

//     pub fn tag(self: Val) ValKind {
//         return activeTag(self);
//     }

//     pub fn isKind(self: Val, kind: ValKind) bool {
//         return activeTag(self) == kind;
//     }

//     pub fn acceptRef(self: Val) ?Ref {
//         switch (self) {
//             .ref => |r| return r,
//             else => return null,
//         }
//     }

//     pub fn acceptBytes(self: Val) ?[8]u8 {
//         switch (self) {
//             .bytes => |b| return b,
//             else => return null,
//         }
//     }

//     pub fn acceptTy(self: Val) ?Ty {
//         switch (self) {
//             .ty => |t| return t,
//             else => return null,
//         }
//     }

//     pub fn fromIndex(index: u32) Val {
//         return .{ .ref = Ref.from(index) };
//     }
//     pub fn fromTy(ty: Ty) Val {
//         return .{ .ty = ty };
//     }

//     pub fn fromBytes(value: anytype) !Val {
//         var out = Val{
//             .bytes = undefined,
//         };
//         const slice = switch (@TypeOf(value)) {
//             comptime_float => std.mem.asBytes(&@as(f64, value)),
//             comptime_int => std.mem.asBytes(&@as(i64, value)),

//             else => std.mem.asBytes(&value),
//         };

//         if (slice.len > 8) {
//             return error.ValTooLarge;
//         }
//         std.mem.copyForwards(u8, out.bytes[0..], slice);
//         return out;
//     }

//     pub fn readBytesAs(self: Val, T: type) T {
//         const bytes = self.bytes;
//         return std.mem.bytesToValue(T, bytes[0..]);
//     }

//     pub fn format(
//         self: Val,
//         comptime _: []const u8,
//         _: std.fmt.FormatOptions,
//         writer: std.io.AnyWriter,
//     ) !void {
//         switch (self) {
//             .bytes => |bytes| {
//                 try writer.print("b{{", .{});

//                 var i: usize = 0;
//                 for (bytes) |b| {
//                     if (i > 0) {
//                         try writer.print(" ", .{});
//                     }
//                     i += 1;
//                     try writer.print("{x}", .{b});
//                 }
//                 try writer.print("}}", .{});
//             },
//             .ref => |ref| {
//                 try writer.print("{}", .{ref});
//             },
//             .ty => |ty| {
//                 try writer.print("val({})", .{ty});
//             },
//             .slice => |slice| {
//                 try writer.print("{{{d}, {d}}}", .{ slice.start, slice.len });
//             },
//             .result => |result| {
//                 try writer.print("{}", .{result});
//             },

//             .unresolved => try writer.print("?", .{}),
//             .runtime => try writer.print("runtime", .{}),
//         }
//     }
//     pub fn unwrapTy(self: Val) Ty {
//         return switch (self) {
//             .ty => |ty| Ty.fromRef(ty),
//             else => unreachable,
//         };
//     }
//     pub fn isImmediate(self: Val) bool {
//         return !self.isKind(.ref);
//     }
// };
// const Inst = @import("./inst.zig").Inst;

// pub const TyVal = struct {
//     val: Val,
//     ty: Ty,

//     pub fn from(ty: Ty, value: Val) TyVal {
//         return .{ .val = value, .ty = ty };
//     }
//     pub fn fromConst(ty: Ty, value: anytype) !TyVal {
//         switch (ty) {
//             .i8 => return from(ty, try Val.fromBytes(@as(i8, @intCast(value)))),
//             .i16 => return from(ty, try Val.fromBytes(@as(i16, @intCast(value)))),
//             .i32 => return from(ty, try Val.fromBytes(@as(i32, @intCast(value)))),
//             .i64 => return from(ty, try Val.fromBytes(@as(i64, @intCast(value)))),
//             .u8 => return from(ty, try Val.fromBytes(@as(u8, @intCast(value)))),
//             .u16 => return from(ty, try Val.fromBytes(@as(u16, @intCast(value)))),
//             .u32 => return from(ty, try Val.fromBytes(@as(u32, @intCast(value)))),
//             .u64 => return from(ty, try Val.fromBytes(@as(u64, @intCast(value)))),
//             .f16 => return from(ty, try Val.fromBytes(@as(f16, @floatCast(value)))),
//             .f32 => return from(ty, try Val.fromBytes(@as(f32, @floatCast(value)))),
//             .f64 => return from(ty, try Val.fromBytes(@as(f64, @floatCast(value)))),
//             // .bool => return from(ty, try Val.fromBytes(@as(bool, value))),

//             else => return error.UnsupportedType,
//         }
//     }

//     pub fn fromBytes(value: anytype) !TyVal {
//         const T = @TypeOf(value);
//         switch (T) {
//             comptime_float => return from(Ty.f64, try Val.fromBytes(@as(f64, value))),
//             comptime_int => return from(Ty.i64, try Val.fromBytes(@as(i64, value))),

//             i8 => return from(Ty.i8, try Val.fromBytes(value)),
//             i16 => return from(Ty.i16, try Val.fromBytes(value)),
//             i32 => return from(Ty.i32, try Val.fromBytes(value)),
//             i64 => return from(Ty.i64, try Val.fromBytes(value)),
//             u8 => return from(Ty.u8, try Val.fromBytes(value)),
//             u16 => return from(Ty.u16, try Val.fromBytes(value)),
//             u32 => return from(Ty.u32, try Val.fromBytes(value)),
//             u64 => return from(Ty.u64, try Val.fromBytes(value)),

//             bool => return from(Ty.bool, try Val.fromBytes(value)),
//             else => return error.UnsupportedType,
//         }
//     }

//     pub fn format(
//         self: TyVal,
//         comptime _: []const u8,
//         _: std.fmt.FormatOptions,
//         writer: std.io.AnyWriter,
//     ) !void {
//         switch (self.ty) {
//             .ref => |ref| {
//                 try writer.print("{{{}, {}}}", .{ ref, self.val });
//             },
//             .i8, .i16, .i32, .i64, .isize => {
//                 try writer.print("{}{{{d}}}", .{ self.ty, self.val.readBytesAs(i64) });
//             },
//             .u8, .u16, .u32, .u64, .usize => {
//                 try writer.print("{}{{{d}}}", .{ self.ty, self.val.readBytesAs(u64) });
//             },
//             else => {
//                 try writer.print("{}{{{d}}}", .{ self.ty, self.val });
//             },
//         }
//     }
// };

fn assertFormatsAs(value: anytype, comptime expected: []const u8) !void {
    var buf: [expected.len * 2]u8 = undefined;
    var fbo = std.io.fixedBufferStream(&buf);
    const writer = fbo.writer();
    try writer.print("{}", .{value});
    const actual = fbo.getWritten();
    return try std.testing.expectEqualStrings(actual, expected);
}
// test "Entities" {
//     const v4 = try TyVal.fromBytes(3.0);
//     try assertFormatsAs(v4.ty, "f64");
//     try assertFormatsAs(TyVal.from(Ty.fromRef(3), Val.fromIndex(1)), "{ty#3, val#1}");
//     try assertFormatsAs(TyVal.from(.i64, Val.fromIndex(1)), "i64{val#1}");
//     try assertFormatsAs(TyVal.from(.i64, .unresolved), "i64{?}");
//     // std.debug.print("{any}\n", .{TyVal.from(Ty.i32, .{ .ref = 1 })});
// }

pub const Ty = union(enum) {
    void,
    f16,
    f32,
    f64,

    i8,
    i16,
    i32,
    i64,

    u8,
    u16,
    u32,
    u64,

    int,
    float,

    bool,

    type,
    unresolved,

    typeof: InstData.Ref,
    // array: struct {
    //     type: Ty,
    //     size: Value,
    // },
    // func: struct {
    //     signature: Module.Signature.Ref,
    //     declaration: Module.Decl.Ref,
    // },

    ref: Ref,

    pub const Ref = utils.MakeRef(.ty, Ty);
    pub const TyData = union(enum) {
        func: Function,
        array: Array,
        // decl: Module.Decl.Ref,
        pub const Array = struct {
            type: Ty,
            size: Value,
        };
        pub const Function = struct {
            signature: Module.Signature.Ref,
            declaration: Module.Decl.Ref,
        };
    };

    pub fn eql(self: Ty, other: Ty) bool {
        const self_tag = std.meta.activeTag(self);
        const other_tag = std.meta.activeTag(other);
        if (self_tag == other_tag) {
            return switch (self) {
                .ref => |ref| ref.ref == other.ref.ref,
                else => true,
            };
        }
        return false;
    }
    pub fn makeRef(self: Ty) Ref {
        return .{ .ref = Ref.from(@intFromEnum(self)) };
    }
    // pub fn toRef(self: Ty) Ref {
    //     const as_int: u32 = @intFromEnum(self);
    //     return Ref.from(as_int);
    // }
    // pub fn fromRef(ref: Ref) Ty {
    //     return @enumFromInt(ref.ref);
    // }
    // pub inline fn asIndex(self: Ty) u32 {
    //     return @intFromEnum(self) - FIRST_REF;
    // }
    // pub inline fn isComplex(self: Ty) bool {
    //     return @intFromEnum(self) >= FIRST_REF;
    // }

    pub fn accept(self: Ty, other: std.meta.Tag(Ty)) ?Ref {
        if (self == other) {
            return self.makeRef();
        }
        return null;
    }
    pub fn toVal(self: Ty) Value {
        return Value.ImmTy(self);
    }

    pub fn format(
        self: Ty,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        switch (self) {
            .ref => |ref| {
                try writer.print("{}", .{ref.ref});
            },
            else => {
                try writer.print("{s}", .{@tagName(self)});
            },
        }
    }

    pub fn isInt(self: Ty) bool {
        return switch (self) {
            .i8,
            .i16,
            .i32,
            .i64,
            .u8,
            .u16,
            .u32,
            .u64,
            // float and int literal don't have define type yet so they can be coerced to both
            // int and float is just how they are currently represented
            // we may introduce other untyped numbers in the future like bigint and decimals
            .int,
            .float,
            => true,
            else => false,
        };
    }
    pub fn isFloat(self: Ty) bool {
        return switch (self) {
            .f16, .f32, .f64, .float, .int => true,
            else => false,
        };
    }
    pub fn isSigned(self: Ty) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .float, .int => true,
            else => false,
        };
    }
    pub fn isUntypedNumber(self: Ty) bool {
        return switch (self) {
            .int, .float => true,
            else => false,
        };
    }
    pub fn bits(self: Ty) u32 {
        return switch (self) {
            .i8, .u8 => 8,
            .i16, .u16 => 16,
            .i32, .u32 => 32,
            .i64, .u64 => 64,
            .int => 0,
            .float => 0,

            else => 0,
        };
    }
    /// There might be cases where the expression hasn't been evaluated yet,
    /// which my be a numeric type but we don't know yet
    pub fn isResolvedNumeric(self: Ty) bool {
        return switch (self) {
            .i8, .i16, .i32, .i64, .u8, .u16, .u32, .u64, .int, .float => true,
            else => false,
        };
    }
};
test "Ty" {
    try assertFormatsAs(Ty.fromRef(1), "ty#1");
    try assertFormatsAs(Ty.fromRef(Ty.Ref.from(1)), "ty#1");
    try assertFormatsAs(Ty.f32, "f32");
    try std.testing.expectEqual(@intFromEnum(Ty.fromRef(0)), Ty.FIRST_REF);
    try std.testing.expectEqual(Ty.fromRef(0).encode(), Ty.FIRST_REF);
    try std.testing.expectEqual(Ty.decode(Ty.FIRST_REF), Ty.fromRef(0));
    try std.testing.expectEqual(Ty.decode(Ty.FIRST_REF + 1), Ty.fromRef(1));
}
