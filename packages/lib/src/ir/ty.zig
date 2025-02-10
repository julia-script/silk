const Op = @import("./opcodes.zig").Op;
const Value = @import("./val.zig").Value;
const std = @import("std");
const activeTag = std.meta.activeTag;
const utils = @import("./utils.zig");
const Block = @import("./Block.zig");
const Definition = @import("./Definition.zig");
const InstData = @import("./inst.zig").InstData;
const Module = @import("./Module.zig");

fn assertFormatsAs(value: anytype, comptime expected: []const u8) !void {
    var buf: [expected.len * 2]u8 = undefined;
    var fbo = std.io.fixedBufferStream(&buf);
    const writer = fbo.writer();
    try writer.print("{}", .{value});
    const actual = fbo.getWritten();
    return try std.testing.expectEqualStrings(actual, expected);
}

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
    global: Module.Decl.Ref,
    ref: Ref,

    pub const Ref = utils.MakeRef(.ty, Ty, "ty{d}");
    pub const TyData = union(enum) {
        func: Function,
        array: Array,
        @"struct": Struct,
        // decl: Module.Decl.Ref,
        pub const Array = struct {
            type: Ty,
            size: Module.TypedValue,
        };
        pub const Function = struct {
            signature: Module.Signature.Ref,
            declaration: Module.Decl.Ref,
        };
        pub const Struct = struct {
            fields: []const Field,
            sealed: bool,
            pub const Field = struct {
                name: []const u8,
                ty: Ty,
                source_order_index: u32,
            };
        };
        pub fn displayFn(self: TyData, writer: std.io.AnyWriter, module: *const Module) anyerror!void {
            switch (self) {
                .func => |func| {
                    try writer.print("func({})", .{func.signature});
                },
                .array => |array| {
                    try writer.print("array({}, {})", .{ array.type.display(module), array.size.display(module) });
                },
                .@"struct" => |struct_ty| {
                    try writer.print("struct{{ ", .{});
                    for (struct_ty.fields, 0..) |field, i| {
                        if (i > 0) {
                            try writer.print(", ", .{});
                        }
                        try writer.print("{s}: {}", .{ field.name, field.ty.display(module) });
                    }
                    try writer.print(" }}", .{});
                },

                // else => {
                //     try writer.print("{}", .{self});
                // },
            }
        }
        pub fn display(self: TyData, module: *const Module) Module.utils.MakeDisplay(TyData, TyData.displayFn) {
            return .{ .value = self, .module = module };
        }
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
                try writer.print("{}", .{ref});
            },
            .global => |ref| {
                try writer.print("{}", .{ref});
            },

            else => {
                try writer.print("{s}", .{@tagName(self)});
            },
        }
    }

    fn displayFn(self: Ty, writer: std.io.AnyWriter, module: *const Module) anyerror!void {
        // try writer.print("{}", .{self});
        switch (self) {
            .ref => |ref| {
                const ty_data = module.tys.get(ref);
                try writer.print("{}", .{ty_data.display(module)});
            },
            .global => |ref| {
                try writer.print("{}", .{ref});
            },
            else => {
                try writer.print("{s}", .{@tagName(self)});
            },
        }
    }
    pub fn display(self: Ty, module: *const Module) Module.utils.MakeDisplay(Ty, displayFn) {
        return .{ .value = self, .module = module };
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
