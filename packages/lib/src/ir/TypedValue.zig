const Module = @import("./Module.zig");
const Value = @import("./value.zig").Value;
const std = @import("std");
const Ty = Module.Ty;

ty: Ty,
value: Value,
is_comptime: bool,

const Self = @This();
pub const Ref = Module.utils.MakeRef(.tyv, Self);
pub const Void = Self{
    .ty = .void,
    .value = .void,
    .is_comptime = true,
};
pub fn init(ty: Ty, value: Value, is_comptime: bool) Self {
    return .{
        .ty = ty,
        .value = value,
        .is_comptime = is_comptime,
    };
}
pub fn Undefined(ty: Ty, is_comptime: bool) Self {
    return .{
        .ty = ty,
        .value = .undefined,
        .is_comptime = is_comptime,
    };
}
pub fn Imm(ty: Ty, value: anytype, is_comptime: bool) Self {
    switch (ty) {
        .i8, .i16, .i32, .i64, .int, .u8, .u16, .u32, .u64, .float, .f32, .f64, .bool => {
            return .{
                .ty = ty,
                .value = Value.fromBytes(ty, value),
                .is_comptime = is_comptime,
            };
        },
        .type => {
            return .{
                .ty = .type,
                .value = .{ .ty = ty },
                // type values are always comptime
                .is_comptime = true,
            };
        },

        else => std.debug.panic("type {} can't be stored as TypedValue.value", .{ty}),
    }
}
pub fn Type(ty: Ty) Self {
    return .{
        .ty = .type,
        .value = .{ .ty = ty },
        .is_comptime = true,
    };
}
pub fn Runtime(ty: Ty, is_comptime: bool) Self {
    return .{
        .ty = ty,
        .value = .{ .runtime = {} },
        .is_comptime = is_comptime,
    };
}
pub fn Local(ty: Ty, local: Module.Dfg.Local.Ref, is_comptime: bool) Self {
    return .{
        .ty = ty,
        .value = .{ .local = local },
        .is_comptime = is_comptime,
    };
}
pub fn Global(ty: Ty, global: Module.Decl.Ref, is_comptime: bool) Self {
    return .{
        .ty = ty,
        .value = .{ .global = global },
        .is_comptime = is_comptime,
    };
}
pub fn Inst(ty: Ty, inst: Module.InstData.Ref, is_comptime: bool) Self {
    return .{
        .ty = ty,
        .value = .{ .inst = inst },
        .is_comptime = is_comptime,
    };
}

pub fn isRuntime(self: Self) bool {
    return switch (self.value) {
        .runtime => true,
        else => false,
    };
}
pub fn isType(self: Self) bool {
    return self.ty == .type;
}

pub fn toTypeTyv(self: Self, module: *const Module) Self {
    _ = module; // autofix
    switch (self.ty) {
        .type => {
            switch (self.value) {
                .ty => |ty| {
                    var tyv = Type(.type);
                    tyv.value = .{ .tyty = ty };
                    return tyv;
                },
                .tyty => {
                    return Type(.type);
                },
                else => {
                    std.debug.panic("type {} can't be stored as TypedValue.value", .{self.ty});
                },
            }
        },
        .u32 => {
            return Type(.u32);
        },
        else => {
            std.debug.panic("type {} can't be stored as TypedValue.value", .{self.ty});
        },
    }
}
pub fn getTypeAsValue(self: Self, module: *const Module) Self {
    switch (self.ty) {
        .type => {
            switch (self.value) {
                .ty => |ty| {
                    return Runtime(ty, false);
                },
                .tyty => |ty| {
                    return Type(ty);
                },
                else => {
                    std.debug.panic("'{}' is not a type", .{self.display(module)});
                },
            }
        },
        else => {
            std.debug.panic("'{}' is not a type", .{self.display(module)});
        },
    }
}
test "getTypeTyv" {
    const a = Type(.u32);
    _ = a; // autofix
    // const b = Type(.type)
    var module = Module.init(std.testing.allocator);
    defer module.deinit();
    var tyv = Imm(.u32, 42, false);
    tyv = tyv.toTypeTyv(&module);
    std.debug.print("{}\n", .{tyv.display(&module)});
    const tyv2 = tyv.toTypeTyv(&module);
    std.debug.print("{}\n", .{tyv2.display(&module)});

    tyv = Type(.{ .global = .{ .ref = 12 } });
    std.debug.print("{}\n", .{tyv.display(&module)});
    tyv = tyv.toTypeTyv(&module);
    std.debug.print("{}\n", .{tyv.display(&module)});
}

fn displayFn(self: Self, writer: std.io.AnyWriter, module: *const Module) anyerror!void {
    switch (self.value) {
        .bytes => {
            switch (self.ty) {
                .i8, .i16, .i32, .i64, .int => {
                    try writer.print("{}{{ {d} }}", .{ self.ty.display(module), self.value.readAs(i64) });
                },
                .u8, .u16, .u32, .u64 => {
                    try writer.print("{}{{ {d} }}", .{ self.ty.display(module), self.value.readAs(u64) });
                },
                .f32, .f64, .float => {
                    try writer.print("{}{{ {d} }}", .{ self.ty.display(module), self.value.readAs(f64) });
                },
                else => {
                    try writer.print("{}{{ {} }}", .{ self.ty.display(module), self.value.readAs(bool) });
                },
            }
        },
        .void => {
            try writer.print("{}", .{self.ty.display(module)});
        },
        .runtime => {
            if (self.is_comptime) {
                try writer.print("{}{{ comptime }}", .{self.ty.display(module)});
            } else {
                try writer.print("{}{{ runtime }}", .{self.ty.display(module)});
            }
        },
        .inst => |ref| {
            try writer.print("{}{{ {} }}", .{ self.ty.display(module), ref });
        },
        .global => |ref| {
            try writer.print("{}{{ {} }}", .{ self.ty.display(module), ref });
        },
        .undefined => {
            try writer.print("{}{{ undefined }}", .{self.ty.display(module)});
        },
        .local => |ref| {
            try writer.print("{}{{ {} }}", .{ self.ty.display(module), ref });
        },
        .ty => {
            try writer.print("Type{{ {} }}", .{self.value.ty.display(module)});
        },
        .tyty => {
            try writer.print("Type{{ {} }}", .{self.getTypeAsValue(module).display(module)});
        },
        .ref => |ref| {
            try writer.print("{}{{ {} }}", .{ self.ty.display(module), ref });
        },

        else => {
            try writer.print("TODO", .{});
        },
    }
}
pub fn display(self: Self, module: *const Module) Module.utils.MakeDisplay(Self, displayFn) {
    return .{ .value = self, .module = module };
}

test "TypedValue" {
    var module = Module.init(std.testing.allocator);
    defer module.deinit();
    var buf: [1024]u8 = undefined;
    const fbo = std.io.fixedBufferStream(&buf);

    var ty: Ty = .f32;
    var value: f32 = 3.14;
    _ = &ty;
    _ = &value;
    // @compileLog(ty, value);
    _ = fbo; // autofix
    const val = Imm(ty, value, false);
    const runtime_val = Runtime(ty, false);
    const inst_val = Inst(ty, .{ .ref = 12 }, false);
    // try fbo.writer().print("{}", .{self});
    std.debug.print("{}\n", .{val.display(&module)});
    std.debug.print("{}\n", .{runtime_val.display(&module)});
    std.debug.print("{}\n", .{inst_val.display(&module)});
    // try display(self, fbo.writer().any(), &module);
    // std.testing.expectEqual(buf, "42");
}
