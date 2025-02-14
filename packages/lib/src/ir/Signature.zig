const Ty = @import("./ty.zig").Ty;
const Val = @import("./ty.zig").Val;
const utils = @import("./utils.zig");
const std = @import("std");
const Module = @import("./Module.zig");

params: std.ArrayListUnmanaged(Param) = .{},
ret: Ty = Ty.void,

pub const Ref = utils.MakeRef(.sig, u32, "sig{d}");
const Self = @This();

pub const Param = struct {
    ty: Ty,
    is_comptime: bool,
};
pub fn init() Self {
    return .{};
}

pub fn deinit(self: *Self) void {
    self.params.deinit();
}

pub fn addParam(self: *Self, allocator: std.mem.Allocator, param: Ty, is_comptime: bool) !void {
    try self.params.append(allocator, .{ .ty = param, .is_comptime = is_comptime });
}

pub fn setReturn(self: *Self, ret: Ty) void {
    self.ret = ret;
}

const Formatable = struct {
    sig: *Self,
    module: *Module,
    pub fn format(
        self: Formatable,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        try writer.writeAll("(");
        for (self.sig.params.items, 0..) |param, i| {
            if (i > 0) {
                try writer.writeAll(", ");
            }
            if (param.is_comptime) {
                try writer.writeAll("comp ");
            }

            try writer.print("{}: {}", .{ Module.Dfg.Local.Ref.from(@intCast(i)), param.ty.display(self.module) });
        }
        try writer.writeAll(")");
        try writer.print(" -> {}", .{self.sig.ret.display(self.module)});
    }
};
pub fn formatable(self: *Self, module: *Module) Formatable {
    return .{ .sig = self, .module = module };
}
