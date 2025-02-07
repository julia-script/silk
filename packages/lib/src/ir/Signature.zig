const Ty = @import("./tyval.zig").Ty;
const Val = @import("./tyval.zig").Val;
const utils = @import("./utils.zig");
const std = @import("std");
const Module = @import("./Module.zig");

params: std.ArrayList(Param),
ret: Ty = Ty.void,

pub const Ref = utils.MakeRef(.sig, u32);
const Self = @This();

pub const Param = struct {
    ty: Ty,
    is_comptime: bool,
};
pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .params = std.ArrayList(Param).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    self.params.deinit();
}

pub fn addParam(self: *Self, param: Ty, is_comptime: bool) !void {
    try self.params.append(.{ .ty = param, .is_comptime = is_comptime });
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

            try writer.print("l{d}: {s}", .{ i, param.ty });
        }
        try writer.writeAll(")");
        try writer.print(" -> {s}", .{self.sig.ret});
    }
};
pub fn formatable(self: *Self, module: *Module) Formatable {
    return .{ .sig = self, .module = module };
}
