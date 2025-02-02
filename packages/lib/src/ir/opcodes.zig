const std = @import("std");

pub const Op = enum {
    load,
    store,

    add,
    sub,
    mul,
    div,

    iadd,
    isub,
    imul,
    idiv,

    call,
    ret,

    pub fn format(
        self: Op,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        try writer.print("{s}", .{@tagName(self)});
    }
};
