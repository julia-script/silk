const std = @import("std");

pub const Op = enum {
    load,
    store,

    add,
    sub,
    mul,
    div,

    // iadd,
    // isub,
    // imul,
    // idiv,

    // fadd,
    // fsub,
    // fmul,
    // fdiv,

    eq,
    ne,

    lt,
    ge,
    gt,
    le,

    // lt_u,
    // ge_u,
    // gt_u,
    // le_u,

    // lt_s,
    // ge_s,
    // gt_s,
    // le_s,

    call,
    ret,

    set_local,

    typeof,
    cast,
    @"break",
    @"return",

    pub fn format(
        self: Op,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        try writer.print("{s}", .{@tagName(self)});
    }
};
