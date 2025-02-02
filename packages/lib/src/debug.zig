const std = @import("std");

pub fn assertPrint(
    ok: bool,
    comptime fmt: []const u8,
    args: anytype,
) void {
    if (!ok) {
        std.debug.panic(fmt, args);
    }
}
pub fn assert(
    ok: bool,
    msg: []const u8,
) void {
    if (!ok) {
        @panic(msg);
    }
}
