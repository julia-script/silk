const std = @import("std");
pub fn fmt(ok: bool, comptime fmt_: []const u8, args: anytype) void {
    if (!ok) {
        std.debug.panic(fmt_, args);
    }
}
