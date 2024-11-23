const std = @import("std");
const host = @import("host.zig");
const Compilation = @import("Compilation.zig");

pub fn throw(message: []const u8) noreturn {
    host.hostThrow(message.ptr, message.len);
}

fn panic(message: []const u8, _: ?*std.builtin.StackTrace, _: ?usize) noreturn {
    throw(message);
}

export fn main() void {}
