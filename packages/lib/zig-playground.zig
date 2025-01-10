const T = struct {
    a: i32,
};

const std = @import("std");
pub fn main() void {
    const num: i32 = 10;
    const ptr = &num;
    (@constCast(&num)).* = 11;
    std.debug.print("ptr: {}\n", .{&num});
    std.debug.print("num: {}\n", .{num});
    std.debug.print("ptr: {}\n", .{ptr});
    std.debug.print("*ptr: {}\n", .{ptr.*});
    std.debug.print("ptr: {}\n", .{&ptr});
}
