const std = @import("std");
const MainProgram = @import("./cli/MainProgram.zig");
const cmd = @import("cmd");

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var args = try std.process.argsAlloc(allocator);
    const main_program = cmd.program(MainProgram);
    try main_program.call(allocator, args[0..], .{});
}
