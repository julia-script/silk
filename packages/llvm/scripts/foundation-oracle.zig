const std = @import("std");

pub fn main(init: std.process.Init) !void {
    const allocator = init.arena.allocator();
    var builder = try std.zig.llvm.Builder.init(.{
        .allocator = allocator,
        .name = "test.ll",
        .triple = "aarch64-apple-darwin",
    });
    defer builder.deinit();

    var assembly: std.Io.Writer.Allocating = .init(allocator);
    try assembly.writer.writeAll("nop");
    try builder.finishModuleAsm(&assembly);

    const words = try builder.toBitcode(allocator, .{
        .name = "silk-effect",
        .version = .{ .major = 0, .minor = 0, .patch = 0 },
    });

    var output_buffer: [4096]u8 = undefined;
    var output = std.Io.File.stdout().writer(init.io, &output_buffer);
    try output.interface.writeAll(std.mem.sliceAsBytes(words));
    try output.interface.flush();
}
