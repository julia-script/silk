const std = @import("std");
const cmd = @import("cmd");
const Meta = cmd.Meta;
const program = cmd.program;
const Compilation = @import("../Compilation.zig");

// pub const MainProgram = program(struct {
file: []const u8,

pub const metadata: Meta.Program(@This()) = .{
    .name = "build",
    .description = "Build a Silk project",
    .options = .{
        .file = Meta.Option{
            // .display_name = "file",
            .description = "The entrypoint file",
            .is_argument = true,
        },
    },
};

pub fn run(self: @This()) !void {
    std.debug.print("Building {s}\n", .{self.file});
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    // std.fs.cwd().writeFile(.{
    //     .sub_path = "hello.txt",
    //     .data = "hello",
    // }) catch |err| {
    //     std.debug.print("Error: {}\n", .{err});
    //     // wasm
    // };

    var compilation = try Compilation.init(allocator, self.file);
    defer compilation.deinit();
    try compilation.compile();
    // try compilation.compile();
}
