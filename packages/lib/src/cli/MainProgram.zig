const std = @import("std");
const cmd = @import("cmd");
const Meta = cmd.Meta;
const program = cmd.program;
const BuildProgram = @import("./BuildProgram.zig");

// pub const MainProgram = program(struct {
// file: []const u8,

pub const metadata: Meta.Program(@This()) = .{
    .name = "silk",
    .description = "Silk CLI",
    .options = .{
        // .entrypoint = Meta.Option{
        //     .display_name = "file",
        //     .description = "The entrypoint file",
        //     .is_argument = true,
        // },
    },
    .commands = &.{program(BuildProgram)},
};

pub fn run(self: @This()) !void {
    _ = self; // autofix
}
