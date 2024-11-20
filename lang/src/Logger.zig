const std = @import("std");
const Self = @This();
const format_utils = @import("format_utils.zig");
// logger: type,

ind: usize = 0,
pub fn init(comptime scope: @Type(.enum_literal)) Self {
    _ = scope; // autofix

    // return std.log.scoped(.{ .scope = scope });
    return Self{
        // .logger = std.log.scoped(scope),
    };
}
fn indent(self: *Self) void {
    self.ind += 1;
}
fn unindent(self: *Self) void {
    self.ind -= 1;
}
pub fn writeIndent(self: *Self) void {
    format_utils.writeIndent(std.io.getStdErr().writer().any(), self.ind, .{
        .indent_guides = true,
    }) catch unreachable;
    // for (0..self.ind) |_| {
    //     // std.debug.print("  ", .{});
    //     // std.debug.print("  ", .{});

    // }
}
pub fn ln(self: *Self, comptime format: []const u8, args: anytype) void {
    self.writeIndent();
    // self.logger.info(format, args);
    std.debug.print(format ++ "\n", args);
}
pub fn open(self: *Self, comptime format: []const u8, args: anytype) void {
    self.writeIndent();
    std.debug.print(format ++ " {{\n", args);
    // self.logger.info(format ++ "{\n", args);
    self.indent();
}

pub fn openInline(self: *Self) void {
    std.debug.print(" {{\n", .{});
    self.indent();
}
pub fn close(self: *Self) void {
    self.unindent();
    self.writeIndent();
    std.debug.print("}}\n", .{});
}

pub fn indented(self: *Self, comptime format: []const u8, args: anytype) void {
    self.indent();
    self.ln(format, args);
    self.unindent();
}
