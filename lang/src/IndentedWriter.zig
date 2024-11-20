const std = @import("std");
const Self = @This();
const format_utils = @import("format_utils.zig");
const Color = @import("Color.zig");
const tw = Color.tw;
// logger: type,

writer: std.io.AnyWriter,
ind: usize = 0,
pub fn init(writer: std.io.AnyWriter) Self {
    return Self{
        .writer = writer,
    };
}
pub fn writeIndent(self: *Self) !void {
    try format_utils.writeIndent(self.writer, self.ind, .{});
}
pub fn indent(self: *Self) void {
    self.ind += 1;
}
pub fn unindent(self: *Self) void {
    self.ind -= 1;
}
pub fn writeAll(self: *Self, str: []const u8) !void {
    try self.writer.writeAll(str);
}
pub fn writeAllIndented(self: *Self, str: []const u8) !void {
    try self.writeIndent();
    try self.writer.writeAll(str);
}

pub fn print(self: *Self, comptime format: []const u8, args: anytype) !void {
    try self.writer.print(format, args);
}

pub fn printIndented(self: *Self, comptime format: []const u8, args: anytype) !void {
    try self.writeIndent();
    try self.writer.print(format, args);
}

pub fn printLn(self: *Self, comptime format: []const u8, args: anytype) !void {
    try self.writer.print(format ++ "\n", args);
}
pub fn printLnIndented(self: *Self, comptime format: []const u8, args: anytype) !void {
    try self.writeIndent();
    try self.writer.print(format ++ "\n", args);
}
pub fn open(self: *Self, comptime format: []const u8, args: anytype) !void {
    try self.writeIndent();
    try self.writer.print(format ++ " {{\n", args);
    self.indent();
}

pub fn openInline(self: *Self) !void {
    try self.writer.print(" {{\n", .{});
    self.indent();
}

pub fn close(self: *Self) void {
    self.unindent();
    self.writeIndent() catch @panic("writeIndent failed");
    self.writer.print("}}\n", .{}) catch @panic("writeAll failed");
}
pub fn fail(self: *Self, comptime format: []const u8, args: anytype) void {
    self.writeIndent() catch @panic("writeIndent failed");

    tw.red_500.bold().write(self.writer, "[Error] ", .{}) catch @panic("writeAll failed");
    self.writer.print(format ++ "\n", args) catch @panic("writeAll failed");
}
pub fn panic(self: *Self, comptime format: []const u8, args: anytype) noreturn {
    self.fail(format, args);
    @panic("IndentedWriter.panic");
}
pub fn todo(self: *Self, comptime format: []const u8, args: anytype) noreturn {
    self.writeIndent() catch @panic("writeIndent failed");

    tw.yellow_400.bold().write(self.writer, "[TODO] ", .{}) catch @panic("writeAll failed");
    self.writer.print(format ++ "\n", args) catch @panic("writeAll failed");
    @panic("IndentedWriter.todo");
}

pub fn indented(self: *Self, comptime format: []const u8, args: anytype) !void {
    self.indent();
    try self.printLn(format, args);
    self.unindent();
}

pub const Error = std.io.AnyWriter.Error;
