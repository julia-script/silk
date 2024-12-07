const std = @import("std");
const Self = @This();
const format_utils = @import("format_utils.zig");
const Color = @import("Color.zig");
const tw = Color.tw;
const config = @import("options");
const builtin = @import("builtin");
// logger: type,

writer: std.io.AnyWriter,
scope: []const u8,
enabled: bool,
ind: usize = 0,
pub fn init(writer: std.io.AnyWriter, scope: []const u8) Self {
    var enabled = false;
    for (config.log_scopes) |s| {
        if (std.mem.eql(u8, s, scope)) {
            enabled = true;
            break;
        }
    }
    return Self{
        .writer = writer,
        .scope = scope,
        .enabled = enabled,
    };
}
pub fn writeIndent(self: *Self) !void {
    if (builtin.target.isWasm()) return;
    if (!self.enabled) return;
    try format_utils.writeIndent(self.writer, self.ind, .{});
}
pub fn indent(self: *Self) void {
    if (builtin.target.isWasm()) return;
    if (!self.enabled) return;
    self.ind += 1;
}
pub fn unindent(self: *Self) void {
    if (builtin.target.isWasm()) return;
    if (!self.enabled) return;
    self.ind -= 1;
}
pub fn writeAll(self: *Self, str: []const u8) !void {
    if (builtin.target.isWasm()) return;
    if (!self.enabled) return;
    try self.writer.writeAll(str);
}
pub fn writeAllIndented(self: *Self, str: []const u8) !void {
    if (builtin.target.isWasm()) return;
    if (!self.enabled) return;
    try self.writeIndent();
    try self.writer.writeAll(str);
}

pub fn print(self: *Self, comptime format: []const u8, args: anytype) !void {
    if (builtin.target.isWasm()) return;
    try self.writer.print(format, args);
}

pub fn printIndented(self: *Self, comptime format: []const u8, args: anytype) !void {
    if (builtin.target.isWasm()) return;
    if (!self.enabled) return;
    try self.writeIndent();
    try self.writer.print(format, args);
}

pub fn printLn(self: *Self, comptime format: []const u8, args: anytype) !void {
    if (builtin.target.isWasm()) return;
    if (!self.enabled) return;
    try self.writer.print(format ++ "\n", args);
}
pub fn printLnIndented(self: *Self, comptime format: []const u8, args: anytype) !void {
    if (builtin.target.isWasm()) return;
    if (!self.enabled) return;
    try self.writeIndent();
    try self.writer.print(format ++ "\n", args);
}

pub fn log(self: *Self, comptime format: []const u8, args: anytype, color: ?Color) void {
    if (builtin.target.isWasm()) return;
    if (!self.enabled) return;
    // self.indent();
    self.writeIndent() catch @panic("writeIndent failed");
    if (color) |c| {
        c.print(self.writer, format ++ "\n", args, .{}) catch @panic("writeAll failed");
    } else {
        self.writer.print(format ++ "\n", args) catch @panic("writeAll failed");
    }
    // self.unindent();
}
pub fn open(self: *Self, comptime format: []const u8, args: anytype) void {
    if (builtin.target.isWasm()) return;
    if (!self.enabled) return;
    self.writeIndent() catch {};
    self.writer.print(format ++ " {{\n", args) catch @panic("writeAll failed");
    self.indent();
}

pub fn openInline(self: *Self) !void {
    if (builtin.target.isWasm()) return;
    if (!self.enabled) return;
    try self.writer.print(" {{\n", .{});
    self.indent();
}

pub fn close(self: *Self) void {
    if (builtin.target.isWasm()) return;
    if (!self.enabled) return;
    self.unindent();
    self.writeIndent() catch @panic("writeIndent failed");
    self.writer.print("}}\n", .{}) catch @panic("writeAll failed");
}
pub fn fail(self: *Self, comptime format: []const u8, args: anytype) void {
    if (builtin.target.isWasm()) return;
    self.writeIndent() catch @panic("writeIndent failed");

    tw.red_500.bold().write(self.writer, "[Error] ", .{}) catch @panic("writeAll failed");
    self.writer.print(format ++ "\n", args) catch @panic("writeAll failed");
}
pub fn panic(self: *Self, comptime format: []const u8, args: anytype) noreturn {
    self.fail(format, args);
    @panic("Logger.panic");
}
pub fn todo(self: *Self, comptime format: []const u8, args: anytype) noreturn {
    if (builtin.target.isWasm()) @panic("Logger.todo");
    self.writeIndent() catch @panic("writeIndent failed");

    tw.yellow_400.bold().write(self.writer, "[TODO] ", .{}) catch @panic("writeAll failed");
    self.writer.print(format ++ "\n", args) catch @panic("writeAll failed");
    @panic("Logger.todo");
}

pub fn indented(self: *Self, comptime format: []const u8, args: anytype) !void {
    if (builtin.target.isWasm()) return;
    if (!self.enabled) return;
    self.indent();
    try self.printLn(format, args);
    self.unindent();
}

pub const Error = std.io.AnyWriter.Error;
