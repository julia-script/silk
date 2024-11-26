const std = @import("std");
pub const tw = @import("./tw.zig");

r: f64,
g: f64,
b: f64,
a: f64,

style_bold: bool = false,
style_underline: bool = false,

const Color = @This();

pub fn new(r: f64, g: f64, b: f64, a: f64) Color {
    return .{
        .r = r,
        .g = g,
        .b = b,
        .a = a,
    };
}

pub fn bold(self: Color) Color {
    return .{
        .r = self.r,
        .g = self.g,
        .b = self.b,
        .a = self.a,
        .style_bold = true,
        .style_underline = self.style_underline,
    };
}

pub fn brighter(self: Color, amount: f64) Color {
    const factor = 1 + amount;
    return .{
        .r = std.math.clamp(self.r * factor, 0, 1),
        .g = std.math.clamp(self.g * factor, 0, 1),
        .b = std.math.clamp(self.b * factor, 0, 1),
        .a = self.a,
    };
}
// pub fn toHSL(self: Color) HSL {
//     const max = std.math.max(self.r, self.g, self.b);
//     const min = std.math.min(self.r, self.g, self.b);
//     const l = (max + min) / 2;
//     // hue
//     const h = hue: {
//         if (max == min) {
//             break :hue 0;
//         }
//         if (max == self.r) {
//             break :hue 60 * (0 + (self.g - self.b) / (max - min));
//         }
//         if (max == self.g) {
//             break :hue 60 * (2 + (self.b - self.r) / (max - min));
//         }
//         break :hue 60 * (4 + (self.r - self.g) / (max - min));
//     };

//     // saturation
//     const s = if (max == min) 0 else if (l <= 0.5) (max - min) / (max + min) else (max - min) / (2 - max - min);
//     return HSL{ .h = h, .s = s, .l = l };
// }

pub fn underline(self: Color) Color {
    return .{
        .r = self.r,
        .g = self.g,
        .b = self.b,
        .a = self.a,
        .style_bold = self.style_bold,
        .style_underline = true,
    };
}
pub fn toHex(self: Color) u32 {
    return @as(u32, @intFromFloat(self.r * 255)) << 24 //
    | @as(u32, @intFromFloat(self.g * 255)) << 16 //
    | @as(u32, @intFromFloat(self.b * 255)) << 8 //
    | @as(u32, @intFromFloat(self.a * 255));
}
pub fn toU8RGB(self: Color) [3]u8 {
    return [_]u8{ @intFromFloat(self.r * 255), @intFromFloat(self.g * 255), @intFromFloat(self.b * 255) };
}

pub fn rgba(r: f64, g: f64, b: f64, a: f64) Color {
    return new(r, g, b, a);
}

pub fn rgb(r: f64, g: f64, b: f64) Color {
    return new(r, g, b, 1);
}

// hex with or without alpha
pub inline fn hex(comptime h: u32, a: f64) Color {
    comptime {
        return .{
            .r = @as(f64, @floatFromInt(h >> 16 & 255)) / 255,
            .g = @as(f64, @floatFromInt(h >> 8 & 255)) / 255,
            .b = @as(f64, @floatFromInt(h & 255)) / 255,
            .a = a,
            .style_bold = false,
            .style_underline = false,
        };
    }
}

fn csiOpen(self: Color, writer: anytype) !void {
    _ = try writer.write("\x1b[");
    if (self.style_bold) {
        _ = try writer.write("1;");
    }
    if (self.style_underline) {
        _ = try writer.write("4;");
    }
    _ = try writer.write("38;2;");
    try writer.print("{d};{d};{d}", .{
        @as(u8, @intFromFloat(self.r * 255)),
        @as(u8, @intFromFloat(self.g * 255)),
        @as(u8, @intFromFloat(self.b * 255)),
    });
    _ = try writer.write("m");
}
fn csiClose(writer: anytype) !void {
    _ = try writer.write("\x1b[0m");
}
pub fn write(self: Color, writer: anytype, s: []const u8, options: FormatColorOptions) !void {
    if (!options.color) {
        _ = try writer.write(s);
        return;
    }
    try csiOpen(self, writer);
    _ = try writer.write(s);
    try csiClose(writer);
}
pub fn writeChar(self: Color, writer: anytype, b: anytype) !void {
    switch (@TypeOf(b)) {
        u8 => {
            try csiOpen(self, writer);
            try writer.writeByte(b);
            try csiClose(writer);
        },
        u21 => {
            var buf: [4]u8 = undefined;
            const len = try std.unicode.utf8Encode(b, buf[0..]);
            try self.write(writer, buf[0..len]);
        },
        else => @panic("unsupported type"),
    }
}
// return "\x1b[38;2;{d};{d};{d}m{s}\x1b[0m";
pub const FormatColorOptions = struct {
    color: bool = true,
};

pub fn print(
    self: Color,
    writer: anytype,
    comptime format: []const u8,
    args: anytype,
    format_color_options: FormatColorOptions,
) !void {
    if (!format_color_options.color) {
        try writer.print(format, args);
        return;
    }
    try csiOpen(self, writer);
    try writer.print(format, args);
    try csiClose(writer);
}
