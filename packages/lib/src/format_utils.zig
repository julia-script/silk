const std = @import("std");
const Color = @import("Color.zig");
const tw = Color.tw;

pub const RAINBOW = [7]Color{
    tw.red_200,
    tw.orange_200,
    tw.yellow_200,
    tw.green_200,
    tw.blue_200,
    tw.indigo_200,
    tw.violet_200,
};
pub fn pickColor(index: usize) Color {
    return RAINBOW[index % RAINBOW.len];
}

pub const IndentOptions = struct {
    rainbow: bool = true,
    size: usize = 2,
    indent_guides: bool = true,
    indent_guide_char: u21 = '┆',
};
pub fn writeIndent(writer: std.io.AnyWriter, indent: usize, options: IndentOptions) !void {
    if (!options.indent_guides) {
        try writer.writeByteNTimes(' ', indent * options.size);
        return;
    }
    var buf: [4]u8 = undefined;
    const len = try std.unicode.utf8Encode(options.indent_guide_char, buf[0..]);
    const indent_guide: []u8 = buf[0..len];
    // if (options.rainbow) {
    for (0..indent) |color| {
        try RAINBOW[color % RAINBOW.len]
            .brighter(-0.3)
            .write(writer, @constCast(indent_guide), .{ .color = options.rainbow });
        try writer.writeByteNTimes(' ', options.size - 1);
    }
    // } else {
    //     for (0..indent) |_| {
    //         _ = try writer.write(indent_guide);
    //         try writer.writeByteNTimes(' ', options.size - 1);
    //     }
    // }
}
