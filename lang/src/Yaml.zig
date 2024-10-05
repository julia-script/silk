const std = @import("std");
const Yaml = @This();

writer: std.io.AnyWriter,
indent: usize = 0,
is_new_line: bool = true,

pub fn init(writer: std.io.AnyWriter) !Yaml {
    return .{
        .writer = writer,
    };
}

fn writeIndent(self: *Yaml) !void {
    try self.writer.writeByteNTimes(' ', self.indent * 2);
}

test "yaml" {
    const allocator = std.testing.allocator;
    var text = std.ArrayList(u8).init(allocator);
    defer text.deinit();
    const writer = text.writer().any();

    var yaml = try Yaml.init(writer);
    _ = yaml; // autofix

    std.debug.print("{s}\n", .{text.items});
}
