const std = @import("std");

max_depth: usize = 128,
writer: std.io.AnyWriter,

indents: std.BoundedArray(bool, 1024),
pub fn init(writer: std.io.AnyWriter) @This() {
    return .{
        .writer = writer,

        .indents = std.BoundedArray(bool, 1024).init(0) catch unreachable,
    };
}
pub fn pushBlank(self: *@This()) !void {
    if (self.indents.len >= self.max_depth) return error.MaxDepthReached;
    try self.indents.append(false);
}
pub fn pushDirLine(self: *@This()) !void {
    if (self.indents.len >= self.max_depth) return error.MaxDepthReached;
    try self.indents.append(true);
}
pub fn pop(self: *@This()) !void {
    // std.debug.print("pop[{d}]\n", .{self.indents.len});
    _ = self.indents.pop();
}
pub const Kind = enum {
    item,
    last_item,
};
pub fn writeIndent(self: *@This(), is_item: bool, is_last: bool) !void {
    try self.writeIndentTo(self.writer, is_item, is_last);
}
pub fn writeIndentTo(self: *@This(), writer: std.io.AnyWriter, is_item: bool, is_last: bool) !void {
    for (self.indents.slice(), 0..) |indent, i| {
        const is_last_indent = i == self.indents.slice().len - 1;
        if (indent) {
            if (is_item and is_last_indent) {
                if (is_last) {
                    try writer.writeAll(" └ ");
                    self.indents.buffer[self.indents.len - 1] = false;
                    // try self.pop();
                    // try self.pushBlank();
                } else {
                    try writer.writeAll(" ├ ");
                }
            } else {
                try writer.writeAll(" │ ");
            }
        } else {
            try writer.writeAll("   ");
        }
    }
}
