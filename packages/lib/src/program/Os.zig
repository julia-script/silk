const std = @import("std");
const WriteError = std.fs.File.WriteError;
const ReadError = std.fs.File.OpenError || std.fs.File.ReadError;
const Codegen = @import("../backend/Codegen.zig");

context: *anyopaque,
readFileFn: *const fn (context: *anyopaque, allocator: std.mem.Allocator, path: []const u8) anyerror![]const u8,
writeFileFn: *const fn (context: *anyopaque, path: []const u8, data: []const u8) anyerror!void,

const Self = @This();

pub const Content = struct {
    content: []const u8,
    allocator: std.mem.Allocator,
    pub fn deinit(self: *Content) void {
        self.allocator.free(self.content);
    }
};

pub fn readFile(self: *Self, allocator: std.mem.Allocator, path: []const u8) !Content {
    const content = try self.readFileFn(self.context, allocator, path);
    return Content{
        .content = content,
        .allocator = allocator,
    };
}

pub fn writeFile(self: *Self, path: []const u8, data: []const u8) !void {
    return self.writeFileFn(self.context, path, data);
}

pub const Fs = struct {
    cwd: std.fs.Dir,

    pub fn os(self: *Fs) Self {
        return .{
            .context = self,
            .readFileFn = readFileFn,
            .writeFileFn = writeFileFn,
        };
    }

    pub fn readFileFn(context: *anyopaque, allocator: std.mem.Allocator, path: []const u8) ![]const u8 {
        const self: *Fs = @alignCast(@ptrCast(context));
        const file = try self.cwd.openFile(path, .{});
        defer file.close();
        const content = try file.readToEndAlloc(allocator, std.math.maxInt(usize));
        return content;
    }

    pub fn writeFileFn(context: *anyopaque, path: []const u8, data: []const u8) !void {
        const self: *Fs = @alignCast(@ptrCast(context));
        try self.cwd.writeFile(.{
            .sub_path = path,
            .data = data,
            .flags = .{ .truncate = true },
        });
    }
};

test "Os" {
    var fs = Fs{ .cwd = std.fs.cwd() };
    var os = fs.os();

    const content = try os.readFile(std.testing.allocator, "README.md");
    try os.writeFile("test.txt", content.content);
    std.debug.print("{s}\n", .{content.content});
    content.deinit();

    // var os = fs.os();
    // os.readFile(std.mem.Allocator.init(std.testing.allocator), "test.txt", "test");
}
