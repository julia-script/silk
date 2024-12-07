const std = @import("std");
const Self = @This();

pub fn makeDir(path: []const u8) !void {
    // const dir = if (self.is_absolute) try std.fs.openDirAbsolute("/", .{}) else std.fs.cwd();
    if (std.fs.path.isAbsolute(path)) {
        var dir = try std.fs.openDirAbsolute("/", .{});
        defer dir.close();
        var dir_path: [std.fs.max_path_bytes]u8 = undefined;
        std.mem.copyForwards(u8, dir_path[1..], path);
        dir_path[0] = '.';

        try dir.makePath(dir_path[0 .. path.len + 1]);
        return;
    }

    try std.fs.cwd().makePath(path);
}

pub fn ensureDir(path: []const u8) !void {
    makeDir(path) catch |err| {
        switch (err) {
            error.PathAlreadyExists => {},
            else => return err,
        }
    };
}
pub fn openDir(path: []const u8, flags: std.fs.Dir.OpenOptions) !std.fs.Dir {
    if (std.fs.path.isAbsolute(path)) {
        return try std.fs.openDirAbsolute(path, flags);
    }
    return try std.fs.cwd().openDir(path, flags);
}
pub inline fn bufJoin(buf: []u8, parts: []const []const u8) ![]const u8 {
    var fba = std.heap.FixedBufferAllocator.init(buf);
    const final_path = try std.fs.path.join(fba.allocator(), parts);
    return final_path;
}
pub inline fn concat(buf: []u8, parts: anytype) []const u8 {
    var i: usize = 0;

    inline for (parts) |part| {
        std.mem.copyForwards(u8, buf[i..], part);
        i += part.len;
    }

    return buf[0..i];
}

test "fs" {
    // const dir_path = "./.tmp/bin/bash";
    // try ensureDir(dir_path);
    // var dir = try openDir(dir_path, .{});
    // defer dir.close();
    // var file = try dir.createFile("heyy.txt", .{});
    // defer file.close();
}
