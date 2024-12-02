const Env = @import("host.zig").Env;
const host = @import("host.zig");
const std = @import("std");

pub const OsFile = struct {
    // content: Content,
    path: []const u8,
    os: *Os,
    virtual: bool,
    pub const Content = union(enum) {
        virtual: *std.ArrayListUnmanaged(u8),
        system: std.fs.File,
    };

    pub fn close(self: *OsFile) void {
        if (self.os.virtual or comptime host.env == .wasm) {
            // self.os.contents.getPtr(self.path).?.deinit(self.os.allocator);
            // _ = self.os.contents.remove(self.path);
        } else {
            self.os.system_files.getPtr(self.path).?.close();
            _ = self.os.system_files.remove(self.path);
        }
        _ = self.os.fs.remove(self.path);
    }
};

pub const Os = struct {
    fs: std.StringHashMapUnmanaged(OsFile) = .{},
    contents: std.StringHashMapUnmanaged(std.ArrayListUnmanaged(u8)) = .{},
    system_files: std.StringHashMapUnmanaged(std.fs.File) = .{},
    allocator: std.mem.Allocator,
    virtual: bool = false,
    pub fn init(allocator: std.mem.Allocator, comptime virtual: bool) Os {
        return .{
            .allocator = allocator,
            .virtual = virtual,
        };
    }
    pub fn deinit(self: *Os) void {
        var v = self.fs.valueIterator();
        while (v.next()) |file| {
            file.close();
        }
        var iter = self.contents.valueIterator();
        while (iter.next()) |content| {
            content.deinit(self.allocator);
        }
        self.fs.deinit(self.allocator);
        self.contents.deinit(self.allocator);
        self.system_files.deinit(self.allocator);
        // var it = self.fs.iterator();
    }
    pub fn fileExists(self: *Os, path: []const u8) bool {
        if (self.virtual or comptime host.env == .wasm) {
            return self.fs.contains(path);
        }
        return std.fs.cwd().access(path, .{}) == .result;
    }
    pub fn createVirtualFile(self: *Os, path: []const u8) !*OsFile {
        const file = try self.fs.getOrPut(self.allocator, path);
        if (file.found_existing) {
            return error.FileAlreadyExists;
        }

        file.value_ptr.* = OsFile{
            .path = file.key_ptr.*,
            .os = self,
            .virtual = true,
        };
        return file.value_ptr;
    }
    pub fn createSystemFile(self: *Os, path: []const u8) !*OsFile {
        if (comptime host.env == .wasm) {
            return error.CannotCreateSystemFileInWasm;
        }
        const file = try self.fs.getOrPut(self.allocator, path);

        if (file.found_existing) {
            return error.FileAlreadyExists;
        }

        const system_file = try self.system_files.getOrPut(self.allocator, path);
        system_file.value_ptr.* = try std.fs.cwd().createFile(path, .{});
        // const end_pos = try system_file.value_ptr.*.getEndPos();
        // try system_file.value_ptr.*.seekTo(end_pos);
        file.value_ptr.* = OsFile{
            .path = file.key_ptr.*,
            .os = self,
            .virtual = false,
        };

        return file.value_ptr;
    }
    pub fn createFile(self: *Os, path: []const u8) !*OsFile {
        if (self.virtual or comptime host.env == .wasm) {
            return self.createVirtualFile(path);
        }
        return self.createSystemFile(path);
    }

    pub inline fn getVirtualFileWriter(self: *Os, path: []const u8) !std.io.AnyWriter {
        // const file = self.contents.getPtr(path) orelse return error.FileNotFound;
        const content = try self.contents.getOrPut(self.allocator, path);
        if (!content.found_existing) {
            content.value_ptr.* = try std.ArrayListUnmanaged(u8).initCapacity(self.allocator, 128);
        }
        return content.value_ptr.writer(self.allocator).any();
    }

    pub inline fn getSystemFileWriter(self: *Os, path: []const u8) !std.io.AnyWriter {
        if (comptime host.env == .wasm) {
            return error.CannotCreateSystemFileInWasm;
        }
        const file = try self.system_files.getOrPut(self.allocator, path);
        if (!file.found_existing) {
            file.value_ptr.* = try std.fs.cwd().createFile(path, .{});
        }
        return file.value_ptr.writer().any();
    }
    pub inline fn getFileWriter(self: *Os, path: []const u8) !std.io.AnyWriter {
        if (self.virtual or comptime host.env == .wasm) {
            return self.getVirtualFileWriter(path);
        }
        return self.getSystemFileWriter(path);
    }
    pub fn dump(self: *Os, writer: std.io.AnyWriter) !void {
        _ = writer; // autofix
        var it = self.fs.iterator();
        while (it.next()) |entry| {
            if (entry.value_ptr.virtual) {
                std.debug.print("virtual: ", .{});
                std.debug.print("{s} ", .{entry.key_ptr.*});
                const content = self.contents.getPtr(entry.key_ptr.*).?;
                std.debug.print("({d} bytes)\n{s}\n", .{ content.items.len, content.items });
            } else {
                std.debug.print("system: ", .{});
                std.debug.print("{s}\n", .{entry.key_ptr.*});
            }
        }
    }
};

test "Os" {
    const allocator = std.testing.allocator;
    var os = Os.init(allocator, false);
    defer os.deinit();
    const file = try os.createFile("test.txt");
    const writer = try os.getFileWriter(file.path);
    try writer.writeAll("Hello, iuuuu!");
    // const writer2 = try os.getFileWriter(file.path);

    // try writer2.writeAll("Hello, world!");
    try os.dump(std.io.getStdErr().writer().any());

    // try file.writer().writeAll("Hello, world!");
}
