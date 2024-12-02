const std = @import("std");

const Allocator = std.mem.Allocator;
const Array = std.ArrayListUnmanaged;
const HashMap = std.AutoHashMapUnmanaged;
const builtin = @import("builtin");
pub const PATH_SEPARATOR = @as(u8, std.fs.path.sep);
const MAX_PATH_BYTES = if (builtin.target.isWasm()) 1024 else std.fs.max_path_bytes;
pub const VirtualFs = Fs(true);
pub fn Fs(comptime virtual: bool) type {
    return struct {
        // Only used for virtual fs
        // const EntryHashMap = if (virtual) HashMap(Entry.Index) else void;
        // const EntryList = if (virtual) Array(Entry) else void;
        const Dir = if (virtual) void else std.fs.Dir;
        // entries: EntryList,
        entry_map: HashMap(Entry.KeyHash, Entry) = .{},
        // index: EntryHashMap,
        allocator: Allocator,

        cwd: Dir,
        const Self = @This();

        // pub fn pushEntry(self: *Self, entry: Entry) !Entry.Index {
        //     if (!virtual) @compileError("Virtual fs only");
        //     const index = self.entries.items.len;

        //     try self.entries.append(self.allocator, entry);
        //     return index;
        // }
        pub inline fn putEntry(self: *Self, entry: Entry) !FsPath {
            if (!virtual) @compileError("Virtual fs only");
            var path: *FsPath = switch (entry) {
                .file => |*file| &@constCast(file).path,
                .dir => |*dir| &@constCast(dir).path,
            };
            const entry_ptr = try self.entry_map.getOrPut(self.allocator, path.hash());
            if (entry_ptr.found_existing) {
                return error.EntryAlreadyExists;
            }
            entry_ptr.value_ptr.* = entry;
            return path.clone();
        }

        pub inline fn init(allocator: Allocator, cwd: []const u8) !Self {
            if (virtual) {
                var fs: Self = .{ .allocator = allocator, .cwd = {} };
                _ = try fs.putEntry(.{ .dir = .{ .path = FsPath.init(), .fs = &fs } });
                return fs;
            } else {
                const cwd_dir = blk: {
                    if (std.fs.path.isAbsolute(cwd)) {
                        break :blk try std.fs.openDirAbsolute(cwd, .{});
                    } else {
                        _ = try std.fs.cwd().makePath(cwd);
                        break :blk try std.fs.cwd().openDir(cwd, .{});
                    }
                };
                return .{ .cwd = cwd_dir, .entry_map = .{}, .allocator = allocator };
            }
        }
        pub fn deinit(self: *Self) void {
            if (comptime virtual) {
                var root = FsPath.init();
                const root_entry = self.entry_map.getPtr(root.hash()) orelse @panic("root not found");
                root_entry.deinit();
            } else {
                var iter = self.entry_map.iterator();
                while (iter.next()) |entry| {
                    entry.value_ptr.*.deinit();
                }
            }
            // self.entry_map.
            // while (iter.next()) |entry| {
            //     entry.value_ptr.*.deinit();
            // }

            self.entry_map.deinit(self.allocator);
        }

        pub const FsFile = struct {

            // const Handle = if (virtual) std.fs.File else std.ArrayListUnmanaged(u8);
            const Handle = union(enum) {
                closed: void,
                system: std.fs.File,
                virtual: std.ArrayListUnmanaged(u8),
            };
            path: FsPath,
            handle: Handle = .closed,
            fs: *Self,
            pub fn deinit(self: *FsFile) void {
                switch (self.handle) {
                    .closed => {},
                    .system => |*file| if (!virtual) file.close(),
                    .virtual => |*list| list.deinit(self.fs.allocator),
                }
            }
            pub inline fn getWriter(self: *FsFile) !std.io.AnyWriter {
                return switch (self.handle) {
                    .closed => {
                        if (comptime virtual) {
                            self.handle = .{ .virtual = .{} };
                            return self.handle.virtual.writer(self.fs.allocator).any();
                        } else {
                            const file: std.fs.File = try self.fs.cwd.openFile(self.path.slice(), .{
                                .mode = .read_write,
                            });

                            std.debug.print("open file {any}\n", .{file});

                            self.handle = .{ .system = file };
                            return file.writer().any();
                        }
                    },
                    .system => |*file| {
                        return file.writer().any();
                    },
                    .virtual => |*list| list.writer(self.fs.allocator).any(),
                };
            }
            const Content = struct {
                content: []u8,
                allocator: Allocator,
                pub fn deinit(self: *Content) void {
                    self.allocator.free(self.content);
                }
            };
            pub fn readAlloc(self: *FsFile, allocator: Allocator) !Content {
                return switch (self.handle) {
                    .closed => {
                        if (comptime virtual) {
                            self.handle = .{ .virtual = .{} };
                            return try self.readAlloc(allocator);
                        } else {
                            self.handle = .{ .system = try self.fs.cwd.openFile(self.path.slice(), .{ .mode = .read_write }) };
                            return try self.readAlloc(allocator);
                        }
                    },
                    .system => |*file| {
                        try file.seekTo(0);
                        const len = try file.getEndPos();
                        var array = try std.ArrayList(u8).initCapacity(allocator, len);
                        array.items.len = len;
                        const bytes = try file.readAll(array.items[0..len]);
                        std.debug.assert(bytes == len);

                        return Content{ .content = array.toOwnedSlice(), .allocator = allocator };
                    },
                    .virtual => |*list| {
                        return Content{ .content = try allocator.dupe(u8, list.items), .allocator = allocator };
                    },
                };
            }
            pub fn readToArray(self: *FsFile, allocator: Allocator) !std.ArrayList(u8) {
                return switch (self.handle) {
                    .closed => {
                        if (comptime virtual) {
                            self.handle = .{ .virtual = .{} };
                            return try self.readToArray(allocator);
                        } else {
                            self.handle = .{ .system = try self.fs.cwd.openFile(self.path.slice(), .{ .mode = .read_write }) };
                            return try self.readToArray(allocator);
                        }
                    },
                    .system => |*file| {
                        try file.seekTo(0);
                        const len = try file.getEndPos();
                        var array = try std.ArrayList(u8).initCapacity(allocator, len);
                        array.items.len = len;
                        const bytes = try file.readAll(array.items[0..len]);
                        std.debug.assert(bytes == len);

                        return array;
                    },
                    .virtual => |*list| {
                        var array = std.ArrayList(u8).init(allocator);
                        try array.appendSlice(list.items);
                        return array;
                    },
                };
            }
        };

        pub const FsDir = struct {
            path: FsPath,
            entries: std.AutoHashMapUnmanaged(Entry.KeyHash, void) = .{},
            fs: *Self,
            pub fn deinit(self: *FsDir) void {
                // Remove all children
                var iter = self.entries.iterator();
                while (iter.next()) |iter_entry| {
                    var child: *Entry = self.fs.entry_map.getPtr(iter_entry.key_ptr.*) orelse @panic("child not found");
                    child.deinit();
                }

                // delete entry
                self.entries.deinit(self.fs.allocator);
            }
        };
        const EntryType = enum {
            file,
            dir,
        };
        pub const Entry = union(EntryType) {
            file: FsFile,
            dir: FsDir,
            const Key = []const u8;
            const KeyHash = u64;
            pub const Index = usize;
            pub fn deinit(self: *Entry) void {
                switch (self.*) {
                    .dir => |*dir| {
                        dir.deinit();
                    },
                    .file => |*file| {
                        file.deinit();
                    },
                }
            }
        };
        pub fn getEntry(self: *Self, path: *FsPath) !*Entry {
            const entry = self.entry_map.getPtr(path.hash()) orelse return error.EntryNotFound;
            return entry;
        }

        // pub fn getVirtualDir(self: *Self, path: []const u8) !Entry.Index {
        //     var iter = std.mem.splitScalar(u8, dirPath(path), PATH_SEPARATOR);
        //     var current_index: Entry.Index = 0;
        //     while (iter.next()) |segment| {
        //         if (segment.len == 0) continue;
        //         const branch = self.entries.items[current_index];
        //         switch (branch) {
        //             .file => return error.NotADir,
        //             .dir => {
        //                 const branch_index = branch.dir.entries.get(segment);
        //                 if (branch_index) |index| {
        //                     current_index = index;
        //                     continue;
        //                 }
        //                 return error.DirNotFound;
        //             },
        //         }
        //     }
        //     return current_index;
        // }
        pub fn getVirtualFile(self: *Self, path: []const u8) !Entry.Index {
            const dir_index = try self.getVirtualDir(path);
            const file_name = std.fs.path.basename(path);
            const file_index = self.entries.items[dir_index].dir.entries.get(file_name) orelse return error.FileNotFound;
            return file_index;
        }
        pub fn fileExists(self: *Self, path: []const u8) bool {
            if (comptime virtual) {
                _ = self.getVirtualFile(path) catch return false;
                return true;
            } else {
                return error.NotImplemented;
            }
        }

        fn makeVirtualDir(self: *Self, path: *FsPath) !void {
            if (!comptime virtual) @compileError("Virtual fs only");

            var path_iter = path.iter();

            var file_path = FsPath.init();

            while (path_iter.next()) |segment| {
                if (segment.len == 0 or std.mem.eql(u8, segment, ".")) continue;
                var branch = try self.getEntry(&file_path);
                switch (branch.*) {
                    .file => return error.NotADir,
                    .dir => {},
                }

                try file_path.append(segment);
                const hash = file_path.hash();
                if (self.entry_map.contains(hash)) {
                    continue;
                }

                const new_branch = try self.putEntry(.{ .dir = .{ .path = try file_path.clone(), .fs = self } });
                _ = new_branch; // autofix

                try branch.dir.entries.put(
                    self.allocator,
                    hash,
                    {},
                );
            }
        }

        fn makeSystemDir(self: *Self, path: *FsPath) !void {
            if (comptime virtual) @compileError("System fs only");
            const path_slice = path.slice();
            if (std.fs.path.isAbsolute(path_slice)) {
                try std.fs.makeDirAbsolute(path_slice);
            } else {
                try self.cwd.makePath(path_slice);
            }
        }
        pub fn makeDir(self: *Self, path: []const u8) !FsPath {
            var file_path = try FsPath.fromSlice(path);

            if (comptime virtual) {
                try self.makeVirtualDir(&file_path);
            } else {
                try self.makeSystemDir(&file_path);
            }
            return file_path;
        }

        fn makeSystemFile(self: *Self, path: *FsPath) !void {
            if (comptime virtual) @compileError("System fs only");
            var dir = path.dir();
            try self.makeSystemDir(&dir);
            // const system_dir = try self.cwd.openDir(dir.slice(), .{});
            const file = try self.cwd.createFile(path.slice(), .{});
            _ = file; // autofix
        }
        fn makeVirtualFile(self: *Self, path: *FsPath) !void {
            if (!comptime virtual) @compileError("Virtual fs only");
            if (path.dir()) |dir_path_ptr| {
                const dir_path = @constCast(&dir_path_ptr);
                try self.makeVirtualDir(dir_path);
                const dir = try self.getEntry(dir_path);
                try dir.dir.entries.put(self.allocator, path.hash(), {});
            }
            _ = try self.putEntry(.{ .file = .{ .path = try path.clone(), .fs = self } });
        }
        pub fn makeFile(self: *Self, path: []const u8) !FsPath {
            var file_path = try FsPath.fromSlice(path);
            if (comptime virtual) {
                try self.makeVirtualFile(&file_path);
            } else {
                try self.makeSystemFile(&file_path);
            }
            return file_path;
        }

        const Indent = struct {
            indent_path: std.BoundedArray(IndentKind, 1024) = std.BoundedArray(IndentKind, 1024).init(0) catch unreachable,
            const IndentKind = enum {
                blank,
                vertical,
                bottomLeftCorner,

                pub fn write(self: IndentKind, writer: std.io.AnyWriter) !void {
                    switch (self) {
                        .blank => try writer.writeAll("  "),
                        .vertical => try writer.writeAll("│ "),
                        .bottomLeftCorner => try writer.writeAll("└ "),
                    }
                }
            };
            pub fn write(self: *Indent, writer: std.io.AnyWriter) !void {
                for (self.indent_path.slice()) |indent_kind| {
                    try indent_kind.write(writer);
                }
            }

            pub fn indent(self: *Indent, kind: IndentKind) !void {
                try self.indent_path.append(kind);
            }
            pub fn dedent(self: *Indent) void {
                _ = self.indent_path.pop();
            }
        };
        fn dumpInner(self: *Self, writer: std.io.AnyWriter, entry_path: *FsPath, indent: *Indent) !void {
            const hash = entry_path.hash();
            const entry = self.entry_map.getPtr(hash) orelse {
                // std.debug.print("entry not found: {s}\n", .{entry_path.slice()});
                return;
            };
            switch (entry.*) {
                .file => {
                    // std.debug.print("file: {s}\n", .{entry_path.slice()});
                },
                .dir => |*dir| {
                    // std.debug.print("{d} {d}\n", .{ dir.entries.count(), dir.entries.count() });
                    var iter = dir.entries.iterator();
                    const dir_len = dir.entries.count();
                    var child_count: usize = 0;
                    while (iter.next()) |iter_entry| {
                        // std.debug.print("{d} {d}\n", .{ child_count, dir_len });
                        // try writer.writeAll(indent);
                        try indent.write(writer);
                        const child = self.entry_map.get(iter_entry.key_ptr.*).?;

                        if (child_count < dir_len) {
                            try writer.writeAll("├ ");
                        } else {
                            try writer.writeAll("└ ");
                        }

                        switch (child) {
                            .file => {
                                var child_path = @constCast(&child.file.path);
                                var file = try self.getFile(child_path.slice());
                                const array = try file.readToArray(std.testing.allocator);
                                defer array.deinit();
                                try writer.print("{s} ({d} bytes, \"{s}...\")\n", .{ child_path.fileNameSlice(), array.items.len, array.items[0..@min(array.items.len, 10)] });
                            },
                            .dir => {
                                var child_path = @constCast(&child.dir.path);
                                try writer.print("{s}/\n", .{child_path.fileNameSlice()});
                                if (child_count < dir_len) {
                                    try indent.indent(.vertical);
                                } else {
                                    try indent.indent(.blank);
                                }
                                try self.dumpInner(writer, child_path, indent);
                                indent.dedent();
                            },
                        }
                        child_count += 1;
                    }
                },
            }
        }
        pub fn dump(self: *Self, writer: std.io.AnyWriter) !void {
            if (comptime virtual) {
                var indent = Indent{};
                var root_path = FsPath.init();

                return self.dumpInner(writer, &root_path, &indent);
            } else {
                var iter = self.entry_map.iterator();
                while (iter.next()) |entry| {
                    try writer.print("{d} {any}\n", .{ entry.key_ptr.*, entry.value_ptr });
                }
            }
        }
        pub fn getFile(self: *Self, path: []const u8) !*FsFile {
            var file_path = try FsPath.fromSlice(path);
            if (comptime virtual) {
                const entry = try self.getEntry(&file_path);
                switch (entry.*) {
                    .file => return &entry.file,
                    .dir => return error.NotAFile,
                }
            } else {
                const entry = try self.entry_map.getOrPut(self.allocator, file_path.hash());
                if (entry.found_existing) {
                    std.debug.print("file already exists: {s}\n", .{file_path.slice()});
                    return &entry.value_ptr.file;
                }

                _ = self.cwd.statFile(file_path.slice()) catch return error.FileNotFound;
                entry.value_ptr.* = .{ .file = FsFile{ .path = try file_path.clone(), .handle = .{ .closed = {} }, .fs = self } };
                return &entry.value_ptr.file;
            }
        }
        fn deleteVirtualFile(self: *Self, path: *FsPath) !void {
            if (!comptime virtual) @compileError("Virtual fs only");
            const hash = path.hash();
            const file_entry = try self.getEntry(path);
            if (std.meta.activeTag(file_entry.*) != .file) return error.NotAFile;

            if (path.dir()) |dir_path_ptr| {
                const dir_path = @constCast(&dir_path_ptr);
                const dir_entry = try self.getEntry(dir_path);
                file_entry.deinit();
                _ = dir_entry.dir.entries.remove(hash);
            }
            _ = self.entry_map.remove(hash);
        }
        fn deleteVirtualDir(self: *Self, path: *FsPath) !void {
            if (!comptime virtual) @compileError("Virtual fs only");
            const entry = try self.getEntry(path);
            switch (entry.*) {
                .file => return error.NotADir,
                .dir => {
                    entry.deinit();
                    // remove self from parent
                    if (path.dir()) |parent_path_ptr| {
                        const parent_path = @constCast(&parent_path_ptr);
                        const parent_entry: *Entry = self.entry_map.getPtr(parent_path.hash()) orelse @panic("parent dir not found");
                        _ = parent_entry.dir.entries.remove(path.hash());
                    }
                },
            }
        }
        fn deleteSystemDir(self: *Self, path: *FsPath) !void {
            if (comptime virtual) @compileError("System fs only");
            try self.cwd.deleteTree(path.slice());
        }
        fn deleteSystemFile(self: *Self, path: *FsPath) !void {
            if (comptime virtual) @compileError("System fs only");
            try self.cwd.deleteFile(path.slice());
        }

        pub fn deleteFile(self: *Self, path: []const u8) !void {
            var file_path = try FsPath.fromSlice(path);
            if (comptime virtual) {
                return try self.deleteVirtualFile(&file_path);
            } else {
                return try self.deleteSystemFile(&file_path);
            }
        }
        pub fn deleteDir(self: *Self, path: []const u8) !void {
            var dir_path = try FsPath.fromSlice(path);
            if (comptime virtual) {
                return try self.deleteVirtualDir(&dir_path);
            } else {
                return try self.deleteSystemDir(&dir_path);
            }
        }

        fn serializeInner(self: *Self, writer: std.io.AnyWriter, hash: u64) !void {
            const entry = self.entry_map.getPtr(hash) orelse {
                std.debug.panic("entry not found: {d}, {d}", .{ hash, self.entry_map.count() });
            };
            try writer.writeAll("{");
            switch (entry.*) {
                .file => {
                    try writer.print("\"path\": \"{s}\", \"kind\": \"file\", \"hash\": {d}", .{ entry.file.path.slice(), hash });
                },
                .dir => {
                    try writer.print("\"path\": \"{s}\", \"kind\": \"dir\", \"hash\": {d}, \"children\": [", .{ entry.dir.path.slice(), hash });
                    var iter = entry.dir.entries.iterator();
                    var first = true;
                    while (iter.next()) |iter_entry| {
                        _ = iter_entry; // autofix
                        if (!first) {
                            try writer.writeAll(",");
                        }
                        first = false;
                        // std.debug.print("{d},", .{iter_entry.key_ptr.*});
                        // try self.serializeInner(writer, iter_entry.key_ptr.*);
                    }
                    try writer.writeAll("]");
                },
            }

            try writer.writeAll("}");
        }
        pub fn serialize(self: *Self, writer: std.io.AnyWriter) !void {
            // var path = FsPath.init();
            var iter = self.entry_map.iterator();
            try writer.writeAll("{\n");
            var first = true;

            while (iter.next()) |kv| {
                const entry = kv.value_ptr;
                if (!first) {
                    try writer.writeAll(",\n");
                }
                first = false;

                switch (entry.*) {
                    .file => {
                        const path_slice = entry.file.path.slice();
                        const hash = entry.file.path.hash();
                        _ = hash; // autofix
                        try writer.print("  \"{s}\": {{\"path\": \"{s}\", \"kind\": \"file\"}}", .{ path_slice, path_slice });
                    },
                    .dir => {
                        const path_slice = entry.dir.path.slice();
                        const hash = entry.dir.path.hash();
                        _ = hash; // autofix
                        try writer.print("  \"{s}\": {{\"path\": \"{s}\", \"kind\": \"dir\", \"children\": [", .{ path_slice, path_slice });
                        var children_iter = entry.dir.entries.keyIterator();
                        var first_child = true;
                        while (children_iter.next()) |child_hash| {
                            var child = self.entry_map.get(child_hash.*).?;
                            const child_path = switch (child) {
                                .file => |*file| file.path.slice(),
                                .dir => |*dir| dir.path.slice(),
                            };
                            if (!first_child) {
                                try writer.writeAll(",");
                            }
                            first_child = false;
                            try writer.print("\"{s}\"", .{child_path});
                        }
                        try writer.writeAll("]}");
                    },
                }
            }

            try writer.writeAll("\n}");
            // try self.serializeInner(writer, path.hash());
        }
    };
}
pub const FsPath = struct {
    path: [MAX_PATH_BYTES]u8 = undefined,
    len: usize = 0,
    pub inline fn fromSlice(sl: []const u8) !FsPath {
        var path = FsPath.init();
        var it = std.mem.splitScalar(u8, sl, PATH_SEPARATOR);
        while (it.next()) |segment| {
            try path.append(segment);
        }
        return path;
    }
    pub inline fn init() FsPath {
        var path = FsPath{ .len = 2 };
        std.mem.copyForwards(u8, path.path[0..], "./");
        return path;
    }
    pub inline fn append(self: *FsPath, segment: []const u8) !void {
        if (segment.len == 0 or std.mem.eql(u8, segment, ".")) return;
        if (self.len > 0 and self.path[self.len - 1] != PATH_SEPARATOR) {
            self.path[self.len] = PATH_SEPARATOR;
            self.len += 1;
        }
        std.mem.copyForwards(u8, self.path[self.len..], segment);
        self.len += segment.len;
    }
    pub fn slice(self: *FsPath) []const u8 {
        return self.path[0..self.len];
    }
    pub fn clone(self: *FsPath) !FsPath {
        var new_path = FsPath.init();
        std.mem.copyForwards(u8, new_path.path[0..], self.path[0..self.len]);
        new_path.len = self.len;
        return new_path;
    }
    pub fn dir(self: *FsPath) ?FsPath {
        const sl = self.slice();
        var new_path = FsPath.init();
        if (std.mem.eql(u8, sl, "./")) return null;
        var index = std.mem.lastIndexOfScalar(u8, sl, PATH_SEPARATOR) orelse return null;
        if (index < 2) index += 1;
        std.mem.copyForwards(u8, new_path.path[0..], sl[0..index]);
        new_path.len = index;
        return new_path;
    }
    pub fn fileNameSlice(self: *FsPath) []const u8 {
        return std.fs.path.basename(self.slice());
    }
    pub fn iter(self: *FsPath) std.mem.SplitIterator(u8, .scalar) {
        return std.mem.splitScalar(u8, self.slice(), PATH_SEPARATOR);
    }
    pub fn hash(self: *FsPath) u64 {
        return std.hash.Wyhash.hash(0, self.slice());
    }
};

const expectEqualStrings = std.testing.expectEqualStrings;
const expectEqual = std.testing.expectEqual;
test "FsPath" {
    var path = try FsPath.fromSlice("./a/b/c");
    try expectEqualStrings("./a/b/c", path.slice());
    var dir = path.dir();
    try expectEqualStrings("./a/b", dir.?.slice());
    const file_name = path.fileNameSlice();
    try expectEqualStrings("c", file_name);
}
test "Fs" {
    // const FsSystem = Fs(false);
    // _ = FsSystem; // autofix

    const FsVirtual = Fs(true);
    var fs_virtual = try FsVirtual.init(std.testing.allocator, "./.tmp-test");
    defer fs_virtual.deinit();
    // const file_path = "./a/b/c/test.txt";
    // const dir = try fs_virtual.makeDir("./a/b/c");
    const file = try fs_virtual.makeFile("./test.txt");
    _ = file; // autofix
    // _ = file; // autofix
    // const file2 = try fs_virtual.makeFile("./a/b/c/test2.txt");
    // _ = file2; // autofix
    // const file_ptr = try fs_virtual.getFile("./a/b/c/test.txt");
    // const writer = try file_ptr.getWriter();
    // try writer.writeAll("test");
    // try fs_virtual.deleteDir("./a/b");
    try fs_virtual.dump(std.io.getStdErr().writer().any());

    try fs_virtual.serialize(std.io.getStdErr().writer().any());

    // _ = dir; // autofix
    // _ = file_path; // autofix
    // try expectEqual(fs_virtual.fileExists(file_path), false);
    // const file_index = try fs_virtual.makeFile(file_path);
    // _ = file_index; // autofix
    // try expectEqual(fs_virtual.fileExists(file_path), true);
    // try expectEqual(file_index, fs_virtual.getFile(file_path));

    // var fs_system = try FsSystem.init(std.testing.allocator, "./.tmp-test");
    // defer fs_system.deinit();
    // _ = try fs_system.makeDir("./a/b/c");
    // try expectEqual(fs_system.fileExists("/a/b/c/test.txt"), false);
}
