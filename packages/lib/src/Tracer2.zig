const std = @import("std");
const Src = std.builtin.SourceLocation;
const IS_ENABLED = @import("options").enable_tracer;

allocator: std.mem.Allocator,
namespace: []const u8,
scratch: std.ArrayList(u8),
id: u64 = 0,
indent: u64 = 0,
count: u64 = 0,
file: ?std.fs.File = null,

const Self = @This();
pub fn init(
    allocator: std.mem.Allocator,
    comptime namespace: anytype,
    options: TraceOptions,
) !Self {
    if (comptime !IS_ENABLED) {
        return Self{
            .allocator = undefined,
            .namespace = undefined,
            .scratch = undefined,
        };
    }
    const dir_path = try std.fs.path.join(allocator, &.{ options.dir, @tagName(namespace) });
    defer allocator.free(dir_path);
    try std.fs.cwd().makePath(dir_path);
    var dir = try std.fs.cwd().openDir(dir_path, .{});
    defer dir.close();

    const file = try dir.createFile("trace.json", .{
        .truncate = true,
    });

    return Self{
        .file = file,

        .allocator = allocator,
        .namespace = @tagName(namespace),
        .scratch = std.ArrayList(u8).init(allocator),
    };
}

pub fn deinit(self: *Self) void {
    if (self.file) |file| {
        file.close();
    }
    self.scratch.deinit();
}

pub const TraceOptions = struct {
    dir: []const u8 = "./.tmp/traces",
    enabled: bool = true,
    log: bool = true,
};

fn getId(self: *Self) u64 {
    const id = self.id;
    self.id += 1;
    return id;
}
const Phase = enum {
    begin,
    end,
    instant,
    async_begin,
    async_end,
    complete_event,
    pub fn format(self: Phase) []const u8 {
        switch (self) {
            .begin => return "B",
            .end => return "E",
            .async_begin => return "b",
            .async_end => return "e",
            .complete_event => return "X",
            .instant => return "i",
        }
    }
};
pub const EndTrace = struct {
    id: u64,
    indent: u64 = 0,
    tracer: *Self,
    phase: Phase = .end,

    pub inline fn end(
        self: *const EndTrace,
        args: anytype,
    ) void {
        if (comptime !IS_ENABLED) {
            return;
        }
        self.endLater(self.tracer, args);
    }
    pub inline fn endLater(self: *const EndTrace, tracer: *Self, args: anytype) void {
        if (comptime !IS_ENABLED) {
            return;
        }
        tracer.writeEntry(.{
            .id = self.id,
            .ph = self.phase.format(),
            .tid = 0,
            .pid = 0,
            .ts = std.time.microTimestamp(),
            .args = args,
        }) catch unreachable;
    }
};
pub inline fn begin(
    self: *Self,
    src: Src,
    format: anytype,
    args: anytype,
) EndTrace {
    if (comptime !IS_ENABLED) {
        return EndTrace{
            .id = 0,
            .indent = 0,
            .tracer = undefined,
            .phase = undefined,
        };
    }
    const id = getId(self);
    self.writeTrace(
        id,
        .begin,
        src,
        format,
        args,
    ) catch unreachable;
    return EndTrace{ .id = id, .indent = self.indent, .tracer = &self.* };
}
pub fn trace(
    self: *Self,
    src: Src,
    format: anytype,
    args: anytype,
) void {
    if (comptime !IS_ENABLED) {
        return;
    }
    const id = getId(self);
    self.writeTrace(
        id,
        .instant,
        src,
        format,
        args,
    ) catch unreachable;
}
pub fn beginAsync(
    self: *Self,
    src: Src,
    format: anytype,
    args: anytype,
) EndTrace {
    if (comptime !IS_ENABLED) {
        return EndTrace{
            .id = 0,
            .indent = 0,
            .tracer = undefined,
            .phase = undefined,
        };
    }
    const id = getId(self);
    self.writeTrace(
        id,
        .async_begin,
        src,
        format,
        args,
    ) catch unreachable;
    return EndTrace{
        .id = id,
        .indent = self.indent,
        .tracer = self,
        .phase = .async_end,
    };
}
const file_end = "\n]\n";
inline fn formatSrc(src: Src) ![]const u8 {
    var buf = try std.BoundedArray(u8, std.fs.max_path_bytes).init(0);
    const writer = buf.writer();
    _ = writer; // autofix

    try buf.writer().print("{s}:{d}:{d}", .{ src.file, src.line, src.column });
    // try buf.writer().print("{s}:{d}:{d}", .{ file_path, src.line, src.column });
    return buf.slice();
}

pub fn writeTrace(
    self: *Self,
    id: u64,
    phase: Phase,
    src: Src,
    format: anytype,
    args: anytype,
) !void {
    if (comptime !IS_ENABLED) {
        return;
    }
    var event_name = try std.BoundedArray(u8, 1024).init(0);
    try event_name.writer().print(format[format.len - 2], format[format.len - 1]);

    try self.writeEntry(.{
        .id = id,
        .ph = phase.format(),
        .name = event_name.slice(),
        .cat = if (format.len > 2) @as([]const u8, format[0][0..]) else undefined,
        .tid = 0,
        .pid = 0,
        .ts = std.time.microTimestamp(),
        .args = .{
            .src = .{
                .fn_name = src.fn_name,
                .file = try formatSrc(src),
                .module_name = src.module,
            },
            .payload = args,
        },
    });
}
pub fn writeEntry(self: *Self, body: anytype) !void {
    if (comptime !IS_ENABLED) {
        return;
    }
    const file = self.file orelse return;
    self.scratch.clearRetainingCapacity();

    const writer = self.scratch.writer().any();
    try formatArgs(writer, body);
    // try writer.writeAll("\n");
    if (self.count == 0) {
        try file.writeAll("[\n");
    } else {
        const file_end_len: i64 = @intCast(file_end.len);
        try file.seekBy(-file_end_len);
        try file.writeAll(",\n");
    }
    try file.writeAll(self.scratch.items);
    try file.writeAll(file_end);
    self.count += 1;
}

pub fn formatArgs(writer: std.io.AnyWriter, args: anytype) !void {
    const T = @TypeOf(args);

    switch (@typeInfo(T)) {
        .@"struct" => |struct_| {
            // std.debug.print("args: {s}\n", .{@typeName(T)});
            if (comptime std.mem.startsWith(u8, @typeName(T), "array_hash_map.ArrayHashMap")) {
                try writer.writeAll("\"unsupported\"");
                return;
                // // return;
                // // std.debug.print("args: {s}\n", .{@typeName(T)});

                // var iter = args.iterator();
                // var first = true;
                // try writer.print("{{", .{});
                // while (iter.next()) |entry| {
                //     if (!first) {
                //         try writer.writeAll(", ");
                //     }
                //     first = false;
                //     std.debug.print("entry.key_ptr: {s}\n", .{entry.key_ptr.ptr[0..entry.key_ptr.len]});
                //     try writer.print("\"{s}\": ", .{entry.key_ptr.ptr[0..entry.key_ptr.len]});
                //     try formatArgs(writer, entry.value_ptr);
                // }
                // try writer.print("}}", .{});
                // return;
            }
            var first = true;
            try writer.print("{{", .{});
            inline for (struct_.fields) |field| {
                if (std.meta.activeTag(@typeInfo(field.type)) != .undefined) {
                    if (!first) {
                        try writer.print(", ", .{});
                    }
                    first = false;
                    try writer.print("\"{s}\": ", .{field.name});
                    try formatArgs(writer, @field(args, field.name));
                }
            }
            try writer.print("}}", .{});
        },
        .@"union" => |uni| {
            const tag_name = switch (std.meta.activeTag(args)) {
                inline else => |tag| @tagName(tag),
            };
            _ = uni; // autofix
            try writer.print("{{", .{});
            try writer.print("\"{s}\": ", .{tag_name});
            switch (args) {
                inline else => |value| {
                    try formatArgs(writer, value);
                },
            }
            try writer.print("}}", .{});
        },
        .pointer => |ptr| {
            if (ptr.size == .Slice) {
                if (ptr.child == u8) {
                    // try writer.print("\"{s}\"", .{args});
                    try std.json.encodeJsonString(args, .{}, writer);
                    return;
                }
                try writer.writeAll("[");
                for (args, 0..) |item, i| {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }
                    try formatArgs(writer, item);
                }
                try writer.writeAll("]");
                return;
            }

            if (ptr.size == .Many and ptr.sentinel == null) {
                try writer.print("\"{any}\"", .{args});
                return;
            }

            try formatArgs(writer, args.*);
        },
        .array => |arr| {
            if (arr.child == u8) {
                try std.json.encodeJsonString(&args, .{}, writer);
                return;
            }
            try writer.writeAll("[");
            for (args, 0..) |item, i| {
                if (i > 0) {
                    try writer.writeAll(", ");
                }
                try formatArgs(writer, item);
            }
            try writer.writeAll("]");
        },
        .optional => |opt| {
            _ = opt; // autofix
            if (args) |value| {
                try formatArgs(writer, value);
            } else {
                try writer.writeAll("null");
            }
        },
        .void => {
            try writer.writeAll("\"void\"");
        },
        .int, .float, .comptime_int, .comptime_float => {
            try writer.print("{d}", .{args});
        },
        .undefined => {
            try writer.writeAll("\"undefined\"");
        },
        .null => {
            try writer.writeAll("null");
        },
        else => {
            try writer.print("\"{any}\"", .{args});
        },
        // .@"enum" => {
        //     try writer.print("\"{}\"", .{args});
        // },
    }
}
