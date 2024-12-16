const std = @import("std");
const options = @import("options");

const Event = struct {
    // name: []const u8,
    // args: []const u8,
};

// writer: std.io.AnyWriter,
file: ?std.fs.File,
// writer: std.io.AnyWriter,
count: u32 = 0,
const Self = @This();
var ID: u64 = 0;

pub fn init(dir: []const u8, name: []const u8, unique_name: bool) !Self {
    if (comptime !options.enable_tracer) {
        return Self{
            .file = null,
        };
    }

    var buf: [std.fs.max_path_bytes]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(buf[0..]);
    const path = try std.fs.path.join(fba.allocator(), &.{ dir, name });

    try std.fs.cwd().makePath(path);

    std.debug.print("trace_dir: {s}\n", .{path});
    // fba.reset();

    var file_name_buf: [std.fs.max_path_bytes]u8 = undefined;

    const file_name = if (!unique_name) try std.fmt.bufPrint(
        file_name_buf[0..],
        "{s}/trace-{d}.json",
        .{ path, std.time.microTimestamp() },
    ) else try std.fmt.bufPrint(
        file_name_buf[0..],
        "{s}/trace.json",
        .{path},
    );

    std.debug.print("file_name: {s}\n", .{file_name});

    return Self{
        .file = try std.fs.cwd().createFile(file_name, .{ .truncate = true }),
    };
}

const file_end = "\n]\n";
const Phase = enum {
    begin,
    end,
    complete_event,
    instant,
    pub fn format(self: Phase, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .begin => try writer.writeAll("B"),
            .end => try writer.writeAll("E"),
            .complete_event => try writer.writeAll("X"),
            .instant => try writer.writeAll("i"),
        }
    }
};
pub fn deinit(self: *Self) void {
    if (self.file) |file| {
        file.close();
    }
}
pub fn beginEvent(self: *Self, name: []const u8, args: anytype) u64 {
    const id = ID;
    ID += 1;
    self.writeEvent(.begin, null, name, args, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
    return id;
}
pub fn endEvent(self: *Self, id: u64, name: []const u8, args: anytype) void {
    self.writeEvent(.end, null, name, args, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
}

pub fn logEvent(self: *Self, name: []const u8, args: anytype) void {
    self.writeEvent(.instant, null, name, args, null) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
}
pub fn panic(self: *Self, msg: []const u8, args: anytype) noreturn {
    self.writeEvent(.instant, "panic", msg, args, null) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
    // std.debug.panic
    std.debug.panic("panic: {s}\n{any}\n", .{ msg, args });
    // std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
}
pub fn writeArgs(T: type, writer: std.io.AnyWriter, args: anytype) !void {
    // const fields = std.meta.fields(T);
    // try writer.print("{{", .{});
    // var first = true;
    // inline for (fields) |field| {
    //     if (!first) {
    //         try writer.print(", ", .{});
    //     }
    //     first = false;
    //     try writer.print("\"{s}\": ", .{field.name});

    //     switch (field.type) {
    //         usize, u8, i8, u16, i16, u32, i32, u64, i64, f32, f64, comptime_int, comptime_float => try writer.print("{d}", .{@field(args, field.name)}),
    //         []const u8, [:0]const u8, []u8 => try writer.print("\"{s}\"", .{@field(args, field.name)}),
    //         bool => try writer.print("{}", .{field.value}),

    //         else => {
    //             switch (@typeInfo(field.type)) {
    //                 // .Struct => try writer.print("{{{s}}}", .{@typeName(field.type)}),
    //                 .@"enum" => try writer.print("\"{}\"", .{field.value}),
    //                 else => @compileError("Unsupported type: " ++ @typeName(field.type)),
    //             }
    //         },
    //     }
    // }
    // try writer.print("}}", .{});
    switch (@typeInfo(T)) {
        .@"struct" => |struct_| {
            var first = true;
            try writer.print("{{", .{});
            inline for (struct_.fields) |field| {
                if (!first) {
                    try writer.print(", ", .{});
                }
                first = false;
                try writer.print("\"{s}\": ", .{field.name});
                try writeArgs(field.type, writer, @field(args, field.name));
            }
            try writer.print("}}", .{});
        },
        .@"union" => |uni| {
            const tag_name = switch (std.meta.activeTag(args)) {
                inline else => |tag| @tagName(tag),
            };
            _ = uni; // autofix
            switch (args) {
                inline else => |value| {
                    try writer.print("{{", .{});
                    try writer.print("\"{s}\": ", .{tag_name});
                    try writeArgs(@TypeOf(value), writer, value);
                    try writer.print("}}", .{});
                },
            }
        },
        .pointer => |ptr| {
            if (ptr.size == .Slice) {
                if (ptr.child == u8) {
                    try writer.print("\"{s}\"", .{args});
                    return;
                }
                try writer.writeAll("[");
                for (args, 0..) |item, i| {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }
                    try writeArgs(ptr.child, writer, item);
                }
                try writer.writeAll("]");
                return;
            }
            try writeArgs(ptr.child, writer, args.*);
        },
        .optional => |opt| {
            if (args) |value| {
                try writeArgs(opt.child, writer, value);
            } else {
                try writer.writeAll("null");
            }
        },
        .void => {
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

pub fn writeEvent(
    self: *Self,
    comptime phase: Phase,
    category: ?[]const u8,
    name: []const u8,
    args: anytype,
    id: ?u64,
) !void {
    if (comptime !options.enable_tracer) {
        return;
    }
    const file = self.file orelse return;
    const ts = std.time.microTimestamp();
    const writer = file.writer().any();
    if (self.count == 0) {
        try writer.print("[\n", .{});
    } else {
        const file_end_len: i64 = @intCast(file_end.len);
        try file.seekBy(-file_end_len);
        try writer.print(",\n", .{});
    }
    defer self.count += 1;

    try writer.print("  {{ \"pid\": \"{d}\", \"ph\": \"{}\", \"ts\": {d}, \"name\": \"{s}\", \"args\": ", .{
        0,
        phase,
        ts,
        name,
    });

    const T = @TypeOf(args);
    try writeArgs(T, writer, args);
    if (id) |id_| {
        try writer.print(", \"id\": {d}", .{id_});
    }
    if (category) |cat| {
        try writer.print(", \"cat\": \"{s}\"", .{cat});
    }
    try writer.print("}}", .{});

    try writer.print(file_end, .{});
}

test "Trace" {
    var self = try init("./.tmp/trace", "trace", true);
    const id = self.beginEvent("test", .{
        .a = 1,
    });
    self.endEvent(id, "test", .{});
}
