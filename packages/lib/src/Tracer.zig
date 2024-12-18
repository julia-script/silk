const std = @import("std");
const options = @import("options");
const fmt = @import("format_utils.zig");

const Event = struct {
    // name: []const u8,
    // args: []const u8,
};

// writer: std.io.AnyWriter,
file: ?std.fs.File,
// writer: std.io.AnyWriter,
count: u32 = 0,
indent: u64 = 0,
name: std.BoundedArray(u8, 1024),
// root_id: u64 = 0,

const Self = @This();
var ID: u64 = 0;

pub fn init(dir: []const u8, name: []const u8, unique_name: bool) !Self {
    const name_ = try std.BoundedArray(u8, 1024).fromSlice(name);
    if (comptime !options.enable_tracer) {
        return Self{
            .file = null,
            .name = name_,
        };
    }

    var buf: [std.fs.max_path_bytes]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(buf[0..]);
    const path = try std.fs.path.join(fba.allocator(), &.{ dir, name });

    try std.fs.cwd().makePath(path);

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

    return Self{
        .file = try std.fs.cwd().createFile(file_name, .{ .truncate = true }),
        .name = name_,
    };
}

const file_end = "\n]\n";
const Phase = enum {
    begin,
    end,
    async_begin,
    async_instant,
    async_end,
    complete_event,
    instant,
    flow_start,
    flow_step,
    flow_end,
    pub fn format(self: Phase, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .begin => try writer.writeAll("B"),
            .end => try writer.writeAll("E"),
            .async_begin => try writer.writeAll("b"),
            .async_end => try writer.writeAll("e"),
            .async_instant => try writer.writeAll("n"),
            .complete_event => try writer.writeAll("X"),
            .instant => try writer.writeAll("i"),
            .flow_start => try writer.writeAll("s"),
            .flow_step => try writer.writeAll("t"),
            .flow_end => try writer.writeAll("f"),
        }
    }
};
pub fn deinit(self: *Self) void {
    if (self.file) |file| {
        file.close();
    }
}
pub fn beginEvent(self: *Self, comptime name: []const u8, args: anytype) u64 {
    const id = ID;
    ID += 1;
    self.writeEvent(.begin, null, comptime name, .{}, args, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
    return id;
}
pub fn endEvent(self: *Self, id: u64, comptime name: []const u8, args: anytype) void {
    self.writeEvent(.end, null, comptime name, .{}, args, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
}

pub fn logEvent(self: *Self, comptime name: []const u8, args: anytype) void {
    const id = ID;
    ID += 1;
    self.writeEvent(.instant, null, name, .{}, args, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
}
pub fn begin(self: *Self, comptime fmt_: []const u8, fmt_args: anytype, args: anytype) u64 {
    const id = ID;
    ID += 1;
    self.writeEvent(.begin, null, fmt_, fmt_args, args, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
    return id;
}
pub fn beginAsync(self: *Self, comptime fmt_: []const u8, fmt_args: anytype, args: anytype) u64 {
    const id = ID;
    ID += 1;
    self.writeEvent(.async_begin, null, fmt_, fmt_args, args, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
    return id;
}
pub fn end(self: *Self, id: u64) void {
    self.writeEvent(.end, null, "", .{}, .{}, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
}
pub fn endAsync(self: *Self, id: u64) void {
    self.writeEvent(.async_end, null, "", .{}, .{}, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
}
pub fn asyncInstant(self: *Self, id: u64, comptime fmt_: []const u8, fmt_args: anytype, args: anytype) void {
    self.writeEvent(.async_instant, null, fmt_, fmt_args, args, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
}
pub fn beginFlow(self: *Self, comptime fmt_: []const u8, fmt_args: anytype, args: anytype) u64 {
    const id = ID;
    ID += 1;
    self.writeEvent(.flow_start, null, fmt_, fmt_args, args, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
    return id;
}
pub fn endFlow(self: *Self, id: u64) void {
    self.writeEvent(.flow_end, null, "", .{}, .{}, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
}
pub fn stepFlow(self: *Self, id: u64, comptime fmt_: []const u8, fmt_args: anytype, args: anytype) void {
    self.writeEvent(.flow_step, null, fmt_, fmt_args, args, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
}
pub fn printEvent(self: *Self, comptime fmt_: []const u8, fmt_args: anytype, args: anytype) void {
    const id = ID;
    ID += 1;
    self.writeEvent(.instant, null, fmt_, fmt_args, args, id) catch |err| {
        std.debug.panic("Error writing event: {s}\n{}", .{ @errorName(err), err });
    };
}
const TraceBlock = struct {
    event_id: u64,
    tracer: *Self,
    pub inline fn call(self: @This(), b: anytype) void {
        std.debug.print("trace block end {d} {any}\n", .{ self.event_id, b });

        self.tracer.end(self.event_id);
    }
};
// pub inline fn traceBlock(self: *Self, comptime fmt_: []const u8, fmt_args: anytype, args: anytype) *TraceBlock {
//     var ty = (TraceBlock{
//         .event_id = self.begin(fmt_, fmt_args, args),
//         .tracer = self,
//     });

//     std.debug.print("trace block begin {d}\n", .{ty.event_id});
//     return &ty;
// }

pub fn panic(self: *Self, comptime msg: []const u8, args: anytype) noreturn {
    const id = ID;
    ID += 1;
    self.writeEvent(.instant, "panic", msg, .{}, args, id) catch |err| {
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

            if (ptr.size == .Many and ptr.sentinel == null) {
                try writer.print("\"{any}\"", .{args});
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
    // name: []const u8,
    comptime fmt_: []const u8,
    fmt_args: anytype,
    args: anytype,
    id: u64,
) !void {
    // std.debug.print
    if (comptime options.print_trace) {
        // if (phase == .end and id != null) {
        //     self.indent = id.?;
        // }
        const color = fmt.pickColor(id);
        const stderr = std.io.getStdErr().writer().any();

        try color.print(stderr, "[{s}] ", .{self.name.slice()}, .{});
        // if (self.root_id == id) {
        //     try color.print(stderr, "root", .{}, .{});
        // }
        // try stderr.writeBytesNTimes("    ", indent);
        try color.print(stderr, "{?d}: ", .{id}, .{});
        try color.print(stderr, fmt_, fmt_args, .{});
        try color.print(stderr, "\n", .{}, .{});

        // if (phase == .begin and id != null) {
        //     // self.indent += 1;
        // }
    }
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

    try writer.print("  {{ \"pid\": \"{d}\", \"tid\": \"{d}\", \"ph\": \"{}\", \"ts\": {d}, \"args\": ", .{
        0,
        0,
        phase,
        ts,
    });

    const T = @TypeOf(args);
    try writeArgs(T, writer, args);
    if (phase != .end) {
        try writer.print(", \"name\": \"", .{});
        try writer.print(fmt_, fmt_args);
        try writer.print("\"", .{});
    }
    try writer.print(", \"id\": {d}", .{id});
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
