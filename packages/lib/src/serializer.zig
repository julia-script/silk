const std = @import("std");
const PackedLists = @import("PackedLists.zig").new;
const InternedSlice = @import("InternedStrings.zig").InternedSlice;
pub const WriteJsonOptions = struct {
    lists: ?*PackedLists(u32, 0) = null,
};
pub fn writeJSON(T: type, writer: std.io.AnyWriter, data: T, options: anytype) !void {
    const type_info = @typeInfo(T);

    switch (type_info) {
        .@"struct" => |str| {
            if (T == InternedSlice) {
                try writer.print("\"{s}\"", .{options.interned.getSlice(data)});

                return;
            }
            try writer.writeAll("{");
            inline for (str.fields, 0..) |field, i| {
                if (i != 0) try writer.writeAll(", ");
                try writer.print("\"{s}\": ", .{field.name});
                if (comptime std.mem.endsWith(u8, field.name, "list")) {
                    if (@hasField(@TypeOf(options), "lists")) {
                        const index: usize = @field(data, field.name);
                        var iter = options.lists.iterList(index);
                        try writer.writeAll("[");
                        var j: usize = 0;
                        while (iter.next()) |child| {
                            if (j != 0) try writer.writeAll(", ");
                            try writer.print("{d}", .{child});
                            j += 1;
                        }
                        try writer.writeAll("]");
                    }
                } else {
                    try writeJSON(field.type, writer, @field(data, field.name), options);
                }
            }
            try writer.writeAll("}");
        },
        .int, .float => {
            try writer.print("{d}", .{data});
        },
        .optional => |opt| {
            if (data) |value| {
                try writeJSON(opt.child, writer, value, options);
            } else {
                try writer.writeAll("null");
            }
        },
        .bool => {
            try writer.print("{any}", .{data});
        },
        .@"enum" => |e| {
            if (std.enums.tagName(T, data)) |tag| {
                try writer.print("\"{s}\"", .{tag});
            } else {
                try writer.print("{d}", .{@intFromEnum(data) - e.fields.len});
            }
        },
        .pointer => |slice| {
            switch (slice.size) {
                .Slice => {
                    try writer.writeAll("[");

                    for (data, 0..) |item, i| {
                        if (i != 0) try writer.writeAll(", ");
                        try writeJSON(slice.child, writer, item, options);
                    }
                    try writer.writeAll("]");
                },
                else => {
                    // try writer.writeAll();
                    try writer.print("{any}", .{slice.size});
                },
            }
        },
        .@"union" => {
            try writer.writeAll("{");
            inline for (std.meta.fields(T)) |field| {
                if (std.mem.eql(u8, field.name, @tagName(data))) {
                    try writer.print("\"{s}\": ", .{field.name});
                    try writeJSON(field.type, writer, @field(data, field.name), options);
                }
            }
            try writer.writeAll("}");
        },
        else => {
            try writer.writeAll(@tagName(std.meta.activeTag(type_info)));
        },
    }
}
pub fn writeTsType(T: type, type_name: []const u8, writer: std.io.AnyWriter) !void {
    try writer.print("export type {s} = ", .{type_name});
    try writeTsTypeInner(T, writer, 0);
    try writer.writeAll(";\n\n");
    var buf: [1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(buf[0..]);
    var hashmap = std.StringHashMap(void).init(fba.allocator());
    try writeEnumsRecursive(T, &hashmap, writer);
}
pub fn writeTypeName(T: type, writer: std.io.AnyWriter) !void {
    const type_name = @typeName(T);
    for (type_name) |c| {
        if (c != '.') {
            try writer.writeByte(c);
        }
    }
}
pub fn writeEnumTsType(T: type, writer: std.io.AnyWriter) !void {
    try writer.writeAll("export type ");
    try writeTypeName(T, writer);
    try writer.writeAll(" = ");
    const info = @typeInfo(T);

    switch (info) {
        .@"enum" => |e| {
            inline for (e.fields, 0..) |field, i| {
                if (i != 0) try writer.writeAll(" | ");
                try writer.print("\"{s}\"", .{field.name});
            }
            if (!e.is_exhaustive) {
                try writer.writeAll(" | number");
            }
        },
        else => {
            @compileError("Unsupported type: " ++ @typeName(T));
        },
    }
}
pub inline fn writeEnumsRecursive(
    T: type,
    hashmap: *std.StringHashMap(void),
    writer: std.io.AnyWriter,
) !void {
    switch (@typeInfo(T)) {
        .@"enum" => |e| {
            _ = e; // autofix
            // types[len.*] = T;
            // len.* = len.* + 1;
            const name = @typeName(T);
            if (!hashmap.contains(name)) {
                hashmap.put(name, {}) catch unreachable;

                try writeEnumTsType(T, writer);
                try writer.writeAll(";\n");
                // try hashmap.put(name, name[0..]);
            }
        },
        .@"struct", .@"union" => {
            inline for (std.meta.fields(T)) |field| {
                try writeEnumsRecursive(field.type, hashmap, writer);
            }
        },
        else => {},
    }
}
pub fn writeTsTypeInner(T: type, writer: std.io.AnyWriter, indent: usize) !void {
    const type_info = @typeInfo(T);
    const indent_chars = "  ";

    switch (type_info) {
        .@"struct" => |str| {
            if (T == InternedSlice) {
                try writer.writeAll("string");
                return;
            }
            try writer.writeAll("{\n");

            inline for (str.fields, 0..) |field, i| {
                if (i != 0) try writer.writeAll(",\n");
                try writer.writeBytesNTimes(indent_chars, indent + 1);
                try writer.print("\"{s}\": ", .{field.name});
                if (comptime std.mem.endsWith(u8, field.name, "list")) {
                    try writer.writeAll("Array<");
                    try writeTsTypeInner(field.type, writer, indent + 2);
                    try writer.writeAll(">");
                } else {
                    try writeTsTypeInner(field.type, writer, indent + 2);
                }
            }
            try writer.writeAll("\n");
            try writer.writeBytesNTimes(indent_chars, indent);
            try writer.writeAll("}");
            // },
            // .int => {
            //     try writer.writeAll("number");
            // },
            // .@"union" => {
            //     inline for (std.meta.fields(T)) |field| {
            //         _ = field; // autofix
            //         // if (std.mem.eql(u8, field.name, @tagName(data))) {
            //         //     try writer.print("\"{s}\": ", .{field.name});
            //         //     try writeJSON(field.type, writer, @field(data, field.name), lists);
            //         // }
            //     }
        },
        .pointer => |slice| {
            switch (slice.size) {
                .Slice => {
                    try writer.writeAll("Array<");
                    try writeTsTypeInner(slice.child, writer, indent + 2);
                    try writer.writeAll(">");
                },
                else => {
                    try writer.writeAll("unknown");
                },
            }
        },
        .int => {
            try writer.writeAll("number");
        },
        .bool => {
            try writer.writeAll("boolean");
        },
        .float => {
            try writer.writeAll("number");
        },
        .optional => |opt| {
            try writer.writeAll("null | ");
            try writeTsTypeInner(opt.child, writer, indent + 2);
        },
        .@"enum" => |e| {
            _ = e; // autofix
            try writeTypeName(T, writer);
        },
        .@"union" => {
            inline for (std.meta.fields(T), 0..) |field, i| {
                if (i != 0) try writer.writeAll(" | ");
                try writer.writeAll("{\n");
                try writer.writeBytesNTimes(indent_chars, indent + 1);
                try writer.print("\"{s}\": ", .{field.name});
                try writeTsTypeInner(field.type, writer, indent + 1);
                try writer.writeBytesNTimes(indent_chars, indent + 1);
                try writer.writeAll("\n");
                try writer.writeBytesNTimes(indent_chars, indent);
                try writer.writeAll("}");
            }
        },
        else => {
            try writer.writeAll(@tagName(std.meta.activeTag(type_info)));
        },
    }
}
const expectEqualStrings = std.testing.expectEqualStrings;
test "serializer" {
    const TestEnum = enum {
        A,
        B,
        C,
    };

    const TestEnum2 = enum(u8) {
        A,
        B,
        C,
        _,
    };
    const TestStruct = struct {
        a: u32,
        c: TestEnum,
        b: []u32,
        d: TestEnum2,
        e: TestEnum,
    };

    var buf = std.BoundedArray(u8, 1024){};
    const writer = buf.writer().any();
    try writeEnumTsType(TestEnum, writer);
    try expectEqualStrings("export type TestEnum = \"A\" | \"B\" | \"C\"", buf.slice());
    buf.clear();
    try writeEnumTsType(TestEnum2, writer);
    try expectEqualStrings("export type TestEnum2 = \"A\" | \"B\" | \"C\" | number", buf.slice());

    buf.clear();
    try writeTsType(TestStruct, "TestStruct", writer);
    try expectEqualStrings(
        \\export type TestStruct = {
        \\  "a": number,
        \\  "c": TestEnum,
        \\  "b": Array<number>,
        \\  "d": TestEnum2,
        \\  "e": TestEnum
        \\};
        \\
        \\export type TestEnum = "A" | "B" | "C";
        \\export type TestEnum2 = "A" | "B" | "C" | number;
        \\
    , buf.slice());
    // try expectEqualStrings("export type TestStruct = { a: number; b: Array<number>; c: TestEnum }", buf.slice());

}
