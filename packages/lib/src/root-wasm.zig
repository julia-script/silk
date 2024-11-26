const std = @import("std");
const serializer = @import("serializer.zig");
const host = @import("host.zig");
const Compilation = @import("Compilation.zig");
const Lexer = @import("Lexer.zig");
const AstGen = @import("AstGen.zig");
const Ast = @import("Ast.zig");
const ErrorManager = @import("ErrorManager.zig");
const HirBuilder = @import("HirBuilder.zig");
const MirBuilder = @import("MirBuilder.zig");
const Hir = @import("Hir.zig");
const Mir = @import("Mir.zig");

pub fn throw(message: []const u8) noreturn {
    host.throw(message.ptr, message.len);
}
pub fn log(message: []const u8) void {
    host.write(message.ptr, message.len);
}

fn panic(message: []const u8, _: ?*std.builtin.StackTrace, _: ?usize) noreturn {
    throw(message);
}

extern fn invokeWriter(writer_id: usize, byte: u8) void;

pub const OsWriter = struct {
    id: usize,
    pub fn init(id: usize) Self {
        return .{ .id = id };
    }

    fn appendWrite(self: *Self, m: []const u8) Error!usize {
        var count: usize = 0;
        for (m) |byte| {
            invokeWriter(self.id, byte);
            count += 1;
        }
        return count;
    }
    pub fn deinit(self: *Self) void {
        destroyWriter(self.id);
    }
    const Self = @This();
    const Error = error{};

    const Writer = std.io.Writer(*Self, Error, appendWrite);

    pub fn writer(self: *Self) Writer {
        return .{ .context = self };
    }
};

extern fn destroyWriter(id: usize) void;

fn lexSource(writer: std.io.AnyWriter, source: []const u8) !void {
    var lexer = Lexer.init(source);
    try writer.writeAll("[");
    var i: usize = 0;
    while (lexer.next()) |token| {
        if (i > 0) try writer.writeAll(",");
        try writer.print("[\"{s}\",{d},{d}]", .{ @tagName(token.tag), token.start, token.end });
        i += 1;
    }
    try writer.writeAll("]");
}
pub fn getSlice(pointer: [*]const u8) []const u8 {
    const len = std.mem.bytesAsSlice(u32, pointer[0..4]);
    const length = len[0];

    return pointer[4 .. length + 4];
}

// export fn lex(pointer: [*]const u8) void {

pub export fn alloc(len: usize) [*]u8 {
    const buf = host.allocator.alloc(u8, len) catch {
        @panic("failed to allocate memory");
    };
    return buf.ptr;
}

pub export fn wasmAllocZ(len: usize) [*:0]u8 {
    const buf = host.allocator.allocSentinel(u8, len, 0) catch {
        @panic("failed to allocate memory");
    };
    return buf.ptr;
}
pub export fn free(ptr: usize, length: usize) void {
    const slice = @as([*]u8, @ptrFromInt(ptr));
    host.allocator.free(slice[0..length]);
}
fn wasmCreate(T: type) *T {
    return host.allocator.create(T) catch {
        @panic("failed to create object");
    };
}

fn wasmDestroy(ptr: anytype) void {
    host.allocator.destroy(ptr);
}

export fn lex(writer_id: usize, pointer: [*]const u8) void {
    var writer = OsWriter.init(writer_id);
    defer writer.deinit();
    const source = getSlice(pointer);
    lexSource(writer.writer().any(), source) catch |err| {
        @panic(@errorName(err));
    };
}
// fn _generateAst(writer: std.io.AnyWriter, source: []const u8) !void {
//     var errors = try ErrorManager.init(host.allocator);
//     defer errors.deinit();
//     var ast = try Ast.parse(host.allocator, &errors, source);
//     defer ast.deinit();
//     try serializer.writeJSON(Ast, writer, &ast, &ast.node_lists);
// }
// export fn generateAst(writer_id: usize, pointer: [*]const u8) void {
//     var writer = OsWriter.init(writer_id);
//     defer writer.deinit();
//     const source = getSlice(pointer);
//     _generateAst(writer.writer().any(), source) catch |err| {
//         @panic(@errorName(err));
//     };
// }

fn _compile(writer: std.io.AnyWriter, source: []const u8) !void {
    var errors = try ErrorManager.init(host.allocator);
    defer errors.deinit();
    try writer.writeAll("{");
    try writer.writeAll("\"nodes\":");
    var ast = Ast.parse(host.allocator, &errors, source) catch |err| {
        try writer.print("{{ \"error\":\"{s}\" }}", .{@errorName(err)});
        try writer.writeAll("}");
        return;
    };
    defer ast.deinit();
    try writer.writeAll("[");
    for (0..ast.nodes.len) |i| {
        if (i > 0) try writer.writeAll(",");
        try serializer.writeJSON(Ast.Node, writer, ast.nodes.get(i), .{
            .lists = &ast.node_lists,
        });
    }
    try writer.writeAll("]");
    try writer.writeAll(",\"hir\":");

    var hir = HirBuilder.gen(host.allocator, &ast, &errors) catch |err| {
        try writer.print("{{ \"error\":\"{s}\" }}", .{@errorName(err)});
        try writer.writeAll("}");
        return;
    };
    defer hir.deinit();

    try serializer.writeJSON([]const Hir.Inst, writer, hir.insts.items, .{
        .lists = &hir.lists,
    });

    try writer.writeAll(",\"mir_instructions\":");
    var mir = MirBuilder.gen(host.allocator, &hir, &errors) catch |err| {
        try writer.print("{{ \"error\":\"{s}\" }}", .{@errorName(err)});
        try writer.writeAll("}");
        return;
    };
    defer mir.deinit();
    try serializer.writeJSON([]const Mir.Instruction, writer, mir.instructions.items, .{
        .lists = &mir.lists,
        .interned = &mir.strings,
    });
    try writer.writeAll(",\"mir_values\":");
    try serializer.writeJSON([]const Mir.Value, writer, mir.values.items, .{
        .lists = &mir.lists,
        .interned = &mir.strings,
    });
    try writer.writeAll(",\"mir_types\":");
    try serializer.writeJSON([]const Mir.Type, writer, mir.types.items, .{
        .lists = &mir.lists,
        .interned = &mir.strings,
    });

    try writer.writeAll("}");
}

export fn compile(writer_id: usize, pointer: [*]const u8) void {
    var writer = OsWriter.init(writer_id);
    defer writer.deinit();
    const source = getSlice(pointer);

    _compile(writer.writer().any(), source) catch |err| {
        @panic(@errorName(err));
    };
}
