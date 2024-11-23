const std = @import("std");
const Mir = @import("Mir.zig");
const ErrorManager = @import("ErrorManager.zig");
const Ast = @import("Ast.zig");
const Hir = @import("Hir.zig");
const AstBuilder = @import("AstGen.zig");
const HirBuilder = @import("HirBuilder.zig");
const MirBuilder = @import("MirBuilder.zig");
const WasmBackend = @import("backend/wasm/Backend.zig");

errors: ErrorManager,
sources: std.StringHashMapUnmanaged(Source),
root: []const u8,
allocator: std.mem.Allocator,
arena: std.heap.ArenaAllocator,
target: Target,

const Target = struct {
    arch: Arch,
    os: Os,
    env: Env,
    const Arch = enum {
        wasm,
    };
    const Os = enum {
        wasm,
    };
    const Env = enum {
        wasm,
    };
};

pub const Source = struct {
    location: Location,
    source: []const u8,
    name: []const u8,
    ast: ?Ast = null,
    hir: ?Hir = null,
    mir: ?Mir = null,
    pub const Location = union(enum) {
        path: []const u8,
        virtual: u64,
    };
    pub fn dump(self: Source) void {
        std.debug.print("{s}\n", .{self.source});
    }
    pub fn compile(self: *Source, compilation: *Self) !void {
        const allocator = compilation.arena.allocator();
        self.ast = try Ast.parse(allocator, &compilation.errors, self.source);
        // std.debug.print("AST:\n", .{});
        // try self.ast.?.format(
        //     std.io.getStdErr().writer().any(),
        //     0,
        //     .{},
        // );
        self.hir = try HirBuilder.gen(allocator, &self.ast.?, &compilation.errors);
        std.debug.print("HIR:\n", .{});
        std.debug.print("{any}\n", .{self.hir});
        self.mir = try MirBuilder.gen(allocator, &self.hir.?, &compilation.errors);
        std.debug.print("MIR:\n", .{});
        std.debug.print("{any}\n", .{self.mir});
    }
};

const Self = @This();

pub fn init(allocator: std.mem.Allocator, root: []const u8) !Self {
    var self = Self{
        .errors = try ErrorManager.init(allocator),
        .sources = .{},
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .root = undefined,
        .target = Target{ .arch = .wasm, .os = .wasm, .env = .wasm },
    };

    try self.addSourceFromFilePath(root);
    self.root = @constCast(&self.sources.valueIterator()).next().?.name;

    return self;
}
pub fn compile(self: *Self) !void {
    var root_source = try self.getRootSource();
    try root_source.compile(self);
    switch (self.target.arch) {
        .wasm => try WasmBackend.compile(self),
    }
}

pub fn createFile(self: *Self, path: []const u8) !std.fs.File {
    _ = self; // autofix
    const out_dir = "out";
    var fba_buffer: [std.fs.max_path_bytes]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&fba_buffer);

    const final_path = try std.fs.path.join(fba.allocator(), &.{ out_dir, path });

    std.debug.print("{s}\n", .{final_path});
    const cwd = std.fs.cwd();
    try cwd.makePath(out_dir);
    // _ = try cwd.statFile(final_path )
    // if (cwd.stat(final_path)) |err| {
    // }
    return try cwd.createFile(final_path, .{});
}
pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.errors.deinit();
}
pub fn getRootSource(self: *Self) !*Source {
    return self.sources.getPtr(self.root) orelse return error.RootSourceNotFound;
}
pub fn addSourceFromFilePath(self: *Self, path: []const u8) !void {
    const allocator = self.arena.allocator();
    const file_path = try allocator.dupe(u8, path);
    const file = try std.fs.cwd().openFile(path, .{});
    defer file.close();
    const name = std.fs.path.basename(file_path);
    const source = try file.readToEndAlloc(allocator, std.math.maxInt(usize));

    try self.sources.put(allocator, name, .{
        .name = name,
        .location = .{ .path = file_path },
        .source = source,
    });
}

test "Compilation" {
    const test_allocator = std.testing.allocator;
    var compilation = try Self.init(test_allocator, "./playground.zig");
    defer compilation.deinit();

    var source_iter = compilation.sources.valueIterator();
    while (source_iter.next()) |s| {
        s.dump();
    }
    try compilation.compile();
    // var errors =
    // var mir = try MirBuilder.gen(test_allocator, &ast, &errors);
}
