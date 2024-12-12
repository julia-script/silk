const std = @import("std");
const Mir = @import("Mir.zig");
const ErrorManager = @import("ErrorManager.zig");
const Ast = @import("Ast.zig");
const Hir = @import("Hir.zig");
const AstBuilder = @import("AstGen.zig");
const HirBuilder = @import("HirBuilder.zig");
const MirBuilder = @import("MirBuilder2.zig");
const WasmBackend = @import("backend/wasm/Backend.zig");
const dir = @import("./dir.zig");

errors: ErrorManager,
sources: std.StringHashMapUnmanaged(Source),
root: []const u8,
name: []const u8,
stack_memory_size: usize = 1024 * 1024,
output_dir: []const u8,
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
        std.debug.print("AST:\n", .{});
        try self.ast.?.format(
            std.io.getStdErr().writer().any(),
            0,
            .{},
        );
        self.hir = try HirBuilder.gen(allocator, &self.ast.?, &compilation.errors);
        std.debug.print("HIR:\n", .{});
        std.debug.print("{any}\n", .{self.hir});
        self.mir = try Mir.build(allocator, &self.hir.?, &compilation.errors);
        std.debug.print("MIR:\n", .{});
        std.debug.print("{any}\n", .{self.mir});
    }
};

const Self = @This();

const CompilationInitializeOptions = struct {
    root: []const u8,
    output_dir: ?[]const u8 = null,
    name: ?[]const u8 = null,
};
pub fn init(allocator: std.mem.Allocator, options: CompilationInitializeOptions) !Self {
    var self = Self{
        .errors = try ErrorManager.init(allocator),
        .sources = .{},
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .output_dir = options.output_dir orelse "./out",
        .root = undefined,
        .target = Target{ .arch = .wasm, .os = .wasm, .env = .wasm },
        .name = options.name orelse std.fs.path.stem(options.root),
    };

    try self.addSourceFromFilePath(options.root);
    self.root = @constCast(&self.sources.valueIterator()).next().?.name;

    return self;
}

pub fn compile(self: *Self) !void {
    const root_source = try self.getRootSource();

    try root_source.compile(self);
    switch (self.target.arch) {
        .wasm => try WasmBackend.compile(self),
    }
}

pub inline fn getOutputDirPath(self: *Self, sub_dir: []const u8) ![]const u8 {
    var buf: [std.fs.max_path_bytes]u8 = undefined;
    return try dir.bufJoin(buf[0..], &(.{ self.output_dir, sub_dir }));
}
pub inline fn openOutputDir(self: *Self, sub_dir: []const u8, comptime ensure_dir: bool) !std.fs.Dir {
    const dir_path = try self.getOutputDirPath(sub_dir);
    if (ensure_dir) {
        try dir.ensureDir(dir_path);
    }
    return try dir.openDir(dir_path, .{});
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

test "Compilation2" {
    // const test_allocator = std.testing.allocator;
    // var compilation = try Self.init(test_allocator, .{ .root = "./playground.zig", .output_dir = "./.tmp" });
    // defer compilation.deinit();
    // var output_dir = try compilation.openOutputDir("bin/bash", true);
    // defer output_dir.close();
    // const file_a = try output_dir.createFile("hello.txt", .{});
    // defer file_a.close();
}
test "Compilation" {
    const test_allocator = std.testing.allocator;
    var compilation = try Self.init(test_allocator, .{ .root = "./playground.zig", .output_dir = "./.tmp" });
    defer compilation.deinit();

    // var source_iter = compilation.sources.valueIterator();
    // while (source_iter.next()) |s| {
    //     s.dump();
    // }
    try compilation.compile();
    // var errors =
    // var mir = try MirBuilder.gen(test_allocator, &ast, &errors);
}
