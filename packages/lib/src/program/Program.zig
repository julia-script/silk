const std = @import("std");
const Os = @import("./Os.zig");
const Sema = @import("../sema/Sema.zig");
const ErrorManager = @import("../ErrorManager.zig");
const Self = @This();
const Target = @import("./Target.zig");
const WasmBackend = @import("../backend/wasm/Backend.zig");

allocator: std.mem.Allocator,
os: Os,
sema: Sema,
target: Target,
entrypoints: []const []const u8,
root_path: []const u8,
out_dir: []const u8,
arena: std.heap.ArenaAllocator,

pub fn getDeclaration(self: *Self, index: Sema.Declaration.Index) *Sema.Declaration {
    return &self.sema.declarations.items[index];
}
pub const CompileOptions = struct {
    entrypoints: []const []const u8 = &.{"main"},
    out_dir: []const u8 = "out",
    root_path: []const u8,
    target: Target,
};
pub fn init(
    allocator: std.mem.Allocator,
    os: Os,
    error_manager: *ErrorManager,
    options: CompileOptions,
) !Self {
    return Self{
        .allocator = allocator,
        .os = os,
        .sema = try Sema.init(
            allocator,
            error_manager,
            .{
                .pointer_size = options.target.getPointerSize(),
            },
        ),
        .arena = std.heap.ArenaAllocator.init(allocator),
        .target = options.target,
        .entrypoints = options.entrypoints,
        .root_path = options.root_path,
        .out_dir = options.out_dir,
    };
}

pub fn compile(self: *Self) !void {
    var file_content = try self.os.readFile(self.allocator, self.root_path);
    defer file_content.deinit();
    const root_source = try self.sema.makeRootSource(file_content.content, self.root_path);
    try self.sema.analyzeAll(root_source);
    var declaration_map = std.StringHashMap(Sema.Declaration.Index).init(self.allocator);
    defer declaration_map.deinit();

    var codegen = switch (self.target.arch) {
        .wasm32 => blk: {
            var backend = WasmBackend.init(self, self.allocator);
            break :blk backend.codegen();
        },
        else => @panic("Unsupported target architecture"),
    };
    defer codegen.deinit();
    for (self.sema.declarations.items, 0..) |declaration, i| {
        try declaration_map.put(
            self.sema.builder.getSlice(declaration.name)[self.root_path.len + 2 ..],
            i,
        );
    }
    for (self.entrypoints) |entrypoint| {
        const declaration_index = declaration_map.get(entrypoint) orelse {
            std.debug.panic("Declaration not found: '{s}'\n", .{entrypoint});
        };
        // try self.sema.emitDeclaration(declaration_index);
        // try self.emitDeclaration(declaration_index);
        std.debug.print("Compiling '{s}'\n", .{entrypoint});
        try codegen.emitDeclaration(declaration_index);
    }
}

pub fn deinit(self: *Self) void {
    self.sema.deinit();
}

test "Program" {
    var fs = Os.Fs{
        .cwd = std.fs.cwd(),
    };
    var error_manager = try ErrorManager.init(std.testing.allocator);
    defer error_manager.deinit();
    var program = try Self.init(
        std.testing.allocator,
        fs.os(),
        &error_manager,
        .{
            .entrypoints = &.{
                "main",
            },
            .out_dir = ".tmp/out",
            .root_path = "src/tests/cases/playground.sk",
            .target = .{
                .arch = .wasm32,
                .os = .unknown,
                .vendor = .unknown,
            },
        },
    );
    defer program.deinit();
    try program.compile();
    // const declarations = program.sema.declarations.items;

    // for (declarations) |declaration| {
    //     std.debug.print("{s}\n", .{program.sema.builder.getSlice(declaration.name)[program.root_path.len + 2 ..]});
    // }
}
