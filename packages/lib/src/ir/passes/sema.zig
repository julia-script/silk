const Module = @import("../Module.zig");
const std = @import("std");
const HashMap = std.hash_map.AutoHashMapUnmanaged;

allocator: std.mem.Allocator,
arena: std.heap.ArenaAllocator,
decl_status: DeclPassStatusMap = .{},
mod: *Module,

const DeclPassStatus = struct {
    const Status = enum {
        idle,
        pending,
        completed,
    };
    decl_status: Status = .idle,
    def_status: Status = .idle,
};

const DeclPassStatusMap = HashMap(Module.Decl.Ref, DeclPassStatus);
const Self = @This();
pub fn setDeclStatus(self: *Self, decl: Module.Decl.Ref, status: DeclPassStatus.Status) !void {
    const gop = try self.decl_status.getOrPut(self.arena.allocator(), decl);
    if (gop.found_existing) {
        gop.value_ptr.decl_status = status;
        return;
    }
    gop.value_ptr.* = .{ .decl_status = status };
}
pub fn setDefStatus(self: *Self, def: Module.Def.Ref, status: DeclPassStatus.Status) !void {
    const gop = try self.decl_status.getOrPut(self.arena.allocator(), def);
    if (gop.found_existing) {
        gop.value_ptr.def_status = status;
        return;
    }
    gop.value_ptr.* = .{ .def_status = status };
}

pub fn getDeclStatus(self: *Self, decl: Module.Decl.Ref) DeclPassStatus.Status {
    return if (self.decl_status.get(decl)) |gop_value| gop_value.decl_status else return .idle;
}
pub fn getDefStatus(self: *Self, def: Module.Def.Ref) DeclPassStatus.Status {
    return if (self.decl_status.get(def)) |gop_value| gop_value.def_status else return .idle;
}
pub fn defStatusIs(self: *Self, def: Module.Def.Ref, status: DeclPassStatus.Status) bool {
    return self.getDefStatus(def) == status;
}
pub fn declStatusIs(self: *Self, decl: Module.Decl.Ref, status: DeclPassStatus.Status) bool {
    return self.getDeclStatus(decl) == status;
}

pub fn run(allocator: std.mem.Allocator, module: *Module) !void {
    var self: Self = .{
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .mod = module,
    };
    defer self.arena.deinit();
    for (0..module.decls.count()) |i| {
        try self.analyzeDecl(.{ .ref = @intCast(i) });
    }
}
pub fn analyzeDecl(self: *Self, decl_ref: Module.Decl.Ref) !void {
    const decl = self.mod.getDeclaration(decl_ref);
    _ = decl; // autofix
    const status = self.getDeclStatus(decl_ref);
    std.debug.print("Requesting {} analysis, current status: {s}\n", .{ decl_ref, @tagName(status) });
    switch (status) {
        .idle => {
            try self.setDeclStatus(decl_ref, .pending);
        },
        .pending => {
            std.debug.panic("Recursive dependency on {}\n", .{decl_ref});
        },
        .completed => {
            std.debug.print("decl {} is completed\n", .{decl_ref});
            return;
        },
    }
    const decl_data = self.mod.getDeclaration(decl_ref);
    switch (decl_data.*) {
        .func => {
            try self.analyzeFuncDecl(decl_ref);
        },
        .global => {
            try self.analyzeGlobalDecl(decl_ref);
        },
    }

    try self.setDeclStatus(decl_ref, .completed);
}

pub fn analyzeDef(self: *Self, decl: Module.Decl.Ref) !void {
    const status = self.getDeclStatus(decl);
    std.debug.print("Requesting {} analysis, current status: {s}\n", .{ decl, @tagName(status) });
    // ensure the decl is analyzed
    try self.analyzeDecl(decl);
    switch (status) {
        .idle => {
            try self.setDeclStatus(decl, .pending);
        },
        .pending => {
            std.debug.panic("Recursive dependency on {}\n", .{decl});
        },
        .completed => {
            std.debug.print("decl {s} is completed\n", .{decl});
            return;
        },
    }

    try self.setDeclStatus(decl, .completed);
}

/// Declaration Analysis
fn analyzeFuncDecl(self: *Self, decl_ref: Module.Decl.Ref) !void {
    const decl = self.mod.getDeclaration(decl_ref);
    _ = decl; // autofix
}
fn analyzeGlobalDecl(self: *Self, decl_ref: Module.Decl.Ref) !void {
    const decl = self.mod.getDeclaration(decl_ref);
    _ = decl; // autofix
}
