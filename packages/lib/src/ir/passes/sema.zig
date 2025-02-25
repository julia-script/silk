const Module = @import("../Module.zig");
const std = @import("std");
const HashMap = std.hash_map.AutoHashMapUnmanaged;
const DfgAnalysis = @import("./DfgAnalysis.zig");

allocator: std.mem.Allocator,
arena: std.heap.ArenaAllocator,
decl_status: DeclStatusMap = .{},
def_status: DefStatusMap = .{},

mod: *Module,

pub const Status = enum {
    idle,
    pending,
    completed,
};
const DeclPassStatus = struct {
    decl_status: Status = .idle,
    def_status: Status = .idle,
};

// const DeclPassStatusMap = HashMap(Module.Decl.Ref, DeclPassStatus);
const DeclStatusMap = HashMap(Module.Decl.Ref, Status);
const DefStatusMap = HashMap(Module.Definition.Ref, Status);
const Self = @This();

pub fn setDeclStatus(self: *Self, decl: Module.Decl.Ref, status: Status) !void {
    const gop = try self.decl_status.getOrPut(self.arena.allocator(), decl);
    if (gop.found_existing) {
        gop.value_ptr.* = status;
        return;
    }
    gop.value_ptr.* = status;
}

pub fn setDefStatus(self: *Self, def: Module.Definition.Ref, status: Status) !void {
    const gop = try self.def_status.getOrPut(self.arena.allocator(), def);
    if (gop.found_existing) {
        gop.value_ptr.* = status;
        return;
    }
    gop.value_ptr.* = status;
}

pub fn getDeclStatus(self: *Self, decl: Module.Decl.Ref) Status {
    return if (self.decl_status.get(decl)) |gop_value| gop_value else return .idle;
}

pub fn getDefStatus(self: *Self, def: Module.Definition.Ref) Status {
    return if (self.def_status.get(def)) |gop_value| gop_value else return .idle;
}

pub fn defStatusIs(self: *Self, def: Module.Decl.Ref, status: Status) bool {
    return self.getDefStatus(def) == status;
}

pub fn declStatusIs(self: *Self, decl: Module.Decl.Ref, status: Status) bool {
    return self.getDeclStatus(decl) == status;
}

pub fn run(allocator: std.mem.Allocator, module: *Module) !void {
    var self: Self = .{
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .mod = module,
    };
    defer self.arena.deinit();

    try self.analyzeNs(.{ .idx = 0 });
}
pub fn analyzeNs(self: *Self, ns_ref: Module.Namespace.Ref) !void {
    const ns = self.mod.namespaces.getPtr(ns_ref);
    std.debug.print("Analyzing namespace {s}\n", .{ns.name});
    var iter = ns.declarations.hashmap.iterator();
    while (iter.next()) |entry| {
        const decl_ref = entry.key_ptr.*;
        try self.analyzeDecl(decl_ref);
    }

    iter.index = 0;
    while (iter.next()) |entry| {
        const decl_ref = entry.key_ptr.*;
        try self.analyzeDeclDef(decl_ref);
    }
}

const Error = error{} || std.mem.Allocator.Error;

pub fn analyzeDecl(self: *Self, decl_ref: Module.Decl.Ref) Error!void {
    const status = self.getDeclStatus(decl_ref);
    std.debug.print("Requesting {} declaration analysis, current status: {s}\n", .{ decl_ref, @tagName(status) });
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
pub fn analyzeDeclDef(self: *Self, decl: Module.Decl.Ref) Error!void {
    const def = self.mod.definition_map.get(decl) orelse std.debug.panic("No definition found for '{}", .{decl});
    return try self.analyzeDef(def);
}

pub fn analyzeDef(self: *Self, def_ref: Module.Definition.Ref) Error!void {
    const status = self.getDefStatus(def_ref);
    std.debug.print("Requesting {} definition analysis, current status: {s}\n", .{ def_ref, @tagName(status) });
    // ensure the decl is analyzed
    // try self.analyzeDecl(decl);

    switch (status) {
        .idle => {
            try self.setDefStatus(def_ref, .pending);
        },
        .pending => {
            std.debug.panic("Recursive dependency on {}\n", .{def_ref});
        },
        .completed => {
            std.debug.print("decl {} is completed\n", .{def_ref});
            return;
        },
    }

    var def = self.mod.getDefinition(def_ref);
    var iter = def.dfg.dependencies.iterator();
    std.debug.print("Dependencies: {}\n", .{def.dfg.dependencies.count()});
    while (iter.next()) |dependency| {
        const dep = dependency.*;

        switch (dep) {
            .declaration => |ref| {
                try self.analyzeDecl(ref);
            },
            .definition => |ref| {
                const def_status = self.getDefStatus(ref);
                if (def_status == .pending) {
                    // recursion.. will need another pass
                    continue;
                }
                try self.analyzeDef(ref);
            },
        }
    }

    def.dfg = try DfgAnalysis.analyze(self.allocator, self.mod, def_ref);
    try self.setDefStatus(def_ref, .completed);

    const result = def.dfg.result orelse return;
    if (!result.isType()) return;
    const ty = result.value.ty;
    const ty_data = ty.getTyData(self.mod) orelse return;

    switch (ty_data) {
        .@"struct" => |str| {
            if (str.associated_ns) |ns_ref| {
                try self.analyzeNs(ns_ref);
            }
        },
        else => {},
    }
}

/// Declaration Analysis
fn analyzeFuncDecl(self: *Self, decl_ref: Module.Decl.Ref) !void {
    const decl = self.mod.getDeclaration(decl_ref);
    const signature = self.mod.getSignature(decl.func.signature);

    for (signature.params.items) |*param| {
        std.debug.print("Analyzing param: {}\n", .{param.ty.display(self.mod)});
        const result = try self.analyzeType(param.ty);
        param.ty = result;
        std.debug.print("finished param with {}\n", .{param.ty.display(self.mod)});
    }
    signature.ret = try self.analyzeType(signature.ret);
}
fn analyzeGlobalDecl(self: *Self, decl_ref: Module.Decl.Ref) !void {
    const decl = self.mod.getDeclaration(decl_ref);
    // const ty = decl.global.ty;
    _ = decl; // autofix

}

pub fn analyzeInlineDefinition(self: *Self, def_ref: Module.Definition.Ref) !Module.TypedValue {
    // TODO: Analyze dfg result
    try self.analyzeDef(def_ref);
    const def = self.mod.getDefinition(def_ref);
    return def.dfg.result orelse std.debug.panic("No result for definition {}\n", .{def_ref});
}
pub fn analyzeType(self: *Self, ty: Module.Ty) !Module.Ty {
    std.debug.print("Analyzing type {}\n", .{ty.display(self.mod)});
    switch (ty) {
        .global => |ref| {
            const def_ref = self.mod.definition_map.get(ref) orelse std.debug.panic("No definition found for decl {}", .{ref});
            const result = try self.analyzeInlineDefinition(def_ref);
            switch (result.ty) {
                .type => {
                    switch (result.value) {
                        .ty => |value_type| {
                            return value_type;
                        },
                        else => {
                            std.debug.panic("Unexpected result type {}\n", .{result});
                        },
                    }
                },
                else => {
                    std.debug.panic("Unexpected result type {}\n", .{result});
                },
            }
        },
        .def => |def_ref| {
            const result = try self.analyzeInlineDefinition(def_ref);
            switch (result.ty) {
                .type => {
                    switch (result.value) {
                        .ty => |value_type| {
                            return value_type;
                        },
                        else => {
                            std.debug.panic("Unexpected result type {}\n", .{result});
                        },
                    }
                },
                else => {
                    std.debug.panic("Unexpected result type {}\n", .{result});
                },
            }
        },
        else => {
            return ty;
        },
    }
}
