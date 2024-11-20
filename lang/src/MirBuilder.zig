const std = @import("std");
const Mir = @import("Mir.zig");
const Hir = @import("Hir.zig");
const HirBuilder = @import("HirBuilder.zig");
const Ast = @import("Ast.zig");
const ErrorManager = @import("ErrorManager.zig");
const assert = @import("assert.zig");
const InternedSlice = @import("InternedStrings.zig").InternedSlice;
const Logger = @import("IndentedWriter.zig");

const Allocator = std.mem.Allocator;
const Self = @This();

errors: *ErrorManager,
hir: *Hir,
mir: *Mir,
allocator: Allocator,
arena: std.heap.ArenaAllocator,
logger: Logger,
scope_children_map: std.AutoHashMapUnmanaged(u32, std.ArrayList(Scope)) = .{},
// Hir Inst '#{d}'
// Type '%{d}'
pub fn deinit(self: *Self) void {
    self.arena.deinit();
}

pub fn gen(allocator: Allocator, hir: *Hir, errors: *ErrorManager) !Mir {
    var mir = try Mir.init(allocator);
    var builder = Self{
        .logger = Logger.init(std.io.getStdErr().writer().any()),
        .errors = errors,
        .hir = hir,
        .mir = &mir,
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
    };
    defer builder.deinit();
    var scope: Scope = .{ .root = .{ .builder = &builder } };
    _ = try scope.resolveType(Hir.Inst.RootIndex);

    // try builder.genModule(Hir.Inst.RootIndex);

    return mir;
}
pub fn getNodeSlice(self: *Self, node_index: Ast.Node.Index) []const u8 {
    const tag = self.hir.ast.getNodeTag(node_index);
    assert.fmt(tag == .identifier or tag == .number_literal or tag == .string_literal, "expected identifier or number_literal or string_literal, got {}", .{tag});
    const token = self.hir.ast.getNodeStartToken(node_index);
    return self.hir.ast.getTokenSlice(token);
}
pub fn internNodeSlice(self: *Self, node_index: Ast.Node.Index) !InternedSlice {
    const tag = self.hir.ast.getNodeTag(node_index);
    assert.fmt(tag == .identifier or tag == .number_literal or tag == .string_literal, "expected identifier or number_literal or string_literal, got {}", .{tag});
    const token = self.hir.ast.getNodeStartToken(node_index);
    const slice = self.hir.ast.getTokenSlice(token);
    return try self.mir.strings.intern(slice);
}
pub fn getSlice(self: *Self, slice: InternedSlice) []const u8 {
    return self.mir.strings.getSlice(slice);
}
pub fn newList(self: *Self) Mir.ChildList {
    return self.mir.lists.new(self.mir.allocator);
}
pub fn getHirInst(self: *Self, index: Hir.Inst.Index) Hir.Inst {
    return self.hir.insts.items[index];
}
pub fn getValue(self: *Self, index: Mir.Value.Index) ?Mir.Value {
    const i = index.toInt() orelse return null;
    return self.mir.values.items[@intCast(i)];
}
pub fn getType(self: *Self, index: Mir.Type.Index) ?Mir.Type {
    const i = index.toInt() orelse return null;
    return self.mir.types.items[@intCast(i)];
}
pub fn assertHirInst(self: *Self, index: Hir.Inst.Index, expected: Hir.Inst.Enum) void {
    const actual = self.getHirInst(index);
    assert.fmt(std.meta.activeTag(actual) == expected, "expected {s}, got {s}", .{ @tagName(expected), @tagName(actual) });
}
pub fn iterHirList(self: *Self, index: Hir.Inst.List) Hir.Lists.ListIter {
    return self.hir.lists.iterList(index);
}
pub fn reserveDefinitionIndex(self: *Self) !Mir.Definition.Index {
    const index: Mir.Definition.Index = @intCast(self.mir.definitions.items.len);
    try self.mir.definitions.append(self.allocator, undefined);
    return index;
}
pub fn pushDefinition(self: *Self, definition: Mir.Definition) !Mir.Definition.Index {
    const index: Mir.Definition.Index = @intCast(self.mir.definitions.items.len);
    try self.mir.definitions.append(self.allocator, definition);
    return index;
}
pub fn reserveTypeIndex(self: *Self) !Mir.Type.Index {
    const index = Mir.Type.Index.fromInt(@intCast(self.mir.types.items.len));
    try self.mir.types.append(self.allocator, undefined);
    return index;
}
pub fn pushType(self: *Self, ty: Mir.Type) !Mir.Type.Index {
    const index = Mir.Type.Index.fromInt(@intCast(self.mir.types.items.len));
    try self.mir.types.append(self.allocator, ty);
    return index;
}
pub fn setType(self: *Self, index: Mir.Type.Index, ty: Mir.Type) void {
    self.mir.types.items[index.toInt() orelse unreachable] = ty;
}
pub fn pushValue(self: *Self, value: Mir.Value) !Mir.Value.Index {
    const index = Mir.Value.Index.fromInt(@intCast(self.mir.values.items.len));
    try self.mir.values.append(self.allocator, value);
    return index;
}
pub fn pushInstruction(self: *Self, inst: Mir.Instruction) !Mir.Instruction.Index {
    const index: Mir.Instruction.Index = @intCast(self.mir.instructions.items.len);
    try self.mir.instructions.append(self.allocator, inst);
    return index;
}
pub fn getInstruction(self: *Self, index: Mir.Instruction.Index) Mir.Instruction {
    return self.mir.instructions.items[index];
}
pub fn genModule(self: *Self, hir_inst: Hir.Inst.Index) !void {
    try self.logger.open("#{d} genModule", .{hir_inst});
    defer self.logger.close();
    self.assertHirInst(hir_inst, .mod_decl);
    const inst = self.getHirInst(hir_inst);
    _ = inst; // autofix

    var module = try ModuleWip.init(self, .{ .root = .{ .builder = self } }, hir_inst);

    try module.collectSymbols();
    var symbols_iter = module.symbols_table.iterator();
    while (symbols_iter.next()) |entry| {
        const index = try module.resolveSymbol(entry.key_ptr.*);
        _ = index; // autofix
    }

    try module.commit();
}
const Error = error{
    SymbolNotFound,
    CircularDependency,
} || std.io.AnyWriter.Error || std.mem.Allocator.Error;

const SymbolWip = struct {
    builder: *Self,
    hir_inst: Hir.Inst.Index,
    state: State = .idle,
    pub const State = union(enum) {
        idle: void,
        resolving: void,
        resolved: Mir.Type.Index,
    };
};
pub const ModuleWip = struct {
    parent: Scope,
    builder: *Self,
    hir_inst: Hir.Inst.Index,
    index: Mir.Type.Index,
    symbols_map: Symbol.Map,

    pub fn init(builder: *Self, parent: Scope, hir_inst: Hir.Inst.Index) Error!ModuleWip {
        try builder.logger.printLnIndented("#{d} ModuleWip.init", .{hir_inst});

        return .{
            .parent = parent,
            .builder = builder,
            .hir_inst = hir_inst,
            .index = try builder.reserveTypeIndex(),
            // .symbols_table = std.AutoHashMap(InternedSlice, SymbolWip).init(builder.arena.allocator()),
            .symbols_map = Symbol.Map.init(builder.arena.allocator()),
        };
    }
    pub fn findSymbol(self: *ModuleWip, key: Symbol.Key) !?Symbol {
        _ = try self.resolveSymbol(key);
        return self.symbols_map.get(key);
    }
    pub fn collectSymbols(self: *ModuleWip) Error!void {
        try self.builder.logger.open("#{d} ModuleWip.collectSymbols", .{self.hir_inst});
        defer self.builder.logger.close();

        const inst = self.builder.getHirInst(self.hir_inst);
        var iter_decl_inst = self.builder.iterHirList(inst.mod_decl.declarations);
        while (iter_decl_inst.next()) |decl_inst_index| {
            const decl_inst: Hir.Inst = self.builder.getHirInst(decl_inst_index);
            const name = try self.builder.internNodeSlice(decl_inst.global_decl.name_node);
            try self.builder.logger.printLnIndented(
                "#{d} collected .{s} \"{s}\"",
                .{ decl_inst_index, @tagName(decl_inst), self.builder.getSlice(name) },
            );
            try self.symbols_map.put(
                decl_inst_index,
                .{
                    .global = .{
                        .idle = .{
                            .name = name,
                            .inst = decl_inst_index,
                        },
                    },
                },
            );
        }
    }
    pub fn resolveSymbol(self: *ModuleWip, key: Symbol.Key) Error!Mir.Type.Index {
        try self.builder.logger.open("#{d} ModuleWip.resolveSymbol", .{key});
        defer self.builder.logger.close();

        // const key: Symbol.Key = .{ .name = name };
        const symbol = self.symbols_map.get(key) orelse return error.SymbolNotFound;
        // const type_wip = TypeWip.init(
        //     self.builder,
        //     self.parent,
        //     symbol.hir_inst,
        // );
        const resolved_type = switch (symbol.global) {
            .idle => |idle| blk: {
                try self.builder.logger.printLnIndented("#{d} ModuleWip.resolveSymbol idle", .{idle.inst});
                const scope: Scope = .{ .module = self };
                const resolved = try scope.resolveType(idle.inst);

                // symbol.state = .{ .resolved = resolved };
                try self.symbols_map.put(key, .{
                    .global = .{
                        .resolved = .{
                            .scope_index = scope.module.index,
                            .name = idle.name,
                            .type = resolved,
                        },
                    },
                });

                break :blk resolved;
            },
            .resolving => return error.CircularDependency,
            .resolved => |resolved| resolved.type,
        };
        // try self.builder.logger.printLnIndented("#{d} ModuleWip.resolveSymbol resolved: {d}", .{ symbol.hir_inst, resolved_type });
        return resolved_type;
    }
};

const TypeWip = struct {
    builder: *Self,
    hir_inst: Hir.Inst.Index,
    scope: Scope,
    // state: State = .idle,
    pub fn init(builder: *Self, scope: Scope, hir_inst: Hir.Inst.Index) TypeWip {
        return .{
            .builder = builder,
            .hir_inst = hir_inst,
            .scope = scope,
        };
    }
    pub fn resolve(self: *TypeWip) Error!Mir.Type.Index {
        try self.builder.logger.open("#{d} TypeWip.resolve", .{self.hir_inst});
        defer self.builder.logger.close();

        const type_inst = self.builder.getHirInst(self.hir_inst);
        const resolved = switch (type_inst) {
            .mod_decl => try self.resolveModule(),
            .global_decl => try self.resolveDecl(),
            .fn_decl => try self.resolveFnDecl(),
            .ty_number => .number,
            .inline_block, .block, .if_expr => try self.resolveBlockDecl(),
            .ty_i32 => .i32,
            .ty_i64 => .i64,
            .ty_f32 => .f32,
            .ty_f64 => .f64,
            else => {
                std.debug.panic("unimplemented type_inst: {s}", .{@tagName(type_inst)});
            },
        };
        try self.builder.logger.printLnIndented("#{d} TypeWip.resolve resolved to: '{s}'", .{ self.hir_inst, if (self.builder.getType(resolved)) |res| @tagName(res) else @tagName(resolved) });
        return resolved;
    }
    pub fn resolveModule(self: *TypeWip) Error!Mir.Type.Index {
        try self.builder.logger.open("#{d} TypeWip.resolveModule", .{self.hir_inst});
        defer self.builder.logger.close();

        var module = try ModuleWip.init(self.builder, .{ .root = .{ .builder = self.builder } }, self.hir_inst);

        try module.collectSymbols();
        var symbols_iter = module.symbols_map.iterator();
        var decls = self.builder.newList();
        while (symbols_iter.next()) |entry| {
            switch (entry.value_ptr.global) {
                .idle => |idle| {
                    const index = try module.resolveSymbol(entry.key_ptr.*);
                    const decl_type = try self.builder.pushType(.{
                        .decl = .{
                            .name = idle.name,
                            .type = index,
                            .init = null,
                        },
                    });
                    try decls.append(decl_type.asInt());
                },
                .resolved => |resolved| {
                    const decl_type = try self.builder.pushType(.{
                        .decl = .{
                            .name = resolved.name,
                            .type = resolved.type,
                            .init = null,
                        },
                    });
                    try decls.append(decl_type.asInt());
                },
                else => unreachable,
            }
        }
        self.builder.setType(module.index, .{ .module = .{
            .decls = try decls.commit(),
        } });
        return module.index;
    }
    pub fn resolveDecl(self: *TypeWip) Error!Mir.Type.Index {
        try self.builder.logger.open("#{d} TypeWip.resolveDecl", .{self.hir_inst});
        defer self.builder.logger.close();
        const decl_inst = self.builder.getHirInst(self.hir_inst);
        const type_index = decl_inst.global_decl.type.?;
        return try self.scope.resolveType(type_index);
    }
    pub fn resolveFnDecl(self: *TypeWip) Error!Mir.Type.Index {
        try self.builder.logger.open("#{d} TypeWip.resolveFnDecl", .{self.hir_inst});
        defer self.builder.logger.close();

        const fn_inst = self.builder.getHirInst(self.hir_inst).fn_decl;

        try self.builder.logger.open("#{d} resolve return type", .{fn_inst.return_type});
        const return_type = try self.scope.resolveType(fn_inst.return_type);
        self.builder.logger.close();

        var fn_wip = FnWip.init(
            self.builder,
            self.scope,
            self.hir_inst,
            return_type,
        );

        var iter_params = self.builder.iterHirList(fn_inst.params);
        while (iter_params.next()) |param_index| {
            try self.builder.logger.open("#{d} resolve param", .{param_index});
            defer self.builder.logger.close();

            const param_inst = self.builder.getHirInst(param_index).param_decl;
            const param_type = try self.scope.resolveType(param_inst.ty);
            const name = try self.builder.internNodeSlice(param_inst.name_node);
            try fn_wip.pushParameter(
                name,
                param_type,
                param_index,
            );
        }
        try self.builder.logger.open("#{d} resolve body", .{self.hir_inst});
        try fn_wip.resolveBody();
        self.builder.logger.close();

        return try fn_wip.commit();
    }
    pub fn resolveBlockDecl(self: *TypeWip) Error!Mir.Type.Index {
        try self.builder.logger.open("#{d} TypeWip.resolveBlockDecl", .{self.hir_inst});
        defer self.builder.logger.close();
        var block = BlockWip.init(self.builder, self.scope, self.hir_inst);
        block.return_type = self.scope.block.return_type;
        try block.resolveInstructions();
        return try block.commit();
    }
};
const Scope = union(enum) {
    module: *ModuleWip,
    @"fn": *FnWip,
    block: *BlockWip,
    root: struct {
        builder: *Self,
    },
    pub fn getBuilder(self: Scope) *Self {
        return switch (self) {
            .module => self.module.builder,
            .root => self.root.builder,
            .@"fn" => self.@"fn".builder,
            .block => self.block.builder,
        };
    }
    pub fn resolveType(self: Scope, inst_index: Hir.Inst.Index) !Mir.Type.Index {
        var builder = self.getBuilder();
        try builder.logger.open("#{d} Scope.resolveType", .{inst_index});
        defer builder.logger.close();

        var type_wip = TypeWip.init(builder, self, inst_index);
        return try type_wip.resolve();
    }
    // pub fn findSymbolType(self: Scope, key: Local.Key) Error!?Mir.Type.Index {
    //     switch (self) {
    //         .@"fn" => |fn_wip| {
    //             if (fn_wip.locals.get(key)) |local| return local.type;
    //         },
    //         .block => |block_wip| {
    //             if (block_wip.locals.get(key)) |local| return local.type;
    //         },
    //         .module => |module_wip| {
    //             switch (key) {
    //                 .named => |name| {
    //                     const index = module_wip.resolveSymbol(name) catch |err| switch (err) {
    //                         error.SymbolNotFound => return null,
    //                         else => return err,
    //                     };
    //                     return index;
    //                 },
    //                 else => return null,
    //             }
    //         },
    //         .root => return null,
    //     }
    //     return null;
    // }
    // pub fn findSymbolTypeRecursive(self: Scope, key: Local.Key) !?Mir.Type.Index {
    //     var builder = self.getBuilder();
    //     try builder.logger.open("Scope.findSymbolTypeRecursive .{s} {}", .{ @tagName(self), key });
    //     defer builder.logger.close();

    //     if (try self.findSymbolType(key)) |index| return index;
    //     switch (self) {
    //         .@"fn" => |fn_wip| return try fn_wip.parent.findSymbolTypeRecursive(key),
    //         .block => |block_wip| return try block_wip.parent.findSymbolTypeRecursive(key),
    //         .module => |module_wip| return try module_wip.parent.findSymbolTypeRecursive(key),
    //         .root => return null,
    //     }
    // }
    pub fn getParent(self: Scope) ?Scope {
        return switch (self) {
            .@"fn" => self.@"fn".parent,
            .block => self.block.parent,
            .module => self.module.parent,
            .root => null,
        };
    }
    pub fn findSymbol(self: Scope, key: Symbol.Key) !?Symbol {
        try self.getBuilder().logger.printLnIndented("Scope.findSymbol .{s} #{d}", .{ @tagName(self), key });
        return switch (self) {
            .@"fn" => |fn_wip| try fn_wip.findSymbol(key),
            .block => |block_wip| try block_wip.findSymbol(key),
            .module => |module_wip| try module_wip.findSymbol(key),
            .root => null,
        };
    }
    pub fn findSymbolRecursive(self: Scope, key: Symbol.Key) !?Symbol {
        if (try self.findSymbol(key)) |symbol| return symbol;
        if (self.getParent()) |parent| return try parent.findSymbolRecursive(key);
        return null;
    }
    pub fn dumpInner(self: *Scope, logger: *Logger) Error!void {
        switch (self) {
            .@"fn" => |fn_wip| {
                try logger.open("#{d} Scope.dumpInner @fn", .{fn_wip.hir_inst});

                defer logger.close();
            },
            .block => |block_wip| {
                try logger.open("#{d} Scope.dumpInner block", .{block_wip.hir_inst});
                defer logger.close();
            },
            .module => |module_wip| {
                try logger.open("#{d} Scope.dumpInner module", .{module_wip.hir_inst});
                defer logger.close();
            },
            .root => |root| {
                _ = root; // autofix
                try logger.open("#{d} Scope.dumpInner root", .{self});

                defer logger.close();
            },
        }
    }
    pub fn dump(self: *Scope) Error!void {
        var logger = Logger.init(std.io.getStdErr().writer().any());
        try self.dumpInner(&logger);
    }
};
const LocalsMap = std.AutoHashMap(Local.Key, Local);
const Local = struct {
    type: Mir.Type.Index,
    pub const Key = union(enum) {
        named: InternedSlice,
        register: u32,
    };
};
const Symbol = union(enum) {
    parameter: struct {
        type: Mir.Type.Index,
        name: InternedSlice,
        index: u32,
    },
    instruction: Mir.Instruction.Index,
    global: Global,
    pub const Global = union(enum) {
        idle: struct {
            name: InternedSlice,
            inst: Hir.Inst.Index,
        },
        resolving: void,
        resolved: struct {
            scope_index: Mir.Type.Index,
            name: InternedSlice,
            type: Mir.Type.Index,
        },
    };
    pub const Key = Hir.Inst.Index;
    pub const Map = std.AutoHashMap(Key, Symbol);
};
pub const FnWip = struct {
    parent: Scope,
    builder: *Self,
    hir_inst: Hir.Inst.Index,
    params: Mir.ChildList,
    return_type: Mir.Type.Index,
    // locals: LocalsMap,
    body: ?BlockWip = null,
    symbols_map: Symbol.Map,

    pub fn init(builder: *Self, parent: Scope, hir_inst: Hir.Inst.Index, return_type: Mir.Type.Index) FnWip {
        return .{
            .parent = parent,
            .builder = builder,
            .hir_inst = hir_inst,
            .params = builder.newList(),
            .return_type = return_type,
            // .locals = LocalsMap.init(builder.arena.allocator()),
            .symbols_map = Symbol.Map.init(builder.arena.allocator()),
        };
    }
    pub fn findSymbol(self: *FnWip, key: Symbol.Key) !?Symbol {
        return self.symbols_map.get(key);
    }
    pub fn pushParameter(self: *FnWip, name: InternedSlice, ty: Mir.Type.Index, hir_inst: Hir.Inst.Index) !void {
        const index = try self.builder.pushType(.{ .param = .{
            .type = ty,
            .name = name,
        } });
        const name_slice = self.builder.mir.strings.getSlice(name);
        try self.builder.logger.printLnIndented("#{d} FnWip.pushParameter [PUSHED #{d}] {s} {s}", .{ self.hir_inst, hir_inst, index.fmt(self.builder.mir), name_slice });
        try self.params.append(index.asInt());
        // try self.symbols_map.put(.{ .register = index.asInt() }, .{

        try self.symbols_map.put(hir_inst, .{
            .parameter = .{
                .name = name,
                .type = ty,
                .index = @intCast(self.params.list.items.len - 1),
            },
        });
    }
    pub fn resolveBody(self: *FnWip) Error!void {
        try self.builder.logger.open("#{d} FnWip.resolveBody", .{self.hir_inst});
        defer self.builder.logger.close();

        const body_inst = self.builder.getHirInst(self.hir_inst).fn_decl.init;
        if (body_inst) |body_inst_index| {
            var block = BlockWip.init(self.builder, .{ .@"fn" = self }, body_inst_index);
            block.return_type = self.return_type;
            try block.resolveInstructions();
            self.body = block;
        } else {
            self.body = null;
            try self.builder.logger.printLnIndented("#{d} FnWip.resolveBody [NO BODY]", .{self.hir_inst});
        }
    }
    pub fn commit(self: *FnWip) Error!Mir.Type.Index {
        return try self.builder.pushType(.{
            .@"fn" = .{
                .params = try self.params.commit(),
                .return_type = self.return_type,
                .body = if (self.body) |*body| try body.commit() else null,
            },
        });
    }
};
pub fn getFloatTypePrecedence(ty: Mir.Type.Index) u8 {
    switch (ty) {
        .number => return 0,
        .f32 => return 2,
        .f64 => return 3,
        .i32 => return 4,
        .i64 => return 5,

        else => std.debug.panic("unimplemented getFloatTypePrecedence: {s}", .{@tagName(ty)}),
    }
}

pub const BlockWip = struct {
    parent: Scope,
    builder: *Self,
    hir_inst: Hir.Inst.Index,
    locals: LocalsMap,
    symbols_map: Symbol.Map,
    instructions: Mir.ChildList,
    instructions_list: std.ArrayListUnmanaged(Mir.Instruction),

    return_type: Mir.Type.Index = .void,

    pub fn init(builder: *Self, parent: Scope, hir_inst: Hir.Inst.Index) BlockWip {
        return .{
            .parent = parent,
            .builder = builder,
            .hir_inst = hir_inst,
            .locals = LocalsMap.init(builder.arena.allocator()),
            .symbols_map = Symbol.Map.init(builder.arena.allocator()),
            .instructions = builder.newList(),
            .instructions_list = .{},
        };
    }
    pub fn findSymbol(self: *BlockWip, key: Symbol.Key) !?Symbol {
        return self.symbols_map.get(key);
    }
    pub fn resolveInstructions(self: *BlockWip) Error!void {
        try self.builder.logger.open("#{d} BlockWip.resolveInstructions", .{self.hir_inst});
        defer self.builder.logger.close();
        const inst = self.builder.getHirInst(self.hir_inst);
        switch (inst) {
            .block, .inline_block => |block_inst| {
                var iter = self.builder.iterHirList(block_inst.instructions);
                while (iter.next()) |inst_index| {
                    _ = try self.resolveSingleInstruction(inst_index);
                }
            },
            else => unreachable,
        }
    }
    pub fn resolveOperand(self: *BlockWip, operand: Hir.Inst.Index) Error!Mir.Instruction.Index {
        const symbol: Symbol = try self.findSymbol(operand) orelse try self.parent.findSymbolRecursive(operand) orelse {
            try self.builder.logger.printLnIndented("BlockWip.resolveOperand [UNRESOLVED OPERAND] (#{d})", .{operand});
            return error.UnresolvedSymbol;
        };
        const inst_index = switch (symbol) {
            .instruction => |instruction| instruction,
            .parameter => |parameter| try self.pushInstruction(.{
                .is_comptime = false,
                .op = .param_get,
                .type = parameter.type,
                .data = .{ .scoped = .{
                    .name = parameter.name,
                    .scope_index = parameter.type,
                    .index = parameter.index,
                } },
            }),
            .global => |global| global: {
                break :global try self.pushInstruction(.{
                    .is_comptime = false,
                    .op = .global_get,
                    .type = global.resolved.type,
                    .data = .{
                        .scoped = .{
                            .name = global.resolved.name,
                            .scope_index = global.resolved.scope_index,
                            .index = null,
                        },
                    },
                });
            },
        };
        // const instruction = self.getInstruction(inst_index);
        // switch (instruction.type) {
        //     .number, .type_f32, .type_f64 => {
        //         return inst_index;
        //     },
        //     else => |ty| {
        //         std.debug.panic("Expected number, got {s}", .{@tagName(ty)});
        //     },
        // }
        return inst_index;
    }
    pub fn resolveOperandsType(self: *BlockWip, lhs: Mir.Instruction.Index, rhs: Mir.Instruction.Index) Error!Mir.Type.Index {
        const lhs_inst = self.getInstruction(lhs);
        const rhs_inst = self.getInstruction(rhs);
        if (lhs_inst.type == rhs_inst.type) return lhs_inst.type;
        const lhs_precedence = getFloatTypePrecedence(lhs_inst.type);
        const rhs_precedence = getFloatTypePrecedence(rhs_inst.type);
        if (lhs_precedence > rhs_precedence) return lhs_inst.type;
        return rhs_inst.type;
    }
    pub fn resolveSingleInstruction(self: *BlockWip, hir_inst_index: Hir.Inst.Index) Error!Mir.Instruction.Index {
        const hir_inst = self.builder.getHirInst(hir_inst_index);
        try self.builder.logger.open("#{d} BlockWip.resolveSingleInstruction (.{s})", .{ hir_inst_index, @tagName(hir_inst) });
        defer self.builder.logger.close();

        switch (hir_inst) {
            .comptime_number => {
                const value_index = try self.builder.resolveValue(hir_inst_index);

                const inst = try self.pushInstruction(.{
                    .is_comptime = true,
                    .op = .constant,
                    .type = .number,
                    .data = .{
                        .value = value_index,
                    },
                });
                try self.putSymbol(hir_inst_index, .{
                    .instruction = inst,
                });
                return inst;
            },
            .add,
            .sub,
            .mul,
            .div,
            .gt,
            .lt,
            => |bin_op| {
                const op: Mir.Instruction.Op = switch (std.meta.activeTag(hir_inst)) {
                    inline .add,
                    .sub,
                    .mul,
                    .div,
                    .gt,
                    .lt,
                    => |tag| Mir.Instruction.Op.fromString(@tagName(tag)),
                    else => unreachable,
                };
                const lhs_inst_index = try self.resolveOperand(bin_op.lhs);
                const rhs_inst_index = try self.resolveOperand(bin_op.rhs);

                const ty = try self.resolveOperandsType(lhs_inst_index, rhs_inst_index);

                const inst = try self.pushInstruction(.{
                    .op = op,
                    .is_comptime = false,
                    .type = ty,
                    .data = .{
                        .binOp = .{
                            .lhs = try self.castIfDifferent(lhs_inst_index, ty),
                            .rhs = try self.castIfDifferent(rhs_inst_index, ty),
                        },
                    },
                });

                try self.putSymbol(hir_inst_index, .{
                    .instruction = inst,
                });
                return inst;
            },
            .ret => |ret_inst| {
                if (ret_inst.operand == 0) {
                    if (self.return_type == .void) {
                        const ret_inst_index = try self.pushInstruction(.{
                            .op = .ret,
                            .is_comptime = false,
                            .type = .void,
                            .data = .{ .void = {} },
                        });
                        return ret_inst_index;
                    }
                    return error.MissingReturn;
                } else {
                    if (self.return_type == .void) {
                        return error.ExpectedVoid;
                    }
                }
                const operand_inst = self.builder.getHirInst(ret_inst.operand);
                std.debug.print("operand_inst: {s}\n", .{@tagName(operand_inst)});
                const symbol = try self.findSymbol(ret_inst.operand) orelse try self.parent.findSymbolRecursive(ret_inst.operand) orelse {
                    try self.builder.logger.printLnIndented("BlockWip.resolveSingleInstruction [UNRESOLVED RET] (#{d})", .{ret_inst.operand});
                    return error.UnresolvedSymbol;
                };

                switch (symbol) {
                    .instruction => |index| {
                        const inst_index = try self.castIfDifferent(index, self.return_type);
                        const inst = self.getInstruction(inst_index);

                        const ret_inst_index = try self.pushInstruction(.{
                            .op = .ret,
                            .is_comptime = inst.is_comptime,
                            .type = self.return_type,
                            .data = .{ .instruction = inst_index },
                        });
                        // try self.putSymbol(hir_inst_index, .{
                        //     .instruction = inst,
                        // });
                        return ret_inst_index;
                    },
                    else => unreachable,
                }
            },
            .block, .inline_block => |block_inst| {
                var block = BlockWip.init(self.builder, .{ .block = self }, hir_inst_index);
                block.return_type = self.return_type;
                var iter = self.builder.iterHirList(block_inst.instructions);
                while (iter.next()) |inst_index| {
                    _ = try self.resolveSingleInstruction(inst_index);
                }
                const committed_type = try block.commit();
                const committed_inst = try self.pushInstruction(.{
                    .op = .block,
                    .is_comptime = false,
                    .type = committed_type,
                    .data = .{ .type = committed_type },
                });

                try self.putSymbol(hir_inst_index, .{ .instruction = committed_inst });
                return committed_inst;
            },
            .param_get, .global_get => |unary_op| {
                const inst_index = try self.resolveOperand(unary_op.operand);

                // const inst_index = try self.castIfDifferent(param_get.operand, .number);
                try self.putSymbol(hir_inst_index, .{ .instruction = inst_index });
                return inst_index;
            },
            .if_expr => |if_expr| {
                const cond_symbol = try self.findSymbol(if_expr.cond) orelse try self.parent.findSymbolRecursive(if_expr.cond) orelse {
                    try self.builder.logger.printLnIndented("BlockWip.resolveSingleInstruction [UNRESOLVED IF EXPR COND] (#{d})", .{if_expr.cond});
                    return error.UnresolvedSymbol;
                };
                const cond_inst_index = switch (cond_symbol) {
                    .instruction => |index| index,
                    else => unreachable,
                };

                var scope: Scope = .{ .block = self };
                const inst = try self.pushInstruction(.{
                    .op = .if_expr,
                    .is_comptime = false,
                    .type = .void,
                    .data = .{ .if_expr = .{
                        .cond = cond_inst_index,
                        .then_body = try scope.resolveType(if_expr.then_body),
                        .else_body = if (if_expr.else_body) |else_body| try scope.resolveType(else_body) else null,
                    } },
                });
                return inst;
            },
            .loop => |loop| {
                var scope: Scope = .{ .block = self };
                const instruction_index = try self.reserveInstructionIndex();
                try self.putSymbol(hir_inst_index, .{ .instruction = instruction_index });
                const body_inst_index = try scope.resolveType(loop.body);
                self.setInstruction(instruction_index, .{
                    .op = .loop,
                    .is_comptime = false,
                    .type = .void,
                    .data = .{ .loop = .{ .body = body_inst_index } },
                });
                return instruction_index;
            },
            .br => |br| {
                var scope: Scope = .{ .block = self };
                const operand_symbol = try scope.findSymbolRecursive(br.operand) orelse self.builder.logger.panic("BlockWip.resolveSingleInstruction [UNRESOLVED BR] (#{d})", .{br.operand});
                _ = operand_symbol; // autofix
                const inst = try self.pushInstruction(.{
                    .op = .br,
                    .is_comptime = false,
                    .type = .void,
                    .data = .{ .type = .void },
                });
                return inst;
            },
            else => {
                std.debug.panic("unimplemented hir_inst: {s}", .{@tagName(hir_inst)});
            },
        }
    }

    pub fn castIfDifferent(self: *BlockWip, operand: Mir.Instruction.Index, ty: Mir.Type.Index) Error!Mir.Instruction.Index {
        const inst = self.getInstruction(operand);
        if (inst.type == ty) return operand;
        return try self.pushInstruction(.{
            .is_comptime = inst.is_comptime,
            .op = .as,
            .type = ty,
            .data = .{ .type = inst.type },
        });
    }
    pub fn reserveInstructionIndex(self: *BlockWip) Error!Mir.Instruction.Index {
        const index: Mir.Instruction.Index = @intCast(self.instructions_list.items.len);
        try self.instructions_list.append(self.builder.arena.allocator(), undefined);
        return index;
    }
    pub fn setInstruction(
        self: *BlockWip,
        index: Mir.Instruction.Index,
        inst: Mir.Instruction,
    ) void {
        self.instructions_list.items[index] = inst;
    }
    pub fn pushInstruction(
        self: *BlockWip,
        inst: Mir.Instruction,
    ) Error!Mir.Instruction.Index {
        const index: Mir.Instruction.Index = @intCast(self.instructions_list.items.len);
        try self.instructions_list.append(self.builder.arena.allocator(), inst);
        try self.builder.logger.printLnIndented("#{d} BlockWip.pushInstruction [PUSHED #{d}] '{s}'", .{ self.hir_inst, index, @tagName(inst.op) });
        return index;

        // try self.instructions.append(index);
        // return index;
    }
    pub fn getInstruction(self: *BlockWip, index: Mir.Instruction.Index) Mir.Instruction {
        return self.instructions_list.items[index];
    }
    pub fn putSymbol(self: *BlockWip, key: Symbol.Key, value: Symbol) !void {
        try self.builder.logger.open("#{d} BlockWip.putSymbol [#{d}]", .{ self.hir_inst, key });
        switch (value) {
            .instruction => |index| {
                const inst = self.getInstruction(index);
                try self.builder.logger.printLnIndented("[#{d}] instruction -> '{s}'", .{ key, @tagName(inst.op) });
            },
            .parameter => |param| {
                // const param_inst = self.builder.getHirInst(param.instruction);
                try self.builder.logger.printLnIndented("[#{d}] parameter -> '{s}'", .{ key, self.builder.getSlice(param.name) });
            },
            .global => |global| {
                try self.builder.logger.printLnIndented("[#{d}] global -> '{s}'", .{ key, self.builder.getSlice(global.resolved.name) });
            },
        }
        defer self.builder.logger.close();
        try self.symbols_map.put(key, value);
    }
    pub fn commit(self: *BlockWip) Error!Mir.Type.Index {
        const start: u32 = @intCast(self.builder.mir.instructions.items.len);
        try self.builder.mir.instructions.appendSlice(self.builder.mir.allocator, self.instructions_list.items);
        return try self.builder.pushType(.{
            .block = .{
                // .locals = try self.locals.commit(),
                .name = null, // TODO
                .instruction_start = start,
                .instruction_count = @intCast(self.instructions_list.items.len),
            },
        });
    }
};

pub fn resolveValue(self: *Self, value_inst: Hir.Inst.Index) Error!Mir.Value.Index {
    const inst = self.getHirInst(value_inst);
    return switch (inst) {
        .comptime_number => |comptime_number_inst| {
            const slice = self.getNodeSlice(comptime_number_inst);
            return try self.pushValue(.{ .float = try std.fmt.parseFloat(f64, slice) });
        },
        else => std.debug.panic("unimplemented value_inst: {s}", .{@tagName(inst)}),
    };
    // return try self.pushValue(.{ .constant = value_inst });
}
test "MirBuilder" {
    const test_allocator = std.testing.allocator;
    const file = try std.fs.cwd().openFile("./playground.sheet", .{});
    defer file.close();
    const source = try file.readToEndAlloc(test_allocator, 1024 * 1024);
    defer test_allocator.free(source);
    std.debug.print("source:\n\n{s}\n", .{source});

    var errors = try ErrorManager.init(test_allocator);
    defer errors.deinit();
    var ast = try Ast.parse(test_allocator, &errors, source);
    defer ast.deinit();
    std.debug.print("AST:\n", .{});
    try ast.format(std.io.getStdErr().writer().any(), 0, .{});

    var hir = try HirBuilder.gen(test_allocator, &ast, &errors);
    defer hir.deinit();
    std.debug.print("Hir:\n{}\n", .{hir});

    var mir = try Self.gen(test_allocator, &hir, &errors);

    defer mir.deinit();
    std.debug.print("Mir:\n{}\n", .{mir});
}
