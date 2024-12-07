const std = @import("std");
const Mir = @import("Mir.zig");
const Hir = @import("Hir.zig");
const HirBuilder = @import("HirBuilder.zig");
const Ast = @import("Ast.zig");
const ErrorManager = @import("ErrorManager.zig");
const assert = @import("assert.zig");
const InternedSlice = @import("InternedStrings.zig").InternedSlice;
const Logger = @import("Logger.zig");
const Color = @import("Color.zig");
const tw = Color.tw;
const Allocator = std.mem.Allocator;
const Self = @This();
const serializer = @import("serializer.zig");
const host = @import("host.zig");

errors: *ErrorManager,
hir: *Hir,
mir: *Mir,
allocator: Allocator,
arena: std.heap.ArenaAllocator,
logger: Logger,

pub fn deinit(self: *Self) void {
    self.arena.deinit();
}

pub fn gen(allocator: Allocator, hir: *Hir, errors: *ErrorManager) !Mir {
    var mir = try Mir.init(allocator);
    var builder = Self{
        .logger = Logger.init(host.getStdErrWriter(), "MirBuilder"),
        .errors = errors,
        .hir = hir,
        .mir = &mir,
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
    };
    defer builder.deinit();
    // var scope: Scope = .{ .root = .{ .builder = &builder } };
    var root_type_wip = try TypeWip.init(
        &builder,
        .{ .root = .{ .builder = &builder } },
        Hir.Inst.RootIndex,
    );
    _ = try root_type_wip.resolve();

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
pub fn reserveInstructionIndex(self: *Self) !Mir.Instruction.Index {
    const index: Mir.Instruction.Index = @intCast(self.mir.instructions.items.len);
    try self.mir.instructions.append(self.allocator, undefined);
    return index;
}
pub fn setInstruction(self: *Self, index: Mir.Instruction.Index, inst: Mir.Instruction) void {
    self.mir.instructions.items[index] = inst;
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
        const index = try module.resolveGlobalType(entry.key_ptr.*);
        _ = index; // autofix
    }

    try module.commit();
}
const Error = error{
    SymbolNotFound,
    CircularDependency,
} || std.io.AnyWriter.Error || std.mem.Allocator.Error;

pub const ModuleWip = struct {
    id: usize,
    parent: Scope,
    builder: *Self,
    hir_inst: Hir.Inst.Index,
    index: Mir.Type.Index,
    // symbols_map: Symbol.Map,
    globals: std.AutoArrayHashMapUnmanaged(u32, Global) = .{},
    const Wip = union(enum) {
        fn_decl: FnWip,
        mod_decl: ModuleWip,
    };
    const Global = struct {
        name: InternedSlice,
        state: State,
        hir_inst: Hir.Inst.Index,
        wip: Wip,

        pub const State = enum {
            idle,
            resolving,
            resolved_type,
            resolved,
        };
    };

    pub fn init(builder: *Self, parent: Scope, hir_inst: Hir.Inst.Index) Error!ModuleWip {
        Scope.i += 1;
        builder.logger.log("S({d}) ModuleWip.init", .{Scope.i}, tw.cyan_400);

        return .{
            .id = Scope.i,
            .parent = parent,
            .builder = builder,
            .hir_inst = hir_inst,
            .index = try builder.reserveTypeIndex(),
            // .symbols_table = std.AutoHashMap(InternedSlice, SymbolWip).init(builder.arena.allocator()),
            // .symbols_map = Symbol.Map.init(builder.arena.allocator()),
        };
    }
    // pub fn findSymbol(self: *ModuleWip, key: Symbol.Key) !?Symbol {
    //     _ = try self.resolveGlobalType(key);
    //     return self.symbols_map.get(key);
    // }
    pub fn collectSymbols(self: *ModuleWip) Error!void {
        self.builder.logger.open("#{d} ModuleWip.collectSymbols", .{self.hir_inst});
        defer self.builder.logger.close();

        const inst = self.builder.getHirInst(self.hir_inst);
        var iter_decl_inst = self.builder.iterHirList(inst.mod_decl.declarations_list);
        while (iter_decl_inst.next()) |decl_inst_index| {
            const decl_inst: Hir.Inst = self.builder.getHirInst(decl_inst_index);
            const name = try self.builder.internNodeSlice(decl_inst.global_decl.name_node);
            try self.builder.logger.printLnIndented(
                "#{d} collected .{s} \"{s}\"",
                .{ decl_inst_index, @tagName(decl_inst), self.builder.getSlice(name) },
            );
            if (!decl_inst.global_decl.is_fn) @panic("not supported");
            const mod_scope: Scope = .{ .module = self };
            try self.globals.put(
                self.builder.arena.allocator(),
                decl_inst_index,
                .{
                    .name = name,
                    .hir_inst = decl_inst_index,
                    // .type_wip = try TypeWip.init(self.builder, .{ .module = self }, decl_inst_index),
                    .wip = .{
                        .fn_decl = try FnWip.init(
                            self.builder,
                            mod_scope,
                            decl_inst_index,
                            name,
                            .unknown,
                        ),
                    },
                    .state = .idle,
                },
            );
        }
    }
    pub fn resolveGlobalType(self: *ModuleWip, key: u32) Error!Mir.Type.Index {
        self.builder.logger.open("#{d} ModuleWip.resolveGlobalType", .{key});
        defer self.builder.logger.close();

        // const key: Symbol.Key = .{ .name = name };
        var global = self.globals.getPtr(key) orelse return self.builder.logger.panic("Could not resolve global #{d} ({s})", .{
            key,
            @tagName(self.builder.getHirInst(key)),
        });
        // const type_wip = TypeWip.init(
        //     self.builder,
        //     self.parent,
        //     symbol.hir_inst,
        // );
        const resolved_type = switch (global.state) {
            .idle => blk: {
                try self.builder.logger.printLnIndented("#{d} ModuleWip.resolveGlobalType idle", .{key});
                const scope: Scope = .{ .module = self };
                // const resolved = try scope.resolveType(key);
                global.state = .resolving;
                const type_inst = self.builder.getHirInst(self.hir_inst);
                _ = type_inst; // autofix

                // const resolved = switch (type_inst) {

                var fn_wip = &global.wip.fn_decl;
                const fn_inst = self.builder.getHirInst(global.wip.fn_decl.hir_inst).fn_decl;
                // const return_type = try TypeWip.resolveUnreferenced(self.builder, scope, global.wip.fn_decl.return_type);

                var iter_params = self.builder.iterHirList(fn_inst.params_list);

                while (iter_params.next()) |param_index| {
                    self.builder.logger.open("#{d} resolve param", .{param_index});
                    defer self.builder.logger.close();

                    const param_inst = self.builder.getHirInst(param_index).param_decl;
                    const param_type = try TypeWip.resolveUnreferenced(self.builder, scope, param_inst.ty);
                    const name = try self.builder.internNodeSlice(param_inst.name_node);

                    const index = try self.builder.pushType(.{ .param = .{
                        .type = param_type,
                        .name = name,
                    } });
                    try fn_wip.params.put(self.builder.arena.allocator(), param_index, index);
                }
                // return try fn_wip.commitType();

                // fn_wip.type_index;
                // self.builder.setType(global.type_wip.index, .{
                //     .global = .{
                //         .name = global.name,
                //         .type = try fn_wip.commitType(),
                //         .init = null,
                //     },
                // });
                const index = try fn_wip.commitType();
                global.state = .resolved_type;

                // try self.globals.put(self.builder.arena.allocator(), key, global) catch unreachable;
                break :blk index;
            },
            .resolved_type, .resolving => return error.CircularDependency,
            .resolved => global.wip.fn_decl.type_index,
        };
        // try self.builder.logger.printLnIndented("#{d} ModuleWip.resolveGlobalType resolved: {d}", .{ symbol.hir_inst, resolved_type });
        return resolved_type;
    }
};

const TypeWip = struct {
    builder: *Self,
    hir_inst: Hir.Inst.Index,
    scope: Scope,
    index: Mir.Type.Index,
    // state: State = .idle,
    pub fn init(builder: *Self, scope: Scope, hir_inst: Hir.Inst.Index) Error!TypeWip {
        return .{
            .builder = builder,
            .hir_inst = hir_inst,
            .scope = scope,
            .index = try builder.reserveTypeIndex(),
        };
    }
    pub fn getType(self: *TypeWip) Mir.Type {
        return self.builder.getType(self.index).?;
    }
    pub fn resolveUnreferenced(builder: *Self, scope: Scope, hir_inst: Hir.Inst.Index) Error!Mir.Type.Index {
        var wip = try TypeWip.init(builder, scope, hir_inst);
        return try wip.resolve();
    }
    pub fn resolve(self: *TypeWip) Error!Mir.Type.Index {
        self.builder.logger.open("#{d} TypeWip.resolve", .{self.hir_inst});
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
        self.builder.logger.log(
            "#{d} TypeWip.resolve resolved to: '{s}'",
            .{ self.hir_inst, if (self.builder.getType(resolved)) |res| @tagName(res) else @tagName(resolved) },
            tw.green_400,
        );
        return resolved;
    }
    pub fn resolveModule(self: *TypeWip) Error!Mir.Type.Index {
        self.builder.logger.open("#{d} TypeWip.resolveModule", .{self.hir_inst});
        defer self.builder.logger.close();

        var module = try ModuleWip.init(self.builder, .{ .root = .{ .builder = self.builder } }, self.hir_inst);

        try module.collectSymbols();
        var globals_iter = module.globals.iterator();
        var decls = self.builder.newList();
        while (globals_iter.next()) |entry| {
            switch (entry.value_ptr.state) {
                .idle => {
                    const index = try module.resolveGlobalType(entry.key_ptr.*);
                    try decls.append(index.asInt());
                    // @panic("stop");
                },
                .resolved_type, .resolved => {
                    try decls.append(entry.value_ptr.wip.fn_decl.type_index.asInt());
                },
                else => unreachable,
            }
        }
        self.builder.setType(module.index, .{ .module = .{
            .decls = try decls.commit(),
        } });
        globals_iter.reset();
        while (globals_iter.next()) |entry| {
            switch (entry.value_ptr.state) {
                .resolving, .idle => unreachable,
                .resolved_type => {
                    @panic("unimplemented");
                    // const global_type = entry.value_ptr.wip.fn_decl.type_index.getType();
                    // const resolved_type = self.builder.getType(global_type.global.type) orelse {
                    //     entry.value_ptr.state = .resolved;
                    //     continue;
                    // };
                    // switch (resolved_type) {
                    //     .@"fn" => {
                    //         // try entry.value_ptr.wip.fn_decl.type_index.resolveInit();
                    //     },
                    //     else => std.debug.panic("unimplemented resolved_type: {s}", .{@tagName(resolved_type)}),
                    // }

                    // entry.value_ptr.state = .resolved;
                },
                .resolved => {},
            }
        }
        return module.index;
    }
    pub fn resolveInit(self: *TypeWip) Error!void {
        self.builder.logger.open("#{d} TypeWip.resolveInit", .{self.hir_inst});
        defer self.builder.logger.close();

        const fn_inst_index = self.builder.getHirInst(self.hir_inst).global_decl.init;

        const fn_inst = self.builder.getHirInst(fn_inst_index);
        const init_inst = fn_inst.fn_decl.init orelse return;
        _ = init_inst; // autofix

        // var block_wip = BlockWip.init(self.builder, .{ .@"fn" = self }, init_inst);
        // try block_wip.resolve();
        // self.builder.setType(self.index, .{ .@"fn" = .{
        //     .body = try block_wip.commit(),
        // } });

        // var iter_params = self.builder.iterHirList(fn_inst.params);

        // const global_type = self.getType();
        // const resolved_type = self.builder.getType(global_type.global.type) orelse unreachable;

        // var param_types_iter = self.builder.mir.lists.iterList(resolved_type.@"fn".params);
        // while (iter_params.next()) |param_index| {
        //     const param_type = param_types_iter.next() orelse unreachable;
        //     const param_type_index = Mir.Type.Index.fromInt(param_type);
        //     const param_type_type = self.builder.getType(param_type_index) orelse unreachable;
        //     try block_wip.pushInstruction(param_index, .{
        //         .op = .param,
        //         .type = param_type_index,
        //         .value = .runtime,
        //         .data = .{
        //             .scoped = .{
        //                 .name = param_type_type.param.name,
        //                 .index = 0,
        //             },
        //         },
        //     });
        // }

        // if (init_inst) |init_hir_inst| {

        // var body_wip: ?*BlockWip = null;
        // _ = body_wip; // autofix

        // if (fn_inst.init) |init_inst| {
        //     fn_wip.body = BlockWip.init(self.builder, .{ .@"fn" = &fn_wip }, init_inst);
        //     fn_wip.body.?.return_type = return_type;
        //     body_wip = &fn_wip.body.?;
        // }
        // var iter_params = self.builder.iterHirList(fn_inst.params);

        // while (iter_params.next()) |param_index| {
        //     self.builder.logger.open("#{d} resolve param", .{param_index});
        //     defer self.builder.logger.close();

        //     const param_inst = self.builder.getHirInst(param_index).param_decl;
        //     const param_type = try self.scope.resolveType(param_inst.ty);
        //     const name = try self.builder.internNodeSlice(param_inst.name_node);

        //     const index = try self.builder.pushType(.{ .param = .{
        //         .type = param_type,
        //         .name = name,
        //     } });
        //     try fn_wip.params.put(self.builder.arena.allocator(), param_index, index);
        //     if (body_wip) |body| {
        //         try body.pushInstruction(param_index, .{
        //             .op = .param,
        //             .type = param_type,
        //             .value = .runtime,
        //             .data = .{ .scoped = .{
        //                 .name = name,
        //                 .index = 0,
        //             } },
        //         });
        //     }
        // }
        // if (body_wip) |body| {
        //     try body.resolve();
        // }
        // }

        // @panic("stop");
    }
    pub fn resolveDecl(self: *TypeWip) Error!Mir.Type.Index {
        self.builder.logger.open("#{d} TypeWip.resolveDecl", .{self.hir_inst});
        defer self.builder.logger.close();
        const decl_inst = self.builder.getHirInst(self.hir_inst);
        const type_index = decl_inst.global_decl.type.?;
        // decl_inst.global_decl.
        // return try self.scope.resolveType(type_index);
        // return try self.builder.getType(type_index);
        return try TypeWip.resolveUnreferenced(self.builder, self.scope, type_index);
    }
    pub fn resolveFnDecl(self: *TypeWip) Error!Mir.Type.Index {
        self.builder.logger.open("#{d} TypeWip.resolveFnDecl", .{self.hir_inst});
        defer self.builder.logger.close();

        const fn_inst = self.builder.getHirInst(self.hir_inst).fn_decl;

        self.builder.logger.open("#{d} resolve return type", .{fn_inst.return_type});
        // const return_type = try self.scope.resolveType(fn_inst.return_type);
        self.builder.logger.close();

        const return_type = try TypeWip.resolveUnreferenced(self.builder, self.scope, fn_inst.return_type);
        const fn_name = try self.builder.internNodeSlice(fn_inst.name_node);
        var fn_wip = try FnWip.init(
            self.builder,
            self.scope,
            self.hir_inst,
            fn_name,
            return_type,
        );

        var iter_params = self.builder.iterHirList(fn_inst.params_list);

        while (iter_params.next()) |param_index| {
            self.builder.logger.open("#{d} resolve param", .{param_index});
            defer self.builder.logger.close();

            const param_inst = self.builder.getHirInst(param_index).param_decl;
            const param_type = try TypeWip.resolveUnreferenced(self.builder, self.scope, param_inst.ty);
            const name = try self.builder.internNodeSlice(param_inst.name_node);

            const index = try self.builder.pushType(.{ .param = .{
                .type = param_type,
                .name = name,
            } });
            try fn_wip.params.put(self.builder.arena.allocator(), param_index, index);
        }
        return try fn_wip.commitType();
    }
    pub fn resolveBlockDecl(self: *TypeWip) Error!Mir.Type.Index {
        self.builder.logger.open("#{d} TypeWip.resolveBlockDecl", .{self.hir_inst});
        defer self.builder.logger.close();
        var block = BlockWip.init(self.builder, self.scope, self.hir_inst);
        block.return_type = self.scope.block.return_type;
        try block.resolve();
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
    pub var i: usize = 0;
    pub fn getBuilder(self: Scope) *Self {
        return switch (self) {
            .module => self.module.builder,
            .root => self.root.builder,
            .@"fn" => self.@"fn".builder,
            .block => self.block.builder,
        };
    }
    // pub fn resolveType(self: Scope, inst_index: Hir.Inst.Index) !Mir.Type.Index {
    //     var builder = self.getBuilder();
    //     builder.logger.open("#{d} Scope.resolveType", .{inst_index});
    //     defer builder.logger.close();

    //     var type_wip = TypeWip.init(builder, self, inst_index);
    //     return try type_wip.resolve();
    // }
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
    //                     const index = module_wip.resolveGlobalType(name) catch |err| switch (err) {
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
    //     builder.logger.open("Scope.findSymbolTypeRecursive .{s} {}", .{ @tagName(self), key });
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
    // pub fn findSymbol(self: Scope, key: Symbol.Key) !?Symbol {
    //     try self.getBuilder().logger.printLnIndented("Scope.findSymbol .{s} #{d}", .{ @tagName(self), key });
    //     return switch (self) {
    //         .@"fn" => |fn_wip| try fn_wip.findSymbol(key),
    //         .block => |block_wip| try block_wip.findSymbol(key),
    //         .module => |module_wip| try module_wip.findSymbol(key),
    //         .root => {
    //             try self.getBuilder().logger.log("Could not find symbol #{d}", .{key}, tw.red_400);
    //             return null;
    //         },
    //     };
    // }
    // pub fn findSymbolRecursive(self: Scope, key: Symbol.Key) !?Symbol {
    //     const hir_inst = self.getBuilder().getHirInst(key);
    //     try self.getBuilder().logger.log("Scope.findSymbolRecursive scope({s}) hir({d}:{s}) ", .{ @tagName(self), key, @tagName(hir_inst) }, tw.cyan_400);

    //     if (try self.findSymbol(key)) |symbol| return symbol;
    //     if (self.getParent()) |parent| return try parent.findSymbolRecursive(key);
    //     return null;
    // }
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
    pub fn getGlobal(self: *Scope, key: u32) !Mir.Type.Index {
        self.getBuilder().logger.log("S({d}.{s}) Scope.getGlobal #{d}", .{ self.getId(), @tagName(self.*), key }, tw.green_400);
        // TODO: resolve recursively once we have multiple modules in nested scopes
        switch (self.*) {
            .module => |module| return try module.resolveGlobalType(key),
            else => return if (self.getParent()) |parent| try @constCast(&parent).getGlobal(key) else return error.GlobalNotFound,
        }
    }
    pub fn getId(self: *Scope) usize {
        return switch (self.*) {
            .@"fn" => |fn_wip| fn_wip.id,
            .block => |block_wip| block_wip.id,
            .module => |module_wip| module_wip.id,
            .root => 0,
        };
    }
    pub fn getParam(self: *Scope, key: u32) !Mir.Type.Index {
        self.getBuilder().logger.log("S({d}.{s}) Scope.getParam #{d}", .{ self.getId(), @tagName(self.*), key }, tw.pink_400);
        switch (self.*) {
            .@"fn" => |fn_wip| {
                return fn_wip.params.get(key) orelse {
                    self.getBuilder().logger.log("S({d}.{s}) Scope.getParam #{d} [NOT FOUND]", .{ self.getId(), @tagName(self.*), key }, tw.red_400);
                    fn_wip.dump_params();
                    return error.ParamNotFound;
                };
            },
            .block => |block_wip| return try block_wip.parent.getParam(key),
            else => unreachable,
        }
    }
    pub fn dump(self: *Scope) Error!void {
        var logger = Logger.init(std.io.getStdErr().writer().any(), "MirBuilder");
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

// const Symbol = union(enum) {
//     local: struct {
//         type: Mir.Type.Index,
//         name: InternedSlice,
//         index: u32,
//     },
//     instruction: Mir.Instruction.Index,
//     global: Global,
//     pub const Global = union(enum) {
//         idle: struct {
//             name: InternedSlice,
//             inst: Hir.Inst.Index,
//         },
//         resolving: void,
//         resolved: struct {
//             scope_index: Mir.Type.Index,
//             name: InternedSlice,
//             type: Mir.Type.Index,
//         },
//     };
//     pub const Key = Hir.Inst.Index;
//     pub const Map = std.AutoHashMap(Key, Symbol);
// };
pub const FnWip = struct {
    id: usize,
    name: InternedSlice,
    parent: Scope,
    builder: *Self,
    hir_inst: Hir.Inst.Index,
    type_index: Mir.Type.Index,
    return_type: Mir.Type.Index,
    // body: ?BlockWip = null,
    params: std.AutoArrayHashMapUnmanaged(Hir.Inst.Index, Mir.Type.Index) = .{},

    pub fn init(builder: *Self, parent: Scope, hir_inst: Hir.Inst.Index, name: InternedSlice, return_type: Mir.Type.Index) !FnWip {
        Scope.i += 1;
        return .{
            .id = Scope.i,
            .parent = parent,
            .builder = builder,
            .hir_inst = hir_inst,
            .type_index = try builder.reserveTypeIndex(),
            .name = name,
            .return_type = return_type,
        };
    }

    // pub fn findSymbol(self: *FnWip, key: Symbol.Key) !?Symbol {
    //     return self.symbols_map.get(key);
    // }
    pub fn pushParameter(self: *FnWip, name: InternedSlice, ty: Mir.Type.Index, hir_inst: Hir.Inst.Index) !void {
        const index = try self.builder.pushType(.{ .param = .{
            .type = ty,
            .name = name,
        } });
        const name_slice = self.builder.mir.strings.getSlice(name);
        self.builder.logger.log("#{d} FnWip.pushParameter [PUSHED #{d}] {s} {s}", .{ self.hir_inst, hir_inst, index.fmt(self.builder.mir), name_slice }, tw.pink_400);
        try self.params.put(self.builder.arena.allocator(), hir_inst, index);
        self.dump_params();
    }
    pub inline fn resolveBody(self: *FnWip) Error!void {
        self.builder.logger.open("#{d} FnWip.resolveBody", .{self.hir_inst});
        defer self.builder.logger.close();

        const body_inst = self.builder.getHirInst(self.hir_inst).fn_decl.init;
        if (body_inst) |body_inst_index| {
            var block = BlockWip.init(self.builder, .{ .@"fn" = self }, body_inst_index);
            // var iter = self.params.iterator();
            // while (iter.next()) |entry| {
            //     try block.pushInstruction(entry.key_ptr.*, .{
            //         .op = .param,
            //         .type = entry.value_ptr.*,
            //         .value = .runtime,
            //         .data = .{ .type = entry.value_ptr.* },
            //     });
            // }
            block.return_type = self.return_type;
            try block.resolve();
            self.body = block;
        } else {
            self.body = null;
            try self.builder.logger.printLnIndented("#{d} FnWip.resolveBody [NO BODY]", .{self.hir_inst});
        }
    }
    pub fn dump_params(self: *FnWip) void {
        var iter = self.params.iterator();
        while (iter.next()) |entry| {
            self.builder.logger.log("#{d}: {s}", .{ entry.key_ptr.*, entry.value_ptr.*.fmt(self.builder.mir) }, tw.pink_400);
        }
    }
    pub fn commitType(self: *FnWip) Error!Mir.Type.Index {
        var params: Mir.ChildList = self.builder.newList();
        for (self.params.values()) |ty| {
            try params.append(ty.asInt());
        }
        self.builder.setType(self.type_index, .{
            .@"fn" = .{
                .name = self.name,
                .params = try params.commit(),
                .return_type = self.return_type,
                // .body = if (self.body) |*body| try body.commit() else null,
                .body = null,
            },
        });
        return self.type_index;
    }
    pub fn commit(self: *FnWip) Error!void {
        _ = self; // autofix
        // _ = try self.commitType();
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
    id: usize,
    parent: Scope,
    builder: *Self,
    hir_inst: Hir.Inst.Index,
    instructions: std.AutoArrayHashMapUnmanaged(u32, InstructionEntry) = .{},
    return_type: Mir.Type.Index = .void,
    children_scopes: std.AutoArrayHashMapUnmanaged(u32, BlockWip) = .{},

    const InstructionEntry = struct { u32, Mir.Instruction };

    pub fn init(builder: *Self, parent: Scope, hir_inst: Hir.Inst.Index) BlockWip {
        Scope.i += 1;
        builder.logger.log("S({d}) BlockWip.init", .{Scope.i}, tw.cyan_400);
        return .{
            .id = Scope.i,
            .parent = parent,
            .builder = builder,
            .hir_inst = hir_inst,
            // .instructions_list = .{},
        };
    }
    pub inline fn makeChild(self: *BlockWip, id: u32) !*BlockWip {
        try self.children_scopes.put(self.builder.arena.allocator(), id, BlockWip.init(
            self.builder,
            .{ .block = self },
            id,
        ));
        return self.children_scopes.getPtr(id).?;
    }
    pub fn dump(self: *BlockWip) void {
        self.builder.logger.open("#{d} BlockWip.dump", .{self.hir_inst});
        defer self.builder.logger.close();
        var iter = self.instructions.iterator();

        while (iter.next()) |inst| {
            self.builder.logger.log("#{d}: {s}", .{ inst.key_ptr.*, @tagName(inst.value_ptr[1].op) }, tw.yellow_400);
        }
    }
    pub fn deadCodeElimination(self: *BlockWip) Error!void {
        var logger = self.builder.logger;
        logger.open("#{d} BlockWip.deadCodeElimination", .{self.hir_inst});
        defer logger.close();
        const allocator = self.builder.arena.allocator();
        var instructions = std.AutoArrayHashMap(u32, *Mir.Instruction).init(allocator);
        var worklist = std.AutoArrayHashMap(u32, void).init(allocator);
        // var locals_cfg = std.AutoArrayHashMap(u32, std.ArrayList(u32)).init(allocator);
        // logger.log("scope count: {d}", .{self.children_scopes.count() + 1}, tw.emerald_400);

        var scopes = std.ArrayList(*BlockWip).init(allocator);
        try scopes.append(self);
        while (scopes.popOrNull()) |scope| {
            var iter = scope.instructions.iterator();
            while (iter.next()) |entry| {
                const id = entry.key_ptr.*;
                const inst = &entry.value_ptr[1];
                try instructions.put(id, inst);
                // logger.log("#{d}: !{d} {s}", .{ id, inst.liveness, @tagName(inst.op) }, tw.emerald_300);
                if (inst.liveness > 0) {
                    try worklist.put(id, {});
                }
            }
            for (scope.children_scopes.values()) |*child| {
                try scopes.append(child);
            }
        }
        {
            // Dump
            logger.log("instructions size: {d}", .{instructions.count()}, tw.emerald_400);
            var instructions_iter = instructions.iterator();
            while (instructions_iter.next()) |entry| {
                const id = entry.key_ptr.*;
                const inst = entry.value_ptr.*;
                logger.log("#{d}: !{d} {s}", .{ id, inst.liveness, @tagName(inst.op) }, tw.emerald_300);
            }
        }

        while (worklist.popOrNull()) |kv| {
            const inst = instructions.get(kv.key) orelse self.builder.logger.panic("BlockWip.deadCodeElimination [UNRESOLVED INSTRUCTION] (#{d})", .{kv.key});
            logger.log("checking #{d}: !{d} {s} data .{s}", .{ kv.key, inst.liveness, @tagName(inst.op), @tagName(inst.data) }, tw.emerald_400);
            switch (inst.data) {
                .instruction => |instruction| {
                    try self.increaseLiveness(&worklist, &instructions, instruction);
                    // try worklist.put(instruction, {});
                },
                .binOp => |bin_op| {
                    try self.increaseLiveness(&worklist, &instructions, bin_op.lhs);
                    try self.increaseLiveness(&worklist, &instructions, bin_op.rhs);
                    // try worklist.put(bin_op.lhs, {});
                    // try worklist.put(bin_op.rhs, {});
                },
                .if_expr => |if_expr| {
                    try self.increaseLiveness(&worklist, &instructions, if_expr.cond);
                    // try worklist.put(if_expr.cond, {});
                },

                // .param_get => {
                //     inst.liveness -= 1;
                // },
                else => {},
            }
            switch (inst.op) {
                .local, .param => {
                    logger.log("local found #{d}: {s}", .{
                        kv.key,
                        @tagName(inst.op),
                    }, tw.emerald_300);
                    var instructions_iter = instructions.iterator();
                    logger.log("instructions size: {d}", .{instructions.count()}, tw.emerald_400);
                    while (instructions_iter.next()) |entry| {
                        logger.log("checking for local #{d}", .{entry.key_ptr.*}, tw.pink_400);
                        const inst_ptr_ = entry.value_ptr.*;
                        const id_ = entry.key_ptr.*;
                        switch (inst_ptr_.op) {
                            .local_set, .param_set, .as => {
                                if (inst_ptr_.data.binOp.lhs == kv.key or inst_ptr_.data.binOp.rhs == kv.key) {
                                    const to_add = id_;
                                    logger.log("local_set found #{d}: !{d} {s}", .{ to_add, inst_ptr_.liveness, @tagName(inst_ptr_.op) }, tw.emerald_300);
                                    logger.log("adding #{d} to worklist", .{to_add}, tw.emerald_300);
                                    try self.increaseLiveness(&worklist, &instructions, to_add);
                                    // try worklist.put(to_add, {});
                                }
                                // try worklist.put(inst.data.instruction, {});
                            },
                            else => {},
                        }
                    }
                },
                else => {},
            }
        }
    }

    pub fn isOperandLocal(inst_index: u32, instructions_map: *std.AutoArrayHashMap(u32, *Mir.Instruction)) bool {
        const inst = instructions_map.get(inst_index) orelse return false;
        return switch (inst.op) {
            .param, .local => true,
            else => false,
        };
    }
    pub inline fn increaseLiveness(self: *BlockWip, worklist: *std.AutoArrayHashMap(u32, void), instructions_map: *std.AutoArrayHashMap(u32, *Mir.Instruction), id: u32) !void {
        self.builder.logger.log("increasing liveness of #{d}", .{id}, tw.orange_400);
        const inst = instructions_map.get(id) orelse unreachable;
        if (inst.liveness == 0) {
            try worklist.put(id, {});
        }

        inst.liveness += 1;
    }

    pub fn remapInstructions(self: *BlockWip, instructions_remap: *std.AutoArrayHashMapUnmanaged(u32, u32)) Error!Mir.Type.Index {
        self.builder.logger.open("#{d} BlockWip.remapInstructions, total: {d}", .{ self.hir_inst, self.instructions.count() });
        defer self.builder.logger.close();
        const allocator = self.builder.arena.allocator();
        var iter = self.instructions.iterator();
        var instructions = self.builder.newList();

        while (iter.next()) |entry| {
            const current_id = entry.key_ptr.*;
            const new_id = try self.builder.reserveInstructionIndex();
            try instructions.append(new_id);
            try instructions_remap.put(allocator, current_id, new_id);

            var inst: Mir.Instruction = entry.value_ptr[1];

            self.builder.logger.log("commiting {s} #{d} to #{d}", .{ @tagName(inst.data), current_id, new_id }, tw.yellow_400);

            switch (inst.data) {
                .instruction => |instruction| {
                    inst.data.instruction = instructions_remap.get(instruction) orelse self.builder.logger.panic("BlockWip.commit [UNRESOLVED INSTRUCTION] (#{d})", .{instruction});
                },
                .if_expr => |if_expr| {
                    inst.data.if_expr.cond = instructions_remap.get(if_expr.cond) orelse self.builder.logger.panic("BlockWip.commit [UNRESOLVED INSTRUCTION] (#{d})", .{if_expr.cond});

                    const if_inst = self.builder.getHirInst(current_id);
                    var then_scope = self.children_scopes.getPtr(if_inst.if_expr.then_body) orelse self.builder.logger.panic("BlockWip.commit [UNRESOLVED THEN BODY] (#{d})", .{if_inst.if_expr.then_body});
                    inst.data.if_expr.then_body = try then_scope.remapInstructions(instructions_remap);

                    if (if_inst.if_expr.else_body) |else_body| {
                        var else_scope = self.children_scopes.getPtr(else_body) orelse self.builder.logger.panic("BlockWip.commit [UNRESOLVED ELSE BODY] (#{d})", .{else_body});
                        inst.data.if_expr.else_body = try else_scope.remapInstructions(instructions_remap);
                    }
                },
                .type => {
                    if (inst.op == .loop) {
                        const loop_inst = self.builder.getHirInst(current_id);

                        var loop_scope = self.children_scopes.getPtr(loop_inst.loop.body) orelse self.builder.logger.panic("BlockWip.commit [UNRESOLVED LOOP BODY] (#{d})", .{loop_inst.loop.body});
                        inst.data.type = try loop_scope.remapInstructions(instructions_remap);
                    }
                },
                .binOp => |bin_op| {
                    inst.data.binOp.lhs = instructions_remap.get(bin_op.lhs) orelse self.builder.logger.panic("BlockWip.commit [UNRESOLVED INSTRUCTION] (#{d})", .{bin_op.lhs});
                    inst.data.binOp.rhs = instructions_remap.get(bin_op.rhs) orelse self.builder.logger.panic("BlockWip.commit [UNRESOLVED INSTRUCTION] (#{d})", .{bin_op.rhs});
                },
                else => {},
            }
            // self.builder.logger.log("remapped #{d} = {}", .{ new_id, inst }, tw.violet_400);
            self.builder.setInstruction(new_id, inst);
        }

        return try self.builder.pushType(.{
            .block = .{
                // .locals = try self.locals.commit(),
                .name = null, // TODO
                .instructions = try instructions.commit(),
            },
        });
    }

    const RemapContext = struct {
        instructions_remap: std.AutoArrayHashMap(u32, u32),
        locals_count: u32,
        // root: *BlockWip,
        pub fn init(allocator: std.mem.Allocator) RemapContext {
            return .{
                .instructions_remap = std.AutoArrayHashMap(u32, u32).init(allocator),
                .locals_count = 0,
            };
        }
        pub fn deinit(self: *RemapContext) void {
            self.instructions_remap.deinit();
        }

        pub fn remap(self: *RemapContext, scope: *BlockWip) Error!Mir.Type.Index {
            scope.builder.logger.open("#{d} BlockWip.remapInstructions, total: {d}", .{ scope.hir_inst, scope.instructions.count() });
            defer scope.builder.logger.close();

            var iter = scope.instructions.iterator();
            var instructions = scope.builder.newList();

            while (iter.next()) |entry| {
                const current_id = entry.key_ptr.*;
                const new_id = try scope.builder.reserveInstructionIndex();
                try instructions.append(new_id);
                try self.instructions_remap.put(current_id, new_id);

                var inst: Mir.Instruction = entry.value_ptr[1];

                scope.builder.logger.log("commiting {s} #{d} to #{d}", .{ @tagName(inst.data), current_id, new_id }, tw.yellow_400);

                switch (inst.data) {
                    .instruction => |instruction| {
                        inst.data.instruction = self.instructions_remap.get(instruction) orelse scope.builder.logger.panic("BlockWip.commit [UNRESOLVED INSTRUCTION] (#{d})", .{instruction});
                    },
                    .if_expr => |if_expr| {
                        inst.data.if_expr.cond = self.instructions_remap.get(if_expr.cond) orelse scope.builder.logger.panic("BlockWip.commit [UNRESOLVED INSTRUCTION] (#{d})", .{if_expr.cond});

                        const if_inst = scope.builder.getHirInst(current_id);
                        const then_scope = scope.children_scopes.getPtr(if_inst.if_expr.then_body) orelse scope.builder.logger.panic("BlockWip.commit [UNRESOLVED THEN BODY] (#{d})", .{if_inst.if_expr.then_body});
                        inst.data.if_expr.then_body = try self.remap(then_scope);

                        if (if_inst.if_expr.else_body) |else_body| {
                            const else_scope = scope.children_scopes.getPtr(else_body) orelse scope.builder.logger.panic("BlockWip.commit [UNRESOLVED ELSE BODY] (#{d})", .{else_body});
                            inst.data.if_expr.else_body = try self.remap(else_scope);
                        }
                    },
                    .type => {
                        if (inst.op == .loop) {
                            const loop_inst = scope.builder.getHirInst(current_id);

                            const loop_scope = scope.children_scopes.getPtr(loop_inst.loop.body) orelse scope.builder.logger.panic("BlockWip.commit [UNRESOLVED LOOP BODY] (#{d})", .{loop_inst.loop.body});
                            inst.data.type = try self.remap(loop_scope);
                        }
                    },
                    .binOp => |bin_op| {
                        inst.data.binOp.lhs = self.instructions_remap.get(bin_op.lhs) orelse scope.builder.logger.panic("BlockWip.commit [UNRESOLVED INSTRUCTION] (#{d})", .{bin_op.lhs});
                        inst.data.binOp.rhs = self.instructions_remap.get(bin_op.rhs) orelse scope.builder.logger.panic("BlockWip.commit [UNRESOLVED INSTRUCTION] (#{d})", .{bin_op.rhs});
                    },
                    else => {},
                }
                switch (inst.op) {
                    .local, .param => {
                        inst.data.scoped.index = self.locals_count;
                        self.locals_count += 1;
                    },
                    else => {},
                }
                // self.builder.logger.log("remapped #{d} = {}", .{ new_id, inst }, tw.violet_400);
                scope.builder.setInstruction(new_id, inst);
            }
            return try scope.builder.pushType(.{
                .block = .{
                    .locals = self.locals_count,
                    // .locals = try self.locals.commit(),
                    .name = null, // TODO
                    .instructions = try instructions.commit(),
                },
            });
        }
    };
    pub fn commit(self: *BlockWip) Error!Mir.Type.Index {
        self.builder.logger.open("#{d} BlockWip.commit", .{self.hir_inst});
        defer self.builder.logger.close();
        // try self.deadCodeElimination();

        var remap_ctx = RemapContext.init(self.builder.arena.allocator());
        defer remap_ctx.deinit();
        return try remap_ctx.remap(self);

        // var instructions_remap: std.AutoArrayHashMapUnmanaged(u32, u32) = .{};
        // return try self.remapInstructions(&instructions_remap);
    }
    pub fn resolve(self: *BlockWip) Error!void {
        self.builder.logger.open("#{d} BlockWip.resolve", .{self.hir_inst});
        defer self.builder.logger.close();
        const inst = self.builder.getHirInst(self.hir_inst);
        switch (inst) {
            .block,
            .inline_block,
            => |block_inst| {
                var iter = self.builder.iterHirList(block_inst.instructions_list);

                while (iter.next()) |inst_index| {
                    // _ = try self.resolveSingleInstruction(inst_index);
                    try self.resolveInstruction(inst_index);
                }
            },
            else => unreachable,
        }
    }
    pub fn resolveInstruction(self: *BlockWip, id: u32) Error!void {
        const hir_inst = self.builder.getHirInst(id);
        self.builder.logger.open("BlockWip.resolveInstruction #{d}: {s}", .{ id, @tagName(hir_inst) });
        defer self.builder.logger.close();

        var scope: Scope = .{ .block = self };
        switch (hir_inst) {
            .comptime_number => {
                const value_index = try self.builder.resolveValue(id);

                try self.pushInstruction(id, .{
                    .value = value_index,

                    .op = .constant,
                    .type = .number,
                    .data = .{
                        .value = value_index,
                    },
                });
                // });
            },
            .global_get => |global_get| {
                const global = try scope.getGlobal(global_get.operand);
                const global_type: Mir.Type = self.builder.getType(global) orelse self.builder.logger.panic("BlockWip.resolveInstruction [UNRESOLVED GLOBAL TYPE] (#{})", .{global});
                try self.pushInstruction(id, .{
                    .value = .runtime, // TODO: resolve comptime globals
                    .op = .global_get,
                    .type = global_type.global.type,
                    .data = .{ .type = global },
                });
            },
            inline .ty_f64, .ty_f32, .ty_i64, .ty_i32, .ty_boolean => {
                const tag = std.meta.activeTag(hir_inst);
                try self.pushInstruction(id, .{
                    .value = switch (tag) {
                        .ty_f64 => .type_f64,
                        .ty_f32 => .type_f32,
                        .ty_i64 => .type_i64,
                        .ty_i32 => .type_i32,
                        .ty_boolean => .type_boolean,
                        else => unreachable,
                    },
                    .op = .type,
                    .type = .type,
                    .data = .{ .type = switch (tag) {
                        .ty_f64 => .f64,
                        .ty_f32 => .f32,
                        .ty_i64 => .i64,
                        .ty_i32 => .i32,
                        .ty_boolean => .boolean,
                        else => unreachable,
                    } },
                });
            },
            .as => |as| {
                const lhs_inst = try self.getInstruction(as.lhs);
                _ = lhs_inst; // autofix
                const rhs_inst = try self.getInstruction(as.rhs);
                // const target_inst = try self.getInstruction(as.operand);
                try self.pushInstruction(id, .{
                    .value = .runtime, // TODO: resolve comptime as
                    .op = .as,
                    .type = rhs_inst.type,
                    .data = .{ .binOp = .{
                        .lhs = as.lhs,
                        .rhs = as.rhs,
                    } },
                });
            },
            .typeof => |typeof| {
                const target_inst = try self.getInstruction(typeof.operand);
                try self.pushInstruction(id, .{
                    .value = switch (target_inst.type) {
                        .f64 => .type_f64,
                        .f32 => .type_f32,
                        .i64 => .type_i64,
                        .i32 => .type_i32,
                        .number => .type_number,
                        .boolean => .type_boolean,
                        else => unreachable,
                    },
                    .op = .type,
                    .type = .type,
                    .data = .{ .type = target_inst.type },
                });
            },
            .local => |local| {
                // const type_instruction = try self.getInstruction(local.type);
                // if (type_instruction.op != .type) {
                //     self.builder.logger.panic("BlockWip.resolveInstruction [UNRESOLVED TYPE] (#{d})", .{local.type});
                // }

                try self.pushInstruction(id, .{
                    .value = .runtime, // TODO: resolve comptime locals
                    .op = .local,
                    .type = .unknown,
                    .data = .{ .scoped = .{
                        .name = try self.builder.internNodeSlice(local.name_node),
                        .index = 0,
                    } },
                });
                // try self.locals.put(self.builder.arena.allocator(), id, local.name_node);
            },
            .local_set => |local_set| {
                const local_inst = try self.getInstruction(local_set.lhs);
                const value_inst = try self.getInstruction(local_set.rhs);

                try self.pushInstruction(id, .{
                    .value = .runtime, // TODO: resolve comptime locals
                    .op = .local_set,
                    .type = if (local_inst.type == .unknown) value_inst.type else local_inst.type,
                    .data = .{ .binOp = .{
                        .lhs = local_set.lhs,
                        .rhs = local_set.rhs,
                    } },
                });
            },
            .loop => |loop| {
                var body_scope = try self.makeChild(loop.body);
                body_scope.return_type = self.return_type;
                try body_scope.resolve();

                try self.pushInstruction(id, .{
                    .value = .runtime, // TODO: resolve comptime loops
                    .op = .loop,
                    .type = .void, // TODO: resolve typed blocks in the future
                    .data = .{ .type = .unknown },
                });
            },
            .param_get => |param_get| {
                // const param_index = try self.parent.getParam(param_get.operand);
                const param_inst = try self.getInstruction(param_get.operand);
                // const param_index = param_inst.data.type;
                // const param: Mir.Type = self.builder.getType(param_index) orelse self.builder.logger.panic("BlockWip.resolveInstruction [UNRESOLVED PARAM at #{d}] ({any})", .{
                //     param_get.operand,
                //     param_index,
                // });

                try self.pushInstruction(id, .{
                    .value = .runtime, // TODO: resolve comptime params
                    .op = .param_get,
                    .type = param_inst.type,
                    .data = .{ .instruction = param_get.operand },
                });
            },
            .local_get => |local_get| {
                const local_index = try self.getInstruction(local_get.operand);

                try self.pushInstruction(id, .{
                    .value = .runtime, // TODO: resolve comptime params
                    .op = .local_get,
                    .type = local_index.type,
                    .data = .{ .instruction = local_get.operand },
                });
            },
            inline .add, .sub, .mul, .div, .gt, .lt, .eq, .ne => |bin_op| {
                const op: Mir.Instruction.Op = switch (std.meta.activeTag(hir_inst)) {
                    inline .add,
                    .sub,
                    .mul,
                    .div,
                    .gt,
                    .lt,
                    .eq,
                    .ne,
                    => |tag| Mir.Instruction.Op.fromString(@tagName(tag)),
                    else => unreachable,
                };
                const lhs_inst = try self.getInstruction(bin_op.lhs);
                _ = lhs_inst; // autofix
                const rhs_inst = try self.getInstruction(bin_op.rhs);

                try self.pushInstruction(id, .{
                    .value = .runtime, // TODO: resolve comptime binops
                    .op = op,
                    .type = rhs_inst.type,
                    .data = .{ .binOp = .{
                        .lhs = bin_op.lhs,
                        .rhs = bin_op.rhs,
                    } },
                });
            },
            .if_expr => |if_expr| {
                const cond_inst = try self.getInstruction(if_expr.cond);
                _ = cond_inst; // autofix
                var then_scope = try self.makeChild(if_expr.then_body);
                // var then_scope = BlockWip.init(self.builder, .{ .block = self }, if_expr.then_body);
                try then_scope.resolve();
                if (if_expr.else_body) |else_body| {
                    var else_scope = try self.makeChild(else_body);
                    try else_scope.resolve();
                }
                try self.pushInstruction(id, .{
                    .value = .runtime, // TODO: resolve comptime if_expr
                    .op = .if_expr,
                    .type = .void, // TODO: resolve typed blocks in the future
                    .data = .{
                        .if_expr = .{
                            .cond = if_expr.cond,
                            .then_body = .unknown,
                            .else_body = null, // if (else_scope != null) undefined else null,
                        },
                    },
                });
            },
            .br => |br| {
                // const target_inst = try self.getInstruction(br.joperand);
                try self.pushInstruction(id, .{
                    .value = .runtime, // TODO: resolve comptime br
                    .op = .br,
                    .type = .void, // TODO: resolve typed blocks in the future
                    .data = .{ .instruction = br.operand },
                    .liveness = 1,
                });
            },
            .ret => |ret| {
                const operand_inst = if (ret.operand == 0) null else try self.getInstruction(ret.operand);
                try self.pushInstruction(id, .{
                    .value = .runtime, // TODO: resolve comptime ret
                    .op = .ret,
                    .type = if (operand_inst) |inst| inst.type else .void,
                    .data = if (operand_inst != null) .{ .instruction = ret.operand } else .{ .void = {} },
                    .liveness = 1,
                });
            },
            .assign => |assign| {
                const target_inst = try self.getInstruction(assign.lhs);
                const value_inst = try self.getInstruction(assign.rhs);
                try self.pushInstruction(id, .{
                    .value = value_inst.value, // TODO: resolve comptime assign
                    .op = .local_set,
                    .type = target_inst.type,
                    .data = .{ .binOp = .{
                        .lhs = assign.lhs,
                        .rhs = assign.rhs,
                    } },
                });
            },
            .debug_var => {},
            else => std.debug.panic("unimplemented resolveInstruction: {s}", .{@tagName(hir_inst)}),
        }
    }
    pub fn pushInstruction(self: *BlockWip, id: u32, inst: Mir.Instruction) Error!void {
        self.builder.logger.log("BlockWip.pushInstruction #{d}: {s}", .{ id, @tagName(inst.op) }, tw.cyan_400);
        try self.instructions.put(self.builder.arena.allocator(), id, .{ id, try self.tryFolding(inst) });
        self.dump();
    }
    pub fn getInstruction(self: *BlockWip, id: u32) !Mir.Instruction {
        if (self.instructions.get(id)) |entry| return entry[1];
        switch (self.parent) {
            .block => |block| return try @constCast(block).getInstruction(id),
            else => self.builder.logger.panic("BlockWip.getInstruction [UNRESOLVED INSTRUCTION] (#{d})", .{id}),
        }
    }
    pub fn getInstructionPtr(self: *BlockWip, id: u32) !*Mir.Instruction {
        if (self.instructions.getPtr(id)) |entry| return &entry[1];
        switch (self.parent) {
            .block => |block| return try @constCast(block).getInstructionPtr(id),
            else => self.builder.logger.panic("BlockWip.getInstruction [UNRESOLVED INSTRUCTION] (#{d})", .{id}),
        }
    }

    pub fn removeInstruction(self: *BlockWip, id: u32) void {
        self.builder.logger.log("BlockWip.removeInstruction #{d}", .{id}, tw.red_400);
        _ = self.instructions.orderedRemove(id);
    }

    pub fn tryFolding(self: *BlockWip, instruction: Mir.Instruction) !Mir.Instruction {
        self.builder.logger.log("BlockWip.tryFolding: {s}", .{@tagName(instruction.op)}, tw.cyan_400);
        switch (instruction.op) {
            .add, .sub, .mul, .div => |tag| {
                const lhs_inst = try self.getInstructionPtr(instruction.data.binOp.lhs);
                const rhs_inst = try self.getInstructionPtr(instruction.data.binOp.rhs);

                if (lhs_inst.type == .number) lhs_inst.type = rhs_inst.type;
                if (rhs_inst.type == .number) rhs_inst.type = lhs_inst.type;
                var inst = instruction;
                inst.type = lhs_inst.type;

                if (lhs_inst.value == .runtime or rhs_inst.value == .runtime) return inst;
                const lhs_value = self.builder.getValue(lhs_inst.data.value) orelse return instruction;
                const rhs_value = self.builder.getValue(rhs_inst.data.value) orelse return instruction;

                if (lhs_value.accept(.float) and rhs_value.accept(.float)) {
                    const value = try self.builder.pushValue(.{
                        .float = switch (tag) {
                            .add => lhs_value.float + rhs_value.float,
                            .sub => lhs_value.float - rhs_value.float,
                            .mul => lhs_value.float * rhs_value.float,
                            .div => lhs_value.float / rhs_value.float,
                            else => unreachable,
                        },
                    });

                    self.removeInstruction(instruction.data.binOp.rhs);
                    self.removeInstruction(instruction.data.binOp.lhs);
                    return .{
                        .value = value,
                        .type = inst.type,
                        .op = .constant,
                        .data = .{
                            .value = value,
                        },
                    };
                }
                return inst;
            },
            .gt, .lt, .eq, .ne => {
                const lhs_inst = try self.getInstructionPtr(instruction.data.binOp.lhs);
                const rhs_inst = try self.getInstructionPtr(instruction.data.binOp.rhs);
                if (lhs_inst.type == .number) lhs_inst.type = rhs_inst.type;
                if (rhs_inst.type == .number) rhs_inst.type = lhs_inst.type;
                var inst = instruction;
                inst.type = lhs_inst.type;
                return inst;
            },
            .type => {

                // const type_inst = try self.getInstruction(instruction.data.type);
                // if (type_inst.op == .type) {
                //     return instruction;
                // }

            },
            .as => {
                const lhs_inst = try self.getInstruction(instruction.data.binOp.lhs);
                const rhs_inst = try self.getInstruction(instruction.data.binOp.rhs);
                if (lhs_inst.type == rhs_inst.type) return instruction;

                //TODO: check if casting is allowec
                self.removeInstruction(instruction.data.binOp.lhs);
                self.removeInstruction(instruction.data.binOp.rhs);
                return .{
                    .value = lhs_inst.value, // TODO: resolve comptime as
                    .op = lhs_inst.op,
                    .type = rhs_inst.data.type,
                    .data = lhs_inst.data,
                };
            },
            .local_set => {
                const value_inst = try self.getInstruction(instruction.data.binOp.rhs);
                var local_inst = try self.getInstructionPtr(instruction.data.binOp.lhs);
                if (local_inst.type == .unknown) {
                    local_inst.type = value_inst.type;
                }
                return .{
                    .value = value_inst.value,
                    .op = .local_set,
                    .type = instruction.type,
                    .data = instruction.data,
                };
            },
            .ret => {
                // const value_inst = try self.getInstruction(instruction.data.instruction);
                // if (value_inst.value == .runtime) return instruction;
            },
            else => {},
        }
        return instruction;
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
    const file = try std.fs.cwd().openFile("./playground.zig", .{});
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
    try serializer.writeJSON(
        []const Mir.Instruction,
        std.io.getStdErr().writer().any(),
        mir.instructions.items,
        .{
            .lists = &mir.lists,
            .interned = &mir.strings,
        },
    );

    try serializer.writeTsType(Mir.Instruction, "Instruction", std.io.getStdErr().writer().any());
}
