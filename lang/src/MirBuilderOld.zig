const std = @import("std");
const Mir = @import("Mir.zig");
const Hir = @import("Hir.zig");
const HirBuilder = @import("HirBuilder.zig");
const Ast = @import("Ast.zig");
const ErrorManager = @import("ErrorManager.zig");
const assert = @import("assert.zig");
const InternedSlice = @import("InternedStrings.zig").InternedSlice;
const Logger = @import("Logger.zig");

const Allocator = std.mem.Allocator;
const Self = @This();
errors: *ErrorManager,
hir: *Hir,
mir: *Mir,
allocator: Allocator,
arena: std.heap.ArenaAllocator,
logger: Logger,

pub fn gen(allocator: Allocator, hir: *Hir, errors: *ErrorManager) !Mir {
    var mir = try Mir.init(allocator);
    var builder = Self{
        .logger = Logger.init(.mir),
        .errors = errors,
        .hir = hir,
        .mir = &mir,
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
    };
    defer builder.deinit();

    try builder.genModule(Hir.Inst.RootIndex);

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
pub fn newList(self: *Self) Mir.ChildList {
    return self.mir.lists.new(self.mir.allocator);
}
pub fn getHirInst(self: *Self, index: Hir.Inst.Index) Hir.Inst {
    return self.hir.insts.items[index];
}
pub fn getDefinition(self: *Self, index: Mir.Definition.Index) Mir.Definition {
    return self.mir.definitions.items[index];
}
pub fn getDefinitionPtr(self: *Self, index: Mir.Definition.Index) *Mir.Definition {
    return &self.mir.definitions.items[index];
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
pub fn pushInstruction(self: *Self, inst: Mir.Inst) !Mir.Inst.Index {
    const index = Mir.Inst.Index.toTypeIndex(@intCast(self.mir.instructions.items.len));
    try self.mir.instructions.append(self.allocator, inst);
    return index;
}
pub fn setDefinition(self: *Self, index: Mir.Definition.Index, definition: Mir.Definition) void {
    self.mir.definitions.items[index] = definition;
}
const ModuleWip = struct {
    hir_inst: Hir.Inst.Index,
    parent: ?*Scope,
    name: ?InternedSlice,

    index: Mir.Definition.Index,
    type_index: Mir.Type.Index,
    builder: *Self,
    definitions_table: std.AutoHashMapUnmanaged(InternedSlice, WipSymbol) = .{},
};
const Scope = union(enum) {
    module: ModuleWip,
    block: BlockWip,

    pub fn initBlock(
        parent: *Scope,
        builder: *Self,
        expected_type: Mir.Type.Index,
        root_inst: Hir.Inst.Index,
    ) !Scope {
        builder.logger.open("initBlock", .{});

        return .{
            .block = .{
                .parent = parent,
                .builder = builder,
                .expected_type = expected_type,
                .root_inst = root_inst,
                .instructions = builder.newList(),
            },
        };
    }
    pub fn initModule(parent: ?*Scope, builder: *Self, hir_inst: Hir.Inst.Index) !Scope {
        builder.logger.open("initModule", .{});
        return .{
            .module = .{
                .hir_inst = hir_inst,
                .parent = parent,
                .name = null,
                .index = try builder.reserveDefinitionIndex(),
                .type_index = try builder.reserveTypeIndex(),
                .builder = builder,
                .definitions_table = .{},
            },
        };
    }
    pub fn commit(self: *Scope) !void {
        switch (self.*) {
            .module => |*mod| {
                var list = mod.builder.newList();
                var iter = mod.definitions_table.valueIterator();
                while (iter.next()) |def| {
                    switch (def.*) {
                        .resolved => |index| {
                            try list.append(index);
                        },
                        else => {
                            unreachable;
                        },
                    }
                }
                mod.builder.mir.definitions.items[mod.index] = .{
                    .module = .{
                        .name = mod.name,
                        .member_defs = try list.commit(),
                    },
                };
                // // try mod.definitions_table.ensureTotalCapacity(mod.builder.arena.allocator(), mod.definitions_table.count());
            },
            .block => |*block| {
                _ = block; // autofix
            },
        }
    }

    pub fn pushDefinition(self: *Scope, name: InternedSlice, inst_index: Hir.Inst.Index) !void {
        const builder = self.getBuilder();
        builder.logger.indented("[PUSH_DEFINITION]: \"{s}\"", .{builder.mir.strings.getSlice(name)});

        const wip = WipSymbol{
            .idle = .{
                .name = name,
                .inst_index = inst_index,
            },
        };
        switch (self.*) {
            .module => |*mod| try mod.definitions_table.put(mod.builder.arena.allocator(), name, wip),
            .block => |*block| try block.definitions_table.put(block.builder.arena.allocator(), name, wip),
        }
    }
    pub fn resolveWipDef(self: *Scope, name: InternedSlice) ?*WipSymbol {
        switch (self.*) {
            .module => |module| return module.definitions_table.getPtr(name) orelse if (module.parent) |parent| parent.resolveWipDef(name) else null,
            .block => |block| return block.definitions_table.getPtr(name) orelse block.parent.resolveWipDef(name),
        }
        // return self.definitions_table.getPtr(name) orelse if (self.parent) |parent| parent.resolveWipDef(name) else null;
    }
    // Recursively collects symbols from all modules,
    pub fn collectSymbols(self: *Scope) !void {
        const builder = self.getBuilder();
        builder.logger.open("[COLLECT_SYMBOLS]", .{});
        defer builder.logger.close();
        switch (self.*) {
            .module => |*mod| {
                const inst = mod.builder.getHirInst(mod.hir_inst);
                var iter = mod.builder.iterHirList(inst.mod_decl.declarations);
                while (iter.next()) |decl| {
                    const decl_inst = mod.builder.getHirInst(decl);

                    switch (decl_inst) {
                        .global_decl => |global_decl| {
                            const name = try mod.builder.internNodeSlice(global_decl.name_node);

                            _ = try self.pushDefinition(name, decl);
                            // _ = try self.pushDefinition(.{
                            //     .global_decl = .{
                            //         .name = try self.builder.internNodeSlice(global_decl.name_node),
                            //         .ty = undefined, //TODO
                            //         .init_inst = null, //TODO
                            //         .visibility = global_decl.visibility,
                            //         .exported = global_decl.exported,
                            //         .@"extern" = global_decl.@"extern",
                            //     },
                            // }, decl);
                        },
                        else => {
                            unreachable;
                        },
                    }
                }
            },
            .block => |block| {
                _ = block; // autofix
            },
        }
    }

    pub fn getBuilder(self: *Scope) *Self {
        return switch (self.*) {
            .module => |*mod| mod.builder,
            .block => |*block| block.builder,
        };
    }
    const Error = error{
        OutOfMemory,
        SymbolNotFound,
        CircularDependency,
        FailedToResolveType,
        Unimplemented,
        InvalidCharacter,
        Overflow,
    };
    // pub fn resolveSymbolType(self: *Scope, name: InternedSlice) Error!Mir.Type.Index {
    //     const builder = self.getBuilder();

    // }
    pub fn resolveSymbol(self: *Scope, name: InternedSlice) Error!Mir.Definition.Index {
        const builder = self.getBuilder();
        builder.logger.open("resolveSymbol: \"{s}\"", .{builder.mir.strings.getSlice(name)});
        defer builder.logger.close();
        const wip_def = self.resolveWipDef(name) orelse return error.SymbolNotFound;

        switch (wip_def.*) {
            .idle => |idle| {
                const index = try self.resolveDefinition(idle.name, idle.inst_index);
                // _ = try self.resolveInstruction(idle.inst_index);
                wip_def.* = .{ .resolved = index };
                return index;
            },
            .resolving => {
                return error.CircularDependency;
            },
            .resolved => |def_index| {
                return def_index;
            },
        }
    }
    pub fn resolveDefinition(self: *Scope, name: InternedSlice, hir_inst_index: Hir.Inst.Index) !Mir.Definition.Index {
        var builder = self.getBuilder();
        const index = try builder.reserveDefinitionIndex();

        const hir_inst = builder.getHirInst(hir_inst_index);

        // std.debug.print("resolveDefinition: {s}\n", .{@tagName(std.meta.activeTag(hir_inst))});
        builder.logger.open("resolveDefinition(\"{s}\")", .{builder.mir.strings.getSlice(name)});
        defer builder.logger.close();

        switch (hir_inst) {
            .global_decl => |global_decl| {
                const ty = if (global_decl.type) |type_inst| try self.resolveType(type_inst) else .unknown;

                builder.setDefinition(index, .{
                    .global_decl = .{
                        .name = name,
                        .type = ty,
                        .body = null,
                        // .init_inst = init,

                        .visibility = global_decl.visibility,
                        .exported = global_decl.exported,
                        .@"extern" = global_decl.@"extern",
                    },
                });
                // std.debug.print("global_decl.init: {}\n", .{ty});
                // const value = blk: {
                const init_inst_index = global_decl.init;
                // const body_def = try builder.reserveDefinitionIndex();
                // var block: Scope = .{
                //     .block = .{
                //         .parent = self,
                //         .root_inst = init_inst_index,
                //         .builder = builder,
                //         .expected_type = ty,
                //         .definitions_table = .{},
                //         .instructions = builder.newList(),
                //     },
                // };
                var block_scope = try Scope.initBlock(self, builder, ty, init_inst_index);

                builder.getDefinitionPtr(index).global_decl.body = try block_scope.resolveBodyDefinition();

                return index;

                // global_decl.init

                // const name = try self.builder.internNodeSlice(global_decl.name_node);
                // _ = try self.pushDefinition(name, hir_inst_index);
            },
            else => {
                unreachable;
            },
        }
        // const wip_def = self.resolveWipDef(name) orelse return error.SymbolNotFound;

        return index;
    }

    pub fn resolveValue(self: *Scope, expr_inst: Hir.Inst.Index) Error!TypedValue {
        var builder = self.getBuilder();
        const inst = builder.getHirInst(expr_inst);
        // std.debug.print("resolveValue: {s}\n", .{@tagName(std.meta.activeTag(inst))});
        builder.logger.open("resolveValue #{d}: \"{s}\"", .{ expr_inst, @tagName(std.meta.activeTag(inst)) });
        defer builder.logger.close();
        switch (inst) {
            // .block => |block| {
            //     _ = block; // autofix

            // },
            // .ty_number => return try builder.pushValue(.{ .type = .type_number }),
            // .fn_decl => |fn_decl| {
            // _ = fn_decl; // autofix
            // assert.fmt(std.meta.activeTag(self.*) == .block, "expected block scope, got {s}", .{@tagName(std.meta.activeTag(self.*))});
            // assert.fmt(self.block.expected_type != .unknown, "expected expected_type to be set", .{});
            // const expected_type = self.block.expected_type;
            // assert.fmt(fn_decl.init != null, "expected body to be set", .{});

            // const init = fn_decl.init.?;
            // std.debug.print("init: {}\n", .{init});

            // return try builder.pushValue(.{
            //     .@"fn" = .{
            //         .type = expected_type,
            //         .instructions = try self.resolveInstruction(init),
            //     },
            // });

            // },
            //
            .comptime_number => |comptime_number| {
                const slice = builder.getNodeSlice(comptime_number);
                const ty: Mir.Type.Index = .comptime_number;
                const value = try std.fmt.parseFloat(f64, slice);
                const value_index = try builder.pushValue(.{ .float = .{
                    .type = ty,
                    .value = value,
                } });
                builder.logger.indented("{any} = \"{d}\"", .{ (value_index), value });
                return .{
                    .value = value_index,
                    .ty = ty,
                };
                // std.debug.print("comptime_number: {s}\n", .{slice});
            },

            else => {
                return error.Unimplemented;
            },
        }
        return Mir.Value.Index.undefined; // TODO
    }
    pub fn resolveBodyDefinition(self: *Scope) !Mir.Definition.Index {
        var builder = self.getBuilder();
        builder.logger.open("resolveBodyDefinition", .{});
        defer builder.logger.close();
        // const inst = builder.getHirInst(self.block.root_inst);
        var registers = std.AutoHashMap(u32, TypedInst).init(builder.arena.allocator());
        const block_inst_index = blk: {
            switch (builder.getHirInst(self.block.root_inst)) {
                .fn_decl => |fn_decl| {
                    var params_iter = builder.iterHirList(fn_decl.params);
                    while (params_iter.next()) |param| {
                        const param_inst = builder.getHirInst(param);

                        const ty = try self.resolveType(param_inst.param_decl.ty);
                        const name = try builder.internNodeSlice(param_inst.param_decl.name_node);
                        const param_def = try builder.pushDefinition(.{
                            .param = .{ .name = name, .type = ty },
                        });
                        _ = param_def; // autofix

                        // try registers.put(param, .{ .inst = param, .type = ty });

                        // std.debug.print("param_inst: {s}\n", .{@tagName(std.meta.activeTag(param_inst.param_decl.ty))});
                        // const ty = try self.resolveType(param_inst.);
                    }

                    if (fn_decl.init) |fn_init_index| {
                        break :blk fn_init_index;
                    }
                },
                .inline_block, .block => {
                    break :blk self.block.root_inst;
                },
                else => {},
            }
            unreachable;
        };

        const list = switch (builder.getHirInst(block_inst_index)) {
            .block => |block| block.instructions,
            .inline_block => |inline_block| inline_block.instructions,
            else => unreachable,
        };

        // try self.resolveInstructionBody(list);

        var iter = builder.iterHirList(list);

        while (iter.next()) |inst_index| {
            const hir_inst = builder.getHirInst(inst_index);
            // _ = try self.resolveInstruction(inst_index);

            switch (hir_inst) {
                .block => |block| {
                    _ = block; // autofix
                },
                .comptime_number => {
                    const typed_value = try self.resolveValue(inst_index);
                    const inst = try self.block.pushInstruction(.{ .constant = typed_value.value }, typed_value.ty);
                    try registers.put(inst_index, .{ .inst = inst, .type = typed_value.ty });
                },
                .add => |bin| {
                    _ = bin; // autofix

                },
                else => {},
            }
        }

        return builder.pushDefinition(.{
            .block = .{
                .instructions = try self.block.instructions.commit(),
                .result_type = self.block.expected_type,
            },
        });
    }
    // pub fn resolveBlock(self: *Scope) !Mir.Definition.Index {
    //     var builder = self.getBuilder();
    //     defer builder.logger.close();

    //     const root_inst = builder.getHirInst(self.block.root_inst);
    //     builder.logger.open("resolveBlock({s})", .{@tagName(std.meta.activeTag(root_inst))});
    //     // std.debug.print("root_inst: {s}\n", .{@tagName(std.meta.activeTag(root_inst))});
    //     switch (root_inst) {
    //         .block => |block| {
    //             var iter = builder.iterHirList(block.instructions);
    //             var instructions = builder.newList();
    //             while (iter.next()) |inst| {
    //                 // const inst_index = try self.resolveInstruction(inst);
    //                 // try instructions.append(inst_index.asInt());
    //             }
    //             return try builder.pushDefinition(.{
    //                 .block = .{
    //                     .instructions = try instructions.commit(),
    //                     .result_type = self.block.expected_type,
    //                 },
    //             });
    //         },
    //         else => {
    //             var instructions = builder.newList();
    //             try instructions.append(self.block.root_inst);

    //             return try builder.pushDefinition(.{
    //                 .inline_block = .{
    //                     .instructions = try instructions.commit(),
    //                     .result_type = self.block.expected_type,
    //                 },
    //             });
    //         },
    //     }
    //     return 0; // TODO
    // }
    pub fn resolveTypeFromExpr(self: *Scope, expr_inst: Hir.Inst.Index) !Mir.Type.Index {
        var builder = self.getBuilder();
        const inst = builder.getHirInst(expr_inst);
        _ = inst; // autofix
        return Mir.Type.Index.unknown; // TODO
    }

    pub fn resolveType(self: *Scope, inst_index: Hir.Inst.Index) !Mir.Type.Index {
        var builder = self.getBuilder();
        const inst: Hir.Inst = builder.getHirInst(inst_index);
        // std.debug.print("resolveType: {s}\n", .{@tagName(std.meta.activeTag(inst))});
        builder.logger.open("resolveType: \"{s}\"", .{@tagName(std.meta.activeTag(inst))});
        defer builder.logger.close();
        switch (inst) {
            .ty_number => return Mir.Type.Index.number,
            .ty_boolean => return Mir.Type.Index.boolean,

            .fn_decl => |fn_decl| {
                var iter = builder.iterHirList(fn_decl.params);
                var params_types = builder.newList();
                while (iter.next()) |param| {
                    const param_inst = builder.getHirInst(param);
                    builder.logger.open("resolve param \"{s}\"", .{builder.getNodeSlice(param_inst.param_decl.name_node)});
                    defer builder.logger.close();
                    // const ty: Mir.Type = .{
                    //     .param = .{
                    //         .name = try builder.internNodeSlice(param_inst.param_decl.name_node),
                    //         .type = try self.resolveType(param_inst.param_decl.ty),
                    //     },
                    // };
                    const param_ty = try self.resolveType(param_inst.param_decl.ty);

                    // const param_ty_index = try builder.pushType(param_ty);
                    try params_types.append(param_ty.asInt());
                }
                return try builder.pushType(.{
                    .@"fn" = .{
                        .params = try params_types.commit(),
                        .ret_type = try self.resolveType(fn_decl.return_type),
                    },
                });
            },
            .decl_ref => |decl_ref| {
                const name = try builder.internNodeSlice(decl_ref);
                const def_index = try self.resolveSymbol(name);
                const def = builder.getDefinition(def_index);

                std.debug.print("def: {}\n", .{def});
                switch (def) {
                    .global_decl => |global_decl| {
                        const body = builder.getDefinition(global_decl.body.?);
                        return switch (body) {
                            .block => |block| block.result_type,
                            .inline_block => |inline_block| inline_block.result_type,
                            else => unreachable,
                        };
                    },
                    else => unreachable,
                }
                // return
                // return try self.resolveType(decl_ref.decl);
            },
            else => {
                unreachable;
            },
        }
        return Mir.Type.Index.unknown; // TODO
    }
    const TypedInst = struct {
        inst: Mir.Inst.Index,
        type: Mir.Type.Index,
    };

    pub fn resolveInstructionBody(self: *Scope, list: Hir.Inst.List) Error!void {
        var builder = self.getBuilder();
        builder.logger.open("resolveInstruction", .{});
        defer builder.logger.close();
        var registers = std.AutoHashMap(u32, TypedInst).init(builder.arena.allocator());

        var iter = builder.iterHirList(list);

        while (iter.next()) |inst_index| {
            const hir_inst = builder.getHirInst(inst_index);
            // _ = try self.resolveInstruction(inst_index);

            switch (hir_inst) {
                .block => |block| {
                    _ = block; // autofix
                },
                .comptime_number => {
                    const typed_value = try self.resolveValue(inst_index);
                    const inst = try self.block.pushInstruction(.{ .constant = typed_value.value }, typed_value.ty);
                    try registers.put(inst_index, .{ .inst = inst, .type = typed_value.ty });
                },
                .add => |bin| {
                    _ = bin; // autofix

                },
                else => {},
            }
        }
    }
    // pub fn resolveInstruction(self: *Scope, inst_index: Hir.Inst.Index) !Mir.Inst.Index {
    //     var builder = self.getBuilder();

    //     const inst: Hir.Inst = builder.getHirInst(inst_index);
    //     // std.debug.print("resolveInstruction: {s}\n", .{@tagName(std.meta.activeTag(inst))});
    //     builder.logger.open("resolveInstruction #{d}: \"{s}\"", .{ inst_index, @tagName(std.meta.activeTag(inst)) });
    //     defer builder.logger.close();

    //     switch (inst) {
    //         // .comptime_number =>
    //         //     // .fn_decl => |fn_decl| {
    //         //     //     _ = fn_decl; // autofix

    //         //     // },
    //         .global_decl => |global_decl| {
    //             const init_inst = builder.getHirInst(global_decl.init);
    //             switch (init_inst) {
    //                 .fn_decl => |fn_decl| {
    //                     if (fn_decl.init) |fn_init_index| {
    //                         const fn_init_inst = builder.getHirInst(fn_init_index);
    //                         // assert.fmt(std.meta.activeTag(fn_init_inst) == .block or std.meta.activeTag(fn_init_inst) == .inline_block, "expected block or inline_block, got {s}", .{@tagName(std.meta.activeTag(fn_init_inst))});

    //                         std.debug.print("fn_init_inst: {s}\n", .{@tagName(std.meta.activeTag(fn_init_inst))});

    //                         return try self.resolveRootBlockInstruction(fn_init_index);
    //                     }
    //                 },
    //                 .inline_block, .block => return try self.resolveRootBlockInstruction(global_decl.init),
    //                 else => {
    //                     unreachable;
    //                 },
    //             }
    //             // global_decl.init;

    //         },
    //         else => return error.Unimplemented,
    //     }
    //     return .unknown; // TODO
    // }
    pub fn resolveRootBlockInstruction(self: *Scope, inst_index: Hir.Inst.Index) !Mir.Inst.Index {
        const builder = self.getBuilder();
        const inst = builder.getHirInst(inst_index);
        assert.fmt(std.meta.activeTag(inst) == .block or std.meta.activeTag(inst) == .inline_block, "expected block or inline_block, got {s}", .{@tagName(std.meta.activeTag(inst))});
        const data = switch (inst) {
            .block => |block| block,
            .inline_block => |inline_block| inline_block,
            else => unreachable,
        };

        var iter = builder.iterHirList(data.instructions);
        while (iter.next()) |block_inst_index| {
            _ = block_inst_index; // autofix
            // _ = try self.block.pushInstruction(block_inst_index);

            // const inst_index = try self.resolveInstruction(inst);
            // try instructions.append(inst_index.asInt());
        }
        return .unknown; // TODO
    }
};
pub const WipSymbol = union(enum) {
    idle: struct {
        name: InternedSlice,
        inst_index: Hir.Inst.Index,
    },
    resolving: void,
    resolved: Mir.Type.Index,
};
const TypedValue = struct {
    value: Mir.Value.Index,
    ty: Mir.Type.Index,
};
const BlockWip = struct {
    parent: *Scope,
    builder: *Self,
    expected_type: Mir.Type.Index,
    root_inst: Hir.Inst.Index,
    definitions_table: std.AutoHashMapUnmanaged(InternedSlice, WipSymbol) = .{},
    instructions: Mir.ChildList,

    pub fn pushInstruction(self: *BlockWip, inst: Mir.Inst, ty: Mir.Type.Index) !Mir.Inst.Index {
        _ = ty; // autofix
        const index = try self.builder.pushInstruction(inst);
        try self.instructions.append(index.asInt());

        return index;
        // return try self.builder.pushInstruction(inst);
        // return .unknown; // TODO
    }
};
pub fn genModule(self: *Self, hir_inst: Hir.Inst.Index) !void {
    self.logger.open("genModule", .{});
    defer self.logger.close();
    self.assertHirInst(hir_inst, .mod_decl);
    const inst = self.getHirInst(hir_inst);
    _ = inst; // autofix

    // var module_wip: Scope = .{
    //     .module = .{
    //         .hir_inst = hir_inst,
    //         .parent = null,
    //         .name = null,
    //         .index = try self.reserveDefinitionIndex(),
    //         .builder = self,
    //         .definitions_table = .{},
    //     },
    // };

    var module_wip = try Scope.initModule(null, self, hir_inst);
    try module_wip.collectSymbols();
    var iter = module_wip.module.definitions_table.iterator();
    while (iter.next()) |entry| {
        // std.debug.print("{}\n", .{entry});
        const index = try module_wip.resolveSymbol(entry.key_ptr.*);
        _ = index; // autofix
    }
    try module_wip.commit();
}
pub fn deinit(self: *Self) void {
    self.arena.deinit();
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
