const Hir = @import("Hir.zig");
const Ast = @import("Ast.zig");
const std = @import("std");
const Self = @This();
const ErrorManager = @import("ErrorManager.zig");
const InternedSlice = @import("InternedStrings.zig").InternedSlice;
const assert = @import("assert.zig");
const shared = @import("shared.zig");
const Logger = @import("Logger.zig");
const serializer = @import("serializer.zig");
const host = @import("host.zig");
const tw = @import("tw.zig");

hir: *Hir,
allocator: std.mem.Allocator,
arena: std.heap.ArenaAllocator,
error_manager: *ErrorManager,
logger: Logger = Logger.init(host.getStdErrWriter(), "HirBuilder"),

pub fn gen(allocator: std.mem.Allocator, ast: *Ast, error_manager: *ErrorManager) !Hir {
    var hir = try Hir.init(allocator, ast);

    var builder = Self{
        .allocator = allocator,
        .hir = &hir,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .error_manager = error_manager,
    };
    defer builder.deinit();
    try builder.genRoot();
    return hir;
}
pub fn deinit(self: *Self) void {
    self.arena.deinit();
}
pub fn pushType(self: *Self, ty: Hir.Type, hash: u64) !Hir.Type.Index {
    if (self.type_map.get(hash)) |index| return index;
    const index = Hir.Type.Index.fromInt(@intCast(self.hir.types.items.len));
    try self.hir.types.append(self.allocator, ty);
    try self.type_map.put(self.arena.allocator(), hash, index);
    return index;
}

pub fn genRoot(self: *Self) !void {
    self.logger.open("#0 genRoot", .{});
    defer self.logger.close();
    var scope = Scope.init(self, null, .module, "root");
    _ = try self.genModule(&scope, 0);

    _ = try scope.commit();
}
pub fn newList(self: *Self) Hir.Lists.List {
    return self.hir.lists.new(self.allocator);
}
pub fn internNodeSlice(self: *Self, node_index: Ast.Node.Index) !InternedSlice {
    const tag = self.hir.ast.getNodeTag(node_index);
    assert.fmt(tag == .identifier or tag == .number_literal or tag == .string_literal, "expected identifier or number_literal or string_literal, got {}", .{tag});
    const token = self.hir.ast.getNodeStartToken(node_index);
    const slice = self.hir.ast.getTokenSlice(token);
    return try self.hir.strings.intern(slice);
}
const Scope = struct {
    label: []const u8,
    kind: Kind,
    parent: ?*Scope,
    builder: *Self,
    instructions: Hir.Lists.List,

    symbols_table: std.StringHashMapUnmanaged(Hir.Inst.Index) = .{},

    const Kind = enum {
        module,
        block,
    };

    pub fn init(builder: *Self, parent: ?*Scope, kind: Kind, label: []const u8) Scope {
        // std.debug.print("[INIT_SCOPE]: {s} {{\n", .{label});
        return .{
            .builder = builder,
            .parent = parent,
            .kind = kind,
            .instructions = builder.newList(),
            .label = label,
        };
    }
    pub fn commit(self: *Scope) !usize {
        return try self.instructions.commit();
    }
    pub fn resolveSymbol(self: *Scope, name: []const u8) ?Hir.Inst.Index {
        if (self.symbols_table.get(name)) |index| return index;
        return null;
    }
    pub fn resolveSymbolRecursively(self: *Scope, name: []const u8) ?Hir.Inst.Index {
        self.builder.logger.open("resolveSymbolRecursively \"{s}\"", .{name});
        defer self.builder.logger.close();
        if (self.symbols_table.get(name)) |index| return index;
        if (self.parent) |parent| return parent.resolveSymbolRecursively(name);
        return null;
    }

    pub fn pushSymbol(self: *Scope, name: []const u8, index: Hir.Inst.Index) !void {
        const inst = self.builder.hir.insts.items[index];
        const tag = std.meta.activeTag(inst);
        assert.fmt(tag == .local or tag == .param_decl or tag == .global_decl, "expected local, param_decl or global_decl instruction, got {s}", .{@tagName(tag)});
        self.builder.logger.open("pushSymbol \"{s}\" {d}", .{ name, index });
        defer self.builder.logger.close();
        if (self.symbols_table.contains(name)) {
            self.builder.logger.panic("Symbol already defined: {s}", .{name});
        }
        try self.symbols_table.put(self.builder.arena.allocator(), name, index);
    }
    pub fn pushInstruction(self: *Scope, inst: Hir.Inst) !Hir.Inst.Index {
        const index = try self.builder.pushInstruction(inst);
        if (self.kind == .block) try self.instructions.append(index);
        return index;
    }
    pub fn reserveInstruction(self: *Scope) !Hir.Inst.Index {
        const index = try self.builder.reserveInstruction();
        if (self.kind == .block) try self.instructions.append(index);
        return index;
    }
    pub fn setInstruction(self: *Scope, index: Hir.Inst.Index, inst: Hir.Inst) void {
        self.builder.setInstruction(index, inst);
        // if (self.kind == .block) try self.instructions.append(index);
    }
};

const HirBuilderError = error{
    OutOfMemory,
    NotImplemented,
    SymbolAlreadyDefined,
    SymbolNotFound,
} || std.io.AnyWriter.Error;

pub fn genBinaryExpression(self: *Self, comptime tag: Ast.Node.Tag, scope: *Scope, node_index: Ast.Node.Index) HirBuilderError!Hir.Inst.Index {
    const nav = Ast.Navigator.init(self.hir.ast, node_index);
    const data = @field(nav.data.*, @tagName(tag));
    const lhs_inst = try self.genInstruction(scope, data.lhs);
    const rhs_inst = try self.genInstruction(scope, data.rhs);
    const bin: Hir.Inst.BinaryOp = .{ .lhs = lhs_inst, .rhs = rhs_inst };
    var inst = comptime @unionInit(Hir.Inst, @tagName(tag), undefined);
    @field(inst, @tagName(tag)) = bin;
    return try scope.pushInstruction(inst);
}
pub fn genInstruction(self: *Self, scope: *Scope, node_index: Ast.Node.Index) HirBuilderError!Hir.Inst.Index {
    var nav = Ast.Navigator.init(self.hir.ast, node_index);
    self.logger.open("#{d} genInstruction .{s}", .{ node_index, @tagName(nav.tag) });
    defer self.logger.close();

    switch (nav.data.*) {
        .root => unreachable,
        .block => {
            const inst = try self.genBlockInstruction(scope, node_index, null);
            try scope.instructions.append(inst);
            return inst;
        },
        .var_decl, .const_decl => |declaration| {
            // const declaration = nav.data.declaration;
            const name_node = declaration.name;
            const ty_node = declaration.type;
            const value_node = declaration.value;
            var ty_inst: ?Hir.Inst.Index = null;
            if (ty_node != 0) {
                ty_inst = try self.genInstruction(scope, ty_node);
                // value_inst = try scope.pushInstruction(.{ .as = .{ .lhs = local_inst, .rhs = ty_inst } });
            }
            var value_inst: Hir.Inst.Index = blk: {
                if (value_node != 0) {
                    break :blk try self.genInstruction(scope, value_node);
                }
                break :blk try scope.pushInstruction(.{ .undefined_value = null });
            };
            if (ty_inst == null) {
                ty_inst = try scope.pushInstruction(.{ .typeof = .{ .operand = value_inst } });
            }

            const local_inst = try scope.pushInstruction(.{
                .local = .{
                    .name_node = name_node,
                    .mutable = switch (nav.data.*) {
                        .var_decl => true,
                        .const_decl => false,
                        else => unreachable,
                    },
                    .type = ty_inst.?,
                },
            });

            nav.move(declaration.name);
            value_inst = try scope.pushInstruction(.{ .as = .{ .lhs = value_inst, .rhs = ty_inst.? } });

            _ = try scope.pushInstruction(.{ .local_set = .{ .lhs = local_inst, .rhs = value_inst } });

            try scope.pushSymbol(nav.getNodeSlice(), local_inst);
            return local_inst;

            // nav.move(declaration.name);
            // const name_slice = nav.getNodeSlice();
            // switch (nav.data.*) {
            //     .var_decl => {
            //         const local_inst = try scope.pushInstruction(.{
            //             .local = .{
            //                 .name_node = name_node,
            //                 .mutable = true,
            //                 .init = value_inst,
            //                 .type = ty_inst.?,
            //             },
            //         });
            //         _ = try scope.pushInstruction(.{ .local_set = .{ .lhs = local_inst, .rhs = value_inst } });
            //         try scope.pushSymbol(name_slice, local_inst);
            //         return local_inst;
            //     },
            //     else => {
            //         const local_inst = try scope.pushInstruction(.{
            //             .local = .{
            //                 .name_node = name_node,
            //                 .mutable = false,
            //                 .init = value_inst,
            //                 .type = ty_inst.?,
            //             },
            //         });
            //         try scope.pushSymbol(name_slice, local_inst);
            //         return local_inst;
            //     },
            // }
        },
        .ty_assign => |bin_expr| {
            const lhs_inst = try self.genInstruction(scope, bin_expr.lhs);
            const rhs_inst = try self.genInstruction(scope, bin_expr.rhs);

            return try scope.pushInstruction(.{ .as = .{ .lhs = lhs_inst, .rhs = rhs_inst } });
        },

        .assign => |bin_expr| {
            // const lhs_inst = try self.genInstruction(scope, nav.data.binary_expression.lhs);
            // nav.move(nav.data.binary_expression.lhs);
            nav.move(bin_expr.lhs);
            const slice = nav.getNodeSlice();
            const lhs_local = scope.resolveSymbolRecursively(slice) orelse return error.SymbolNotFound;
            // const lhs_interned = try scope.builder.strings.intern(nav.getNodeSlice());
            const rhs_inst = try self.genInstruction(scope, bin_expr.rhs);

            return try scope.pushInstruction(.{ .local_set = .{ .lhs = lhs_local, .rhs = rhs_inst } });
        },

        .identifier => {
            const inst_index = scope.resolveSymbolRecursively(nav.getNodeSlice()) orelse return error.SymbolNotFound;
            const inst: Hir.Inst = scope.builder.hir.insts.items[inst_index];
            switch (inst) {
                .param_decl => {
                    return try scope.pushInstruction(.{ .param_get = .{ .operand = inst_index } });
                },
                .global_decl => {
                    return try scope.pushInstruction(.{ .global_get = .{ .operand = inst_index } });
                },
                .local => {
                    return try scope.pushInstruction(.{ .local_get = .{ .operand = inst_index } });
                },
                else => {},
            }

            return inst_index;
        },
        .number_literal => {
            return try scope.pushInstruction(.{ .comptime_number = nav.node });
        },
        .ret_expression => |ret_expr| {
            return try scope.pushInstruction(.{
                .ret = .{
                    .operand = if (ret_expr.node == 0) 0 else try self.genInstruction(scope, ret_expr.node),
                },
            });
        },
        .if_expr => |data| {
            nav.move(data.condition);
            const cond_inst = blk: {
                if (nav.is(.group)) break :blk try self.genInstruction(scope, nav.data.group.node);
                break :blk try self.genInstruction(scope, data.condition);
            };
            // const cond_inst = try self.genInstruction(scope, data.condition);
            // try scope.instructions.append(cond_inst);
            const body_inst = try self.genBlockInstruction(scope, data.then_branch, null);

            var if_expr = Hir.Inst.IfExpr{
                .cond = cond_inst,
                .then_body = body_inst,
                .else_body = null,
            };
            if (data.else_branch > 0) {
                const else_body_inst = try self.genBlockInstruction(scope, data.else_branch, null);
                if_expr.else_body = else_body_inst;
            }
            return try scope.pushInstruction(.{ .if_expr = if_expr });
        },
        .ty_number => return try scope.pushInstruction(.{ .ty_number = nav.node }),
        .ty_boolean => return try scope.pushInstruction(.{ .ty_boolean = nav.node }),
        .ty_i32 => return try scope.pushInstruction(.{ .ty_i32 = nav.node }),
        .ty_i64 => return try scope.pushInstruction(.{ .ty_i64 = nav.node }),
        .ty_f32 => return try scope.pushInstruction(.{ .ty_f32 = nav.node }),
        .ty_f64 => return try scope.pushInstruction(.{ .ty_f64 = nav.node }),
        .while_loop => {
            const loop_index = try scope.reserveInstruction();
            var loop_scope = Scope.init(self, scope, .block, "while_loop");
            const cond_inst = try self.genInstruction(&loop_scope, nav.data.while_loop.condition);

            // const break_instruction = try self.pushInstruction(.{ .br = .{ .operand = loop_index } });
            // var else_list = self.newList();
            // try else_list.append(break_instruction);
            // const else_block = try self.pushInstruction(.{ .inline_block = .{
            //     .name_node = null,
            //     .instructions = try else_list.commit(),
            // } });
            _ = try loop_scope.pushInstruction(.{ .if_expr = .{
                .cond = cond_inst,
                .then_body = try self.genBlockInstruction(&loop_scope, nav.data.while_loop.body, loop_index),
                .else_body = null,
            } });
            self.setInstruction(loop_index, .{
                .loop = .{
                    .body = try self.pushInstruction(.{
                        .block = .{
                            .name_node = null,
                            .instructions_list = try loop_scope.commit(),
                        },
                    }),
                },
            });
            return loop_index;
        },
        .group => {
            return try self.genInstruction(scope, nav.data.group.node);
        },
        .fn_call => {
            const callee = try self.genInstruction(scope, nav.data.fn_call.callee);
            var args_iter = self.hir.ast.node_lists.iterList(nav.data.fn_call.args_list);
            const args_list: Hir.Inst.List = blk: {
                if (nav.data.fn_call.args_list == 0) break :blk 0;
                var list = self.newList();
                while (args_iter.next()) |arg| {
                    try list.append(try self.genInstruction(scope, arg));
                }
                break :blk try list.commit();
            };
            const index = try scope.pushInstruction(.{ .fn_call = .{
                .callee = callee,
                .args_list = args_list,
            } });

            return index;
        },

        .fn_decl => {
            self.logger.todo("Error message: functions are only allowed at the module level", .{});
        },
        else => {
            switch (nav.tag) {
                inline .add, .sub, .mul, .div, .gt, .lt, .ge, .le, .eq, .ne => |tag| return try self.genBinaryExpression(tag, scope, nav.node),
                else => {},
            }
        },
    }
    self.logger.panic("unimplemented genInstruction: {s}", .{@tagName(nav.tag)});
}
pub fn genScopedBlockInstruction(self: *Self, scope: *Scope, node_index: Ast.Node.Index, break_index: ?Hir.Inst.Index) !Hir.Inst.Index {
    self.logger.open("#{d} genScopedBlockInstruction", .{node_index});
    defer self.logger.close();
    var nav = Ast.Navigator.init(self.hir.ast, node_index);
    const is_inline_block = !nav.is(.block);
    if (!is_inline_block) {
        var statements_iter = self.hir.ast.node_lists.iterList(nav.data.block.list);
        while (statements_iter.next()) |child| {
            _ = try self.genInstruction(scope, child);
            // try scope.instructions.append(inst_index);
        }
    } else {
        _ = try self.genInstruction(scope, node_index);
    }
    for (scope.instructions.list.items) |inst_index| {
        try self.logger.printLnIndented("instruction: {d}", .{inst_index});
    }
    if (break_index) |break_inst| {
        const br_inst = try scope.builder.pushInstruction(.{ .br = .{ .operand = break_inst } });
        try scope.instructions.append(br_inst);
    }
    const instructions = try scope.commit();
    assert.fmt(scope.parent != null, "blocks should always have a parent scope", .{});
    if (is_inline_block) {
        return try scope.builder.pushInstruction(.{
            .inline_block = .{
                .name_node = null,
                .instructions_list = instructions,
            },
        });
    }
    return try scope.builder.pushInstruction(.{
        .block = .{
            .name_node = null,
            .instructions_list = instructions,
        },
    });
}
pub fn genBlockInstruction(self: *Self, parent_scope: *Scope, node_index: Ast.Node.Index, break_index: ?Hir.Inst.Index) !Hir.Inst.Index {
    self.logger.open("#{d} genBlockInstruction", .{node_index});
    defer self.logger.close();
    // var nav = Ast.Navigator.init(self.hir.ast, node_index);
    // nav.assertTag(.block);
    var scope = Scope.init(self, parent_scope, .block, "block");

    const inst = try self.genScopedBlockInstruction(&scope, node_index, break_index);
    // try parent_scope.instructions.append(inst);
    return inst;
}
const WipDef = struct {
    inst: Hir.Inst.Index,
    init_node: ?Ast.Node.Index,
    ty_node: ?Ast.Node.Index,
};
pub fn genModule(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !Hir.Inst.Index {
    self.logger.open("#{d} genModule", .{node_index});
    defer self.logger.close();
    const index = try self.reserveInstruction();
    var nav = Ast.Navigator.init(self.hir.ast, node_index);
    nav.assertTag(.root);
    var defs = std.ArrayList(WipDef).init(self.arena.allocator());
    defer defs.deinit();
    var decl_list = self.newList();
    var iter = self.hir.ast.node_lists.iterList(nav.data.root.list);
    while (iter.next()) |child| {
        const wip = try self.genDef(scope, child);
        try decl_list.append(wip.inst);
        try defs.append(wip);
    }

    for (defs.items) |def| {
        const inst = &self.hir.insts.items[def.inst];
        switch (inst.*) {
            .global_decl => {
                if (inst.global_decl.is_fn) {
                    // self.hir.insts.items[def.inst].global_decl.type = if (def.ty_node) |ty_node| try self.genInstruction(scope, ty_node) else null;
                    // self.hir.insts.items[def.inst].global_decl.init = if (def.init_node) |init_node| try self.genInstruction(scope, init_node) else null;
                    // if (def.init_node) |node| {
                    //     const type_inst_index = try self.genInstruction(scope, node);
                    //     self.hir.insts.items[def.inst].global_decl.type = type_inst_index;
                    //     const type_inst = self.hir.insts.items[type_inst_index];
                    //     self.hir.insts.items[def.inst].global_decl.init = type_inst.fn_decl.init;
                    // }
                    const fn_decl_inst = try self.genFnDeclInstruction(
                        scope,
                        def.ty_node orelse unreachable,
                    );
                    self.hir.insts.items[def.inst].global_decl.init = fn_decl_inst.init;
                    self.hir.insts.items[def.inst].global_decl.type = fn_decl_inst.index;
                    continue;
                }

                const inline_block = try self.genInlineBlock(scope, def.ty_node, def.init_node);
                self.hir.insts.items[def.inst].global_decl.init = inline_block.index;
                self.hir.insts.items[def.inst].global_decl.type = inline_block.ty_inst;
            },
            else => unreachable,
        }

        // if (def.init_node) |node| {
        //     // const init_inst = try self.genInstruction(scope, node);
        //     inst.global_decl.init = try self.genInstruction(scope, node);
        // }
        // if (def.ty_node) |node| {
        //     if (self.hir.ast.getNodeTag(node) == .fn_decl) { // True for fn decls
        //         // assert.fmt(self.hir.ast.getNodeTag(node) == .fn_decl, "expected fn_decl, got {}", .{self.hir.ast.getNodeTag(node)});
        //         inst.global_decl.type = inst.global_decl.init;
        //         continue;
        //     }
        //     self.hir.insts.items[def.inst].global_decl.type = try self.genInstruction(scope, node);
        // }
    }

    self.setInstruction(index, .{
        .mod_decl = .{
            .name_node = null,
            .declarations_list = try decl_list.commit(),
        },
    });
    return index;
}

pub fn genFnDeclInstruction(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !struct {
    index: Hir.Inst.Index,
    init: ?Hir.Inst.Index,
} {
    var nav = Ast.Navigator.init(self.hir.ast, node_index);
    const fn_decl = nav.data.fn_decl;
    const proto = fn_decl.proto;
    // var proto_scope = Scope.init(self, scope);
    const proto_data = nav.getDataOf(proto).fn_proto;
    const body = fn_decl.body;
    var fn_scope = Scope.init(self, scope, .block, "fn_decl");
    const params = try self.genFnParams(&fn_scope, proto);
    const return_type = try self.genInstruction(scope, proto_data.ret_type);
    // const init_inst = if
    const init_inst = blk: {
        if (body == 0) break :blk null;
        const inst = try self.genScopedBlockInstruction(&fn_scope, body, null);
        try scope.instructions.append(inst);
        break :blk inst;
    };
    // try scope.instructions.append(params);

    return .{
        .index = try scope.pushInstruction(.{
            .fn_decl = .{
                .name_node = proto_data.name,
                .params_list = params,
                .return_type = return_type,
            },
        }),
        .init = init_inst,
    };
}
pub fn genInlineBlock(self: *Self, parent_scope: *Scope, ty_node: ?Ast.Node.Index, init_node: ?Ast.Node.Index) !struct {
    ty_inst: ?Hir.Inst.Index,
    index: Hir.Inst.Index,
} {
    var scope = Scope.init(self, parent_scope, .block, "inline_block");

    const ty_inst = if (ty_node) |node| try self.genInstruction(&scope, node) else null;
    const init_inst = init: {
        if (init_node) |node| {
            const inst = try self.genInstruction(&scope, node);
            if (ty_inst) |ty_inst_index| {
                break :init try scope.pushInstruction(.{ .as = .{ .lhs = inst, .rhs = ty_inst_index } });
            }
            break :init inst;
        }
        break :init try scope.pushInstruction(.{ .undefined_value = null });
    };
    _ = init_inst; // autofix

    const index = try scope.commit();
    return .{
        .ty_inst = ty_inst,
        .index = try parent_scope.pushInstruction(.{
            .inline_block = .{ .name_node = null, .instructions_list = index },
        }),
    };
    // const index = try self.reserveInstruction();
}
pub fn genDef(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !WipDef {
    self.logger.open("#{d} genDef", .{node_index});
    defer self.logger.close();
    var ast = Ast.Navigator.init(self.hir.ast, node_index);
    const visibility: shared.Visibility = visibility: {
        if (ast.is(.@"pub")) {
            ast.move(ast.data.@"pub".node);
            break :visibility .public;
        }
        break :visibility .private;
    };

    const is_extern: bool = is_extern: {
        if (ast.is(.@"extern")) {
            ast.move(ast.data.@"extern".node);
            break :is_extern true;
        }
        break :is_extern false;
    };

    const is_export: bool = is_export: {
        if (ast.is(.@"export")) {
            ast.move(ast.data.@"export".node);
            break :is_export true;
        }
        break :is_export false;
    };
    if (ast.is(.fn_decl)) {
        const fn_decl_node = ast.node;
        const fn_decl = ast.data.fn_decl;
        const proto = fn_decl.proto;
        const proto_data = ast.forkTo(proto).data.fn_proto;

        // const init_inst = try self.genInstruction(scope, ast.node);
        const inst = try scope.pushInstruction(.{
            .global_decl = .{
                .name_node = proto_data.name,
                .visibility = visibility,
                .exported = is_export,
                .mutable = false,
                .@"extern" = is_extern,
                .is_fn = true,

                .init = null,
                .type = null,
            },
        });
        ast.move(proto_data.name);
        try scope.pushSymbol(ast.getNodeSlice(), inst);
        return .{
            .inst = inst,
            .init_node = fn_decl.body,
            .ty_node = fn_decl_node,
        };
    }

    assert.fmt(ast.tag == .const_decl or ast.tag == .var_decl, "expected const_decl or var_decl, got {}", .{ast.tag});

    var global_decl = Hir.Inst.GlobalDecl{
        .name_node = undefined,
        .is_fn = false,
        .visibility = visibility,
        .exported = is_export,
        .@"extern" = is_extern,
        .mutable = ast.is(.var_decl),
        .init = undefined,
        .type = null,
    };
    const decl = switch (ast.tag) {
        .const_decl => ast.data.const_decl,
        .var_decl => ast.data.var_decl,
        else => unreachable,
    };
    global_decl.name_node = decl.name;
    const inst = try scope.pushInstruction(.{ .global_decl = global_decl });
    var name = ast.forkTo(decl.name);

    try scope.pushSymbol(name.getNodeSlice(), inst);
    return .{
        .inst = inst,
        .init_node = if (decl.value != 0) decl.value else null,
        .ty_node = if (decl.type != 0) decl.type else null,
    };
}
pub fn pushInstruction(self: *Self, instruction: Hir.Inst) !Hir.Inst.Index {
    const index: Hir.Inst.Index = @intCast(self.hir.insts.items.len);
    self.logger.log(
        "pushInstruction: #{d} {s}",
        .{ index, @tagName(instruction) },
        tw.yellow_400,
    );
    try self.hir.insts.append(self.hir.allocator, instruction);
    return index;
}
pub fn reserveInstruction(self: *Self) !Hir.Inst.Index {
    const index: Hir.Inst.Index = @intCast(self.hir.insts.items.len);
    try self.hir.insts.append(self.hir.allocator, undefined);
    return index;
}
pub fn setInstruction(self: *Self, index: Hir.Inst.Index, instruction: Hir.Inst) void {
    self.hir.insts.items[index] = instruction;
}
pub fn genFnParams(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !Hir.Inst.List {
    var nav = Ast.Navigator.init(self.hir.ast, node_index);
    nav.assertTag(.fn_proto);
    var params = self.newList();
    var iter = self.hir.ast.node_lists.iterList(nav.data.fn_proto.params_list);

    while (iter.next()) |child| {
        var nav_param = Ast.Navigator.init(self.hir.ast, child);
        // const name_inst = try scope.pushInstruction(.{ .decl_param =  });
        const ty = try self.genInstruction(scope.parent.?, nav_param.data.fn_param.type);
        const inst_index = try self.pushInstruction(
            .{
                .param_decl = .{
                    .name_node = nav_param.data.fn_param.name,
                    .ty = ty,
                },
            },
        );

        nav_param.move(nav_param.data.fn_param.name);
        try scope.pushSymbol(nav_param.getNodeSlice(), inst_index);
        // var keyIter = scope.symbols_table.keyIterator();
        // while (keyIter.next()) |key| {
        //     std.debug.print("key: {s} ptr: {*}\n", .{ key.*, scope });
        // }
        // std.debug.print("scope.symbols_table: {}\n", .{scope.symbols_table.keyIterator()});

        // try scope.pushSymbol(nav_param.getNodeSlice(), inst_index);

        try params.append(inst_index);
    }
    return try params.commit();
}
test "HirBuilder" {
    const test_allocator = std.testing.allocator;
    const file = try std.fs.cwd().openFile("./playground.zig", .{});
    defer file.close();
    const source = try file.readToEndAlloc(test_allocator, 1024 * 1024);
    defer test_allocator.free(source);
    // std.debug.print("source:\n\n{s}\n", .{source});

    var errors = try ErrorManager.init(test_allocator);
    defer errors.deinit();
    var ast = try Ast.parse(test_allocator, &errors, source);
    defer ast.deinit();
    std.debug.print("AST:\n", .{});
    try ast.format(std.io.getStdErr().writer().any(), 0, .{});

    var hir = try Self.gen(test_allocator, &ast, &errors);
    defer hir.deinit();
    try serializer.writeJSON([]Hir.Inst, std.io.getStdErr().writer().any(), hir.insts.items, .{
        .lists = &hir.lists,
    });

    // std.debug.print("Hir:\n{}\n", .{hir});
}