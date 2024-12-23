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
context: Context = .normal,
tracer: Tracer,
const activeTag = std.meta.activeTag;
const Tracer = @import("Tracer.zig");
const Context = enum {
    normal,
    assign_lhs,
    assign_rhs,
};

pub const Options = struct {
    trace_dir: ?[]const u8 = null,
    trace_name: ?[]const u8 = null,
    unique_trace_name: bool = true,
};
pub fn build(allocator: std.mem.Allocator, ast: *Ast, error_manager: *ErrorManager, options: Options) !Hir {
    var hir = try Hir.init(allocator, ast);

    var builder = Self{
        .allocator = allocator,
        .hir = &hir,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .error_manager = error_manager,
        .tracer = try Tracer.init(
            options.trace_dir orelse "./.tmp/trace",
            options.trace_name orelse "hir-builder",
            options.unique_trace_name,
        ),
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
    const event_id = self.tracer.beginEvent("genRoot", .{});
    defer self.tracer.endEvent(event_id, "genRoot", .{});
    self.logger.open("#0 genRoot", .{});
    defer self.logger.close();
    var scope = Scope.init(self, null, .struct_decl, "root");
    _ = try self.genStructDecl(&scope, 0);

    _ = try scope.commit();
}
pub fn newList(self: *Self) Hir.InternedLists.WorkingList {
    return self.hir.lists.new();
}
pub fn internNodeSlice(self: *Self, node_index: Ast.Node.Index) !InternedSlice {
    const tag = self.hir.ast.getNodeTag(node_index);
    assert.fmt(tag == .identifier or tag == .number_literal or tag == .string_literal, "expected identifier or number_literal or string_literal, got {}", .{tag});
    const token = self.hir.ast.getNodeStartToken(node_index);
    const slice = self.hir.ast.getTokenSlice(token);
    return try self.hir.strings.intern(slice);
}
const Scope = struct {
    index: usize,
    label: []const u8,
    kind: Kind,
    parent: ?*Scope,
    builder: *Self,
    instructions: Hir.InternedLists.WorkingList,

    symbols_table: std.StringHashMapUnmanaged(Hir.Inst.Index) = .{},
    event_id: u64,

    const Kind = enum {
        struct_decl,
        block,
    };

    pub var COUNT: usize = 0;
    pub fn init(builder: *Self, parent: ?*Scope, kind: Kind, label: []const u8) Scope {
        const event_id = builder.tracer.beginEvent("Scope.init", .{label});
        const index = Scope.COUNT;
        Scope.COUNT += 1;
        builder.logger.log("[INIT_SCOPE]: {s} {s} {d}\n", .{ @tagName(kind), label, index }, null);
        return .{
            .index = index,
            .builder = builder,
            .parent = parent,
            .kind = kind,
            .instructions = builder.newList(),
            .label = label,
            .event_id = event_id,
        };
    }
    pub fn commit(self: *Scope) !Hir.InternedLists.Range {
        self.builder.tracer.endEvent(self.event_id, "Scope.commit", .{});
        self.builder.logger.log("[COMMIT SCOPE] {s} {d}\n", .{ self.label, self.index }, null);
        if (self.kind == .struct_decl) {
            // We can discard struct decl instruction list
            // because all inner declarations will have their own instructions list
            // from their own scope
            self.instructions.deinit();
            return Hir.InternedLists.Range.empty;
        }
        return try self.instructions.commit();
    }
    pub fn resolveSymbol(self: *Scope, name: []const u8) ?Hir.Inst.Index {
        if (self.symbols_table.get(name)) |index| return index;
        return null;
    }
    pub fn resolveSymbolRecursively(self: *Scope, name: []const u8) ?Hir.Inst.Index {
        const event_id = self.builder.tracer.beginEvent("Scope.resolveSymbolRecursively", .{name});
        defer self.builder.tracer.endEvent(event_id, "Scope.resolveSymbolRecursively", .{name});
        self.builder.logger.open("resolveSymbolRecursively \"{s}\"", .{name});

        defer self.builder.logger.close();
        if (self.symbols_table.get(name)) |index| return index;
        if (self.parent) |parent| return parent.resolveSymbolRecursively(name);
        return null;
    }

    pub fn pushSymbol(self: *Scope, name: []const u8, index: Hir.Inst.Index) !void {
        self.builder.tracer.logEvent("Scope.pushSymbol", .{ name, index });
        const inst = self.builder.hir.insts.items[index];
        const tag = std.meta.activeTag(inst);
        assert.fmt(tag == .local or tag == .param_decl or tag == .global_decl or tag == .alloc, "expected local, param_decl or global_decl instruction, got {s}", .{@tagName(tag)});
        self.builder.logger.open("pushSymbol \"{s}\" {d}", .{ name, index });
        defer self.builder.logger.close();
        if (self.symbols_table.contains(name)) {
            self.builder.tracer.panic("Symbol already defined", .{name});
        }
        try self.symbols_table.put(self.builder.arena.allocator(), name, index);
    }
    pub fn pushInstruction(self: *Scope, inst: Hir.Inst) !Hir.Inst.Index {
        const index = try self.builder.pushInstruction(inst);
        self.builder.tracer.logEvent("Scope.pushInstruction", .{index});
        if (self.kind == .block) try self.instructions.append(index);
        return index;
    }
    pub fn reserveInstruction(self: *Scope) !Hir.Inst.Index {
        const event_id = self.builder.tracer.beginEvent("Scope.reserveInstruction", .{});
        _ = event_id; // autofix
        const index = try self.builder.reserveInstruction();
        if (self.kind == .block) try self.instructions.append(index);
        return index;
    }
    pub fn setInstruction(self: *Scope, index: Hir.Inst.Index, inst: Hir.Inst) void {
        self.builder.tracer.logEvent("Scope.setInstruction", .{ index, inst });
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

pub fn genTypeInstruction(self: *Self, scope: *Scope, node_index: Ast.Node.Index) HirBuilderError!Hir.Inst.Index {
    const event_id = self.tracer.beginEvent("genTypeInstruction", .{node_index});
    defer self.tracer.endEvent(event_id, "genTypeInstruction", .{node_index});
    const inst_index = try self.genInstruction(scope, node_index);
    const inst = self.hir.insts.items[inst_index];

    switch (inst) {
        .global_decl => {
            return try scope.pushInstruction(.{ .global_get = .{ .operand = inst_index } });
        },
        else => return inst_index,
    }
}
pub fn genLoadedInstruction(self: *Self, scope: *Scope, node_index: Ast.Node.Index) HirBuilderError!Hir.Inst.Index {
    const event_id = self.tracer.beginEvent("genLoadedInstruction", .{node_index});
    defer self.tracer.endEvent(event_id, "genLoadedInstruction", .{node_index});
    const inst_index = try self.genInstruction(scope, node_index);
    const inst = self.hir.insts.items[inst_index];
    switch (inst) {
        .get_element_pointer, .get_property_pointer, .alloc => {
            return scope.pushInstruction(.{ .load = .{ .operand = inst_index } });
        },
        .global_decl => {
            return try scope.pushInstruction(.{ .global_get = .{ .operand = inst_index } });
        },

        else => {
            return inst_index;
        },
    }
}
pub fn genBinaryExpression(self: *Self, comptime tag: Ast.Node.Tag, scope: *Scope, node_index: Ast.Node.Index) HirBuilderError!Hir.Inst.Index {
    const event_id = self.tracer.beginEvent("genBinaryExpression", .{node_index});
    defer self.tracer.endEvent(event_id, "genBinaryExpression", .{node_index});
    const nav = Ast.Navigator.init(self.hir.ast, node_index);
    const data = @field(nav.data.*, @tagName(tag));
    const lhs_inst = try self.genLoadedInstruction(scope, data.lhs);
    const rhs_inst = try self.genLoadedInstruction(scope, data.rhs);
    const bin: Hir.Inst.BinaryOp = .{ .lhs = lhs_inst, .rhs = rhs_inst };
    var inst = comptime @unionInit(Hir.Inst, @tagName(tag), undefined);
    @field(inst, @tagName(tag)) = bin;
    return try scope.pushInstruction(inst);
}
pub fn genInstruction(self: *Self, scope: *Scope, node_index: Ast.Node.Index) HirBuilderError!Hir.Inst.Index {
    const event_id = self.tracer.beginEvent("genInstruction", .{node_index});
    defer self.tracer.endEvent(event_id, "genInstruction", .{node_index});
    var nav = Ast.Navigator.init(self.hir.ast, node_index);
    self.logger.open("#{d} genInstruction .{s}", .{ node_index, @tagName(nav.tag) });
    defer self.logger.close();

    switch (nav.data.*) {
        .comment_line => {
            return 0;
        },
        .struct_decl => {
            var struct_scope = Scope.init(self, scope, .struct_decl, "root");
            const inst = try self.genStructDecl(&struct_scope, nav.node);
            try scope.instructions.append(inst);
            _ = try struct_scope.commit();
            return inst;
        },
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
            const name_slice = self.hir.ast.getNodeSlice(name_node);
            // var ty_inst: ?Hir.Inst.Index = null;
            const mutable = switch (nav.data.*) {
                .var_decl => true,
                .const_decl => false,
                else => unreachable,
            };

            const value_inst_index: Hir.Inst.Index = blk: {
                if (value_node != 0) {
                    break :blk try self.genLoadedInstruction(scope, value_node);
                }
                break :blk try scope.pushInstruction(.{ .undefined_value = .{ .node = null } });
            };

            const ty_inst_index = if (ty_node == 0) null else try self.genTypeInstruction(scope, ty_node);
            // const ty_inst = self.hir.insts.items[ty_inst_index];

            // try scope.pushInstruction(.{ .typeof = .{ .operand = value_inst_index } })
            const local_inst = blk: {
                const value_inst = &self.hir.insts.items[value_inst_index];
                switch (value_inst.*) {
                    .alloc => {
                        value_inst.alloc.mutable = mutable;
                        if (ty_inst_index) |index| {
                            const ty_as_pointer_inst = try scope.pushInstruction(.{ .ty_pointer = .{ .operand = index } });
                            _ = try scope.pushInstruction(.{ .as = .{ .lhs = value_inst_index, .rhs = ty_as_pointer_inst } });
                        }

                        break :blk value_inst_index;
                    },
                    else => {},
                }
                const inst = try scope.pushInstruction(.{
                    .alloc = .{
                        // .name_node = name_node,
                        .mutable = mutable,
                        .type = ty_inst_index orelse try scope.pushInstruction(.{ .typeof = .{ .operand = value_inst_index } }),
                    },
                });

                _ = try scope.pushInstruction(.{
                    .store = .{
                        .pointer = inst,
                        .value = value_inst_index,
                    },
                });
                break :blk inst;
            };

            try scope.pushSymbol(name_slice, local_inst);
            return local_inst;
        },

        .ty_assign => |bin_expr| {
            const lhs_inst = try self.genInstruction(scope, bin_expr.lhs);
            const rhs_inst = try self.genInstruction(scope, bin_expr.rhs);

            return try scope.pushInstruction(.{ .as = .{ .lhs = lhs_inst, .rhs = rhs_inst } });
        },

        .assign => |bin_expr| {
            nav.move(bin_expr.lhs);
            const slice = nav.getNodeSlice();
            // const lhs_index = scope.resolveSymbolRecursively(slice) orelse return error.SymbolNotFound;
            // const lhs_inst_index = try self.genInstruction(scope, bin_expr.lhs);
            self.context = .assign_lhs;
            const lhs_inst_index = switch (nav.tag) {
                .identifier => scope.resolveSymbolRecursively(slice) orelse return error.SymbolNotFound,
                else => try self.genInstruction(scope, bin_expr.lhs),
            };
            const lhs_inst = self.hir.insts.items[lhs_inst_index];
            self.context = .assign_rhs;
            const rhs_inst_index = try self.genLoadedInstruction(scope, bin_expr.rhs);
            self.context = .normal;
            // const lhs_inst = self.hir.insts.items[lhs_index];
            switch (lhs_inst) {
                .param_decl => {
                    return try scope.pushInstruction(.{ .param_set = .{ .lhs = lhs_inst_index, .rhs = rhs_inst_index } });
                },
                .alloc => {
                    return try scope.pushInstruction(.{ .store = .{ .pointer = lhs_inst_index, .value = rhs_inst_index } });
                },
                .global_decl => {
                    return try scope.pushInstruction(.{ .global_set = .{ .lhs = lhs_inst_index, .rhs = rhs_inst_index } });
                },
                .get_element_pointer, .get_property_pointer => {
                    return try scope.pushInstruction(.{ .store = .{ .pointer = lhs_inst_index, .value = rhs_inst_index } });
                },
                else => {
                    self.tracer.panic("Unsupported instruction:", .{@tagName(lhs_inst)});
                },
            }

            // return try scope.pushInstruction(.{ .local_set = .{ .lhs = lhs_index, .rhs = rhs_inst } });
        },

        .identifier => {
            const slice = nav.getNodeSlice();
            const inst_index = scope.resolveSymbolRecursively(slice) orelse self.tracer.panic("Symbol not found:", .{slice});
            const inst: Hir.Inst = scope.builder.hir.insts.items[inst_index];
            switch (inst) {
                .param_decl => {
                    return try scope.pushInstruction(.{ .param_get = .{ .operand = inst_index } });
                },
                // .global_decl => {
                //     return try scope.pushInstruction(.{ .global_get = .{ .operand = inst_index } });
                // },
                .local => {
                    return try scope.pushInstruction(.{ .local_get = .{ .operand = inst_index } });
                },
                .alloc => {
                    return inst_index;
                    // return try scope.pushInstruction(.{ .load = .{ .operand = inst_index } });
                },
                else => {},
            }

            return inst_index;
        },
        .number_literal => {
            return try scope.pushInstruction(.{ .comptime_number = .{ .node = nav.node } });
        },
        .ret_expression => |ret_expr| {
            return try scope.pushInstruction(.{
                .ret = .{
                    .value = if (ret_expr.node == 0) null else try self.genLoadedInstruction(scope, ret_expr.node),
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
            const then_body_inst = try self.genBlockInstruction(scope, data.then_branch, null);

            // var if_expr = Hir.Inst.IfExpr{
            //     .cond = cond_inst,
            //     .then_body = then_body_inst,
            //     .else_body = null,
            // };
            var else_body_inst: ?Hir.Inst.Index = null;
            if (data.else_branch > 0) {
                else_body_inst = try self.genBlockInstruction(scope, data.else_branch, null);
                if (activeTag(self.hir.insts.items[then_body_inst]) == .inline_block and activeTag(self.hir.insts.items[else_body_inst.?]) == .inline_block) {
                    return try scope.pushInstruction(.{ .select_expr = .{
                        .cond = cond_inst,
                        .then_body = then_body_inst,
                        .else_body = else_body_inst,
                    } });
                }
            }

            return try scope.pushInstruction(.{ .if_expr = .{
                .cond = cond_inst,
                .then_body = then_body_inst,
                .else_body = else_body_inst,
            } });
        },

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
            // _ = try loop_scope.pushInstruction(.{ .br = .{ .operand = null, .target = loop_index } });
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
        .array_init => |array_init| {
            const items_list = self.hir.ast.interned_lists.getSlice(array_init.items_list);

            const type_inst = try self.genInstruction(scope, array_init.type);
            const inst_index = try scope.reserveInstruction();

            for (items_list, 0..) |item, i| {
                const item_inst = try self.genInstruction(scope, item);
                const index_inst = try scope.pushInstruction(.{ .constant_int = .{
                    .value = @intCast(i),
                } });
                const pointer_inst = try scope.pushInstruction(.{ .get_element_pointer = .{
                    .pointer = inst_index,
                    .index = index_inst,
                } });
                _ = try scope.pushInstruction(.{ .store = .{
                    .pointer = pointer_inst,
                    .value = item_inst,
                } });
            }

            self.setInstruction(inst_index, .{
                .alloc = .{
                    .type = type_inst,
                    .mutable = false,
                },
            });
            return inst_index;
        },
        .type_init => {
            const type_inst_index = try self.genTypeInstruction(scope, nav.data.type_init.type);
            const inst_index = try scope.reserveInstruction();

            const field_init_list = self.hir.ast.interned_lists.getSlice(nav.data.type_init.field_init_list);

            for (field_init_list) |field_init| {
                const node = self.hir.ast.getNode(field_init);
                const field_inst_index = try scope.pushInstruction(.{
                    .get_property_pointer = .{
                        .base = inst_index,
                        .property_name_node = node.data.field_init.name,
                    },
                });
                const field_inst = try self.genInstruction(scope, node.data.field_init.value);
                _ = try scope.pushInstruction(.{
                    .store = .{
                        .pointer = field_inst_index,
                        .value = field_inst,
                    },
                });

                // const field_inst = try self.genInstruction(scope, field_init.name);
                // const field_inst_index = try scope.pushInstruction(.{ .field_init = .{
                //     .base = inst_index,
                //     .field_name = field_inst,
                //     .value = try self.genLoadedInstruction(scope, field_init.value),
                // } });
            }

            // const type_inst = &self.hir.insts.items[type_inst_index];

            self.setInstruction(inst_index, .{
                .alloc = .{
                    .type = type_inst_index,
                    .mutable = false,
                },
            });
            return inst_index;
        },
        .array_prop_access => {
            const array_inst = try self.genInstruction(scope, nav.data.array_prop_access.lhs);
            const index_inst = try self.genLoadedInstruction(scope, nav.data.array_prop_access.rhs);
            // if (self.context == .assign_lhs) {
            return try scope.pushInstruction(.{ .get_element_pointer = .{
                .pointer = array_inst,
                .index = index_inst,
            } });
            // }
            // return try scope.pushInstruction(.{ .get_element_value = .{
            //     .pointer = array_inst,
            //     .index = index_inst,
            // } });
        },
        .prop_access => {
            const lhs_inst = try self.genInstruction(scope, nav.data.prop_access.lhs);
            // if (self.context == .assign_lhs) {
            return try scope.pushInstruction(.{ .get_property_pointer = .{
                .base = lhs_inst,
                .property_name_node = nav.data.prop_access.rhs,
            } });
            // }
            // return try scope.pushInstruction(.{ .get_property_value = .{
            //     .base = lhs_inst,
            //     .property_name = slice,
            // } });
        },

        .fn_call => {
            // const callee = try self.genInstruction(scope, nav.data.fn_call.callee);
            // const slice = nav.getNodeSlice();
            // const callee = scope.resolveSymbolRecursively(slice) orelse self.tracer.panic("Symbol not found", .{slice});
            const callee = try self.genInstruction(scope, nav.data.fn_call.callee);
            const args_iter = self.hir.ast.interned_lists.getSlice(nav.data.fn_call.args_list);
            const args_list: Hir.Inst.List = blk: {
                if (args_iter.len == 0) break :blk Hir.Inst.List.empty;
                var list = self.newList();
                for (args_iter) |arg| {
                    try list.append(try self.genLoadedInstruction(scope, arg));
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
                //                 .ty_number => return try scope.pushInstruction(.{ .ty_number = nav.node }),
                // .ty_boolean => return try scope.pushInstruction(.{ .ty_boolean = nav.node }),
                // .ty_i8 => return try scope.pushInstruction(.{ .ty_i8 = nav.node }),
                // .ty_i16 => return try scope.pushInstruction(.{ .ty_i16 = nav.node }),
                // .ty_i32 => return try scope.pushInstruction(.{ .ty_i32 = nav.node }),
                // .ty_i64 => return try scope.pushInstruction(.{ .ty_i64 = nav.node }),
                // .ty_f32 => return try scope.pushInstruction(.{ .ty_f32 = nav.node }),
                // .ty_f64 => return try scope.pushInstruction(.{ .ty_f64 = nav.node }),
                // .ty_void => return try scope.pushInstruction(.{ .ty_void = nav.node }),
                // .ty_usize => return try scope.pushInstruction(.{ .ty_usize = nav.node }),
                inline .ty_i8,
                .ty_i16,
                .ty_i32,
                .ty_i64,
                .ty_i128,
                .ty_i256,

                .ty_u8,
                .ty_u16,
                .ty_u32,
                .ty_u64,
                .ty_u128,
                .ty_u256,

                .ty_usize,

                .ty_f32,
                .ty_f64,
                .ty_void,
                => |tag| {
                    const tag_name = comptime @tagName(tag);
                    const data = @unionInit(Hir.Inst, tag_name, .{ .node = nav.node });
                    return try scope.pushInstruction(data);
                },

                .ty_array => {
                    const type_inst = try self.genInstruction(scope, nav.data.ty_array.type);
                    const size_inst = try self.genInstruction(scope, nav.data.ty_array.size_expr);

                    return try scope.pushInstruction(.{
                        .ty_array = .{
                            .type = type_inst,
                            .size = size_inst,
                        },
                    });
                },
                inline .add,
                .sub,
                .mul,
                .div,
                .gt,
                .lt,
                .ge,
                .le,
                .eq,
                .ne,
                => |tag| return try self.genBinaryExpression(tag, scope, nav.node),
                else => {},
            }
        },
    }
    std.debug.panic("unimplemented genInstruction {s}", .{@tagName(nav.tag)});
}

const Block = struct {
    inst: Hir.Inst.Index,
    is_inline: bool,
    scope: Scope,

    pub fn init(builder: *Self, parent_scope: *Scope) !Block {
        const inst_index = try parent_scope.reserveInstruction();
        // const is_inline = !nav.is(.block);
        return Block{
            .inst = inst_index,
            .is_inline = false,
            .scope = Scope.init(builder, parent_scope, .block, "block"),
        };
    }
    pub fn pushInstructionsFromBlock(self: *Block, node_index: Ast.Node.Index) !void {
        var nav = Ast.Navigator.init(self.scope.builder.hir.ast, node_index);
        const is_inline_block = !nav.is(.block);
        if (!is_inline_block) {
            const statements_iter = self.scope.builder.hir.ast.interned_lists.getSlice(nav.data.block.list);
            for (statements_iter) |child| {
                _ = try self.scope.builder.genInstruction(&self.scope, child);
            }
        } else {
            _ = try self.scope.builder.genInstruction(&self.scope, node_index);
        }
    }
    pub fn pushInstruction(self: *Block, inst: Hir.Inst) !Hir.Inst.Index {
        return try self.scope.pushInstruction(inst);
    }
    pub fn commit(self: *Block) !Hir.Inst.Index {
        const instructions = try self.scope.commit();
        self.scope.builder.setInstruction(self.inst, .{
            .block = .{
                .name_node = null,
                .instructions_list = instructions,
            },
        });
        return self.inst;
    }
};
pub fn genScopedBlockInstruction(self: *Self, scope: *Scope, node_index: Ast.Node.Index, break_index: ?Hir.Inst.Index) !Hir.Inst.Index {
    const event_id = self.tracer.beginEvent("genScopedBlockInstruction", .{node_index});
    defer self.tracer.endEvent(event_id, "genScopedBlockInstruction", .{node_index});
    self.logger.open("#{d} genScopedBlockInstruction", .{node_index});
    defer self.logger.close();
    const inst_index = try scope.builder.reserveInstruction();
    var nav = Ast.Navigator.init(self.hir.ast, node_index);
    const is_inline_block = !nav.is(.block);

    if (!is_inline_block) {
        const statements_iter = self.hir.ast.interned_lists.getSlice(nav.data.block.list);
        for (statements_iter) |child| {
            _ = try self.genInstruction(scope, child);
        }
    } else {
        _ = try self.genInstruction(scope, node_index);
    }

    if (break_index) |break_inst| {
        const br_inst = try scope.builder.pushInstruction(.{ .br = .{ .operand = null, .target = break_inst } });
        try scope.instructions.append(br_inst);
    }

    if (break_index == null and is_inline_block) {
        const br_inst = try scope.builder.pushInstruction(.{
            .br = .{
                .operand = if (scope.instructions.list.items.len > 0) scope.instructions.list.items[scope.instructions.list.items.len - 1] else null,
                .target = inst_index,
            },
        });
        try scope.instructions.append(br_inst);
    }
    const instructions = try scope.commit();
    assert.fmt(scope.parent != null, "blocks should always have a parent scope", .{});
    if (is_inline_block) {
        scope.builder.setInstruction(inst_index, .{
            .inline_block = .{
                .name_node = null,
                .instructions_list = instructions,
            },
        });
        return inst_index;
    }
    scope.builder.setInstruction(inst_index, .{
        .block = .{
            .name_node = null,
            .instructions_list = instructions,
        },
    });
    return inst_index;
}
pub fn genBlockInstruction(self: *Self, parent_scope: *Scope, node_index: Ast.Node.Index, break_index: ?Hir.Inst.Index) !Hir.Inst.Index {
    const event_id = self.tracer.beginEvent("genBlockInstruction", .{node_index});
    defer self.tracer.endEvent(event_id, "genBlockInstruction", .{node_index});
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
pub fn genStructDecl(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !Hir.Inst.Index {
    const event_id = self.tracer.beginEvent("genStructDecl", .{node_index});
    defer self.tracer.endEvent(event_id, "genStructDecl", .{node_index});
    self.logger.open("#{d} genStructDecl", .{node_index});
    defer self.logger.close();
    const index = try scope.reserveInstruction();
    const is_root = node_index == 0;
    const nav = Ast.Navigator.init(self.hir.ast, node_index);
    var wip_declarations_list = std.ArrayList(WipDef).init(self.arena.allocator());
    defer wip_declarations_list.deinit();

    var fields_list = self.newList();
    var decl_list = self.newList();

    const members_list = self.hir.ast.interned_lists.getSlice(nav.data.struct_decl.members_list);

    for (members_list) |child_index| {
        const child = self.hir.ast.getNode(child_index);
        switch (child.data) {
            .comment_line => {
                continue;
            },
            .struct_field => {
                if (is_root) {
                    self.logger.todo("error for: root struct declaration should not have fields", .{});
                }
                const field = child.data.struct_field;
                if (decl_list.len() > 0) {
                    self.logger.todo("error for: struct field declared after struct declaration", .{});
                }

                const inst_index = try self.reserveInstruction();

                const inline_block_inst = if (field.default_value != 0) try self.genBlockInstruction(scope, field.default_value, null) else null;
                const type_inst = if (field.type != 0) try self.genInstruction(scope, field.type) else null;
                self.setInstruction(inst_index, .{
                    .struct_field = .{
                        .name_node = field.name,
                        // .ty = if (field.type != 0) try self.genInstruction(scope, field.type) else null,
                        .type = type_inst,
                        .init = inline_block_inst,
                    },
                });
                try fields_list.append(inst_index);
            },
            else => {
                if (child_index == 0) self.tracer.panic("root struct declaration should not have fields", .{});
                const wip = try self.genDef(scope, child_index);
                try decl_list.append(wip.inst);
                try wip_declarations_list.append(wip);
            },
        }
    }

    for (wip_declarations_list.items) |def| {
        const inst = &self.hir.insts.items[def.inst];
        switch (inst.*) {
            .global_decl => {
                if (inst.global_decl.is_fn) {
                    const fn_decl_inst = try self.genFnDeclInstruction(
                        scope,
                        def.ty_node orelse unreachable,
                    );
                    self.hir.insts.items[def.inst].global_decl.init = fn_decl_inst.init;
                    self.hir.insts.items[def.inst].global_decl.type = fn_decl_inst.index;
                    continue;
                }

                if (def.init_node) |init_node| {
                    const inline_block = try self.genBlockInstruction(scope, init_node, null);
                    self.hir.insts.items[def.inst].global_decl.init = inline_block;
                }
                if (def.ty_node) |ty_node| {
                    const ty_inst = try self.genInstruction(scope, ty_node);
                    self.hir.insts.items[def.inst].global_decl.type = ty_inst;
                }
            },
            else => unreachable,
        }
    }

    self.setInstruction(index, .{
        .struct_decl = .{
            .name_node = null,
            .declarations_list = try decl_list.commit(),
            .fields_list = try fields_list.commit(),
        },
    });
    return index;
}
pub fn genInlineBlockInstruction(self: *Self, parent_scope: *Scope, node_index: Ast.Node.Index) !Hir.Inst.Index {
    const event_id = self.tracer.beginEvent("genInlineBlockInstruction", .{node_index});
    defer self.tracer.endEvent(event_id, "genInlineBlockInstruction", .{node_index});
    const index = try parent_scope.reserveInstruction();

    var scope = Scope.init(self, parent_scope, .block, "inline_block");
    const inst = try self.genInstruction(&scope, node_index);
    _ = try scope.pushInstruction(.{
        .br = .{
            .operand = inst,
            .target = index,
        },
    });
    const range = try scope.commit();
    self.setInstruction(index, .{
        .inline_block = .{
            .name_node = null,
            .instructions_list = range,
        },
    });
    return index;
}

pub fn genFnDeclInstruction(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !struct {
    index: Hir.Inst.Index,
    init: ?Hir.Inst.Index,
} {
    const event_id = self.tracer.beginEvent("genFnDeclInstruction", .{node_index});
    defer self.tracer.endEvent(event_id, "genFnDeclInstruction", .{node_index});
    self.logger.open("#{d} genFnDeclInstruction", .{node_index});
    defer self.logger.close();
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

        var block = try Block.init(self, scope);
        for (self.hir.lists.getSlice(params)) |param| {
            const name = self.hir.insts.items[param].param_decl.name_node;
            try block.scope.pushSymbol(self.hir.ast.getNodeSlice(name), param);
            // try block.pushInstruction(.{ .param_ = .{ .name_node = name, .type = self.hir.insts.items[param].param_decl.type } });
            try block.scope.instructions.append(param);
            // _ = try block.pushInstruction(param);
        }

        try block.pushInstructionsFromBlock(body);

        // const inst = try self.genScopedBlockInstruction(&fn_scope, body, null);

        // try scope.instructions.append(inst);
        break :blk try block.commit();
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

// pub fn genInlineBlock(self: *Self, parent_scope: *Scope, ty_node: ?Ast.Node.Index, init_node: ?Ast.Node.Index) !struct {
//     ty_inst: ?Hir.Inst.Index,
//     index: ?Hir.Inst.Index,
// } {
//     const event_id = self.tracer.beginEvent("genInlineBlock", .{ ty_node, init_node });
//     defer self.tracer.endEvent(event_id, "genInlineBlock", .{ ty_node, init_node });
//     self.logger.open("genInlineBlock", .{});
//     defer self.logger.close();

//     const ty_inst = if (ty_node) |node| try self.genInstruction(parent_scope, node) else null;
//     var scope = Scope.init(self, parent_scope, .block, "inline_block");

//     const init_node_index = init_node orelse return .{
//         .ty_inst = ty_inst,
//         .index = null,
//     };

//     const inst = try self.genInstruction(&scope, init_node_index);
//     if (ty_inst) |ty_inst_index| {
//         _ = try scope.pushInstruction(.{ .as = .{ .lhs = inst, .rhs = ty_inst_index } });
//     }
//     // break :init inst;
//     // break :init try scope.pushInstruction(.{ .undefined_value = null });
//     // };

//     // const br_inst = try scope.pushInstruction(.{
//     //     .br = .{
//     //         .operand = init_inst,
//     //     },
//     // });
//     const index = try scope.commit();
//     return .{
//         .ty_inst = ty_inst,
//         .index = try parent_scope.pushInstruction(.{
//             .inline_block = .{
//                 .name_node = null,
//                 .instructions_list = index,
//             },
//         }),
//     };
//     // const index = try self.reserveInstruction();
// }
pub fn genDef(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !WipDef {
    const event_id = self.tracer.beginEvent("genDef", .{node_index});
    defer self.tracer.endEvent(event_id, "genDef", .{node_index});
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
                .is_type = false,
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

    assert.fmt(ast.tag == .const_decl or ast.tag == .var_decl or ast.tag == .type_decl, "expected const_decl or var_decl or type_decl, got {}, '{s}'", .{ ast.tag, ast.getNodeSlice() });

    var global_decl = Hir.Inst.GlobalDecl{
        .name_node = undefined,
        .is_fn = false,
        .is_type = ast.tag == .type_decl,
        .visibility = visibility,
        .exported = is_export,
        .@"extern" = is_extern,
        .mutable = ast.is(.var_decl),
        .init = null,
        .type = null,
    };
    const decl = switch (ast.tag) {
        .const_decl => ast.data.const_decl,
        .var_decl => ast.data.var_decl,
        .type_decl => ast.data.type_decl,
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
    const event_id = self.tracer.beginEvent("pushInstruction", .{instruction});
    defer self.tracer.endEvent(event_id, "pushInstruction", .{instruction});
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
    const event_id = self.tracer.beginEvent("reserveInstruction", .{});
    defer self.tracer.endEvent(event_id, "reserveInstruction", .{});
    const index: Hir.Inst.Index = @intCast(self.hir.insts.items.len);
    try self.hir.insts.append(self.hir.allocator, undefined);
    return index;
}
pub fn setInstruction(self: *Self, index: Hir.Inst.Index, instruction: Hir.Inst) void {
    const event_id = self.tracer.beginEvent("setInstruction", .{ index, instruction });
    defer self.tracer.endEvent(event_id, "setInstruction", .{ index, instruction });
    self.hir.insts.items[index] = instruction;
}
pub fn genFnParams(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !Hir.InternedLists.Range {
    const event_id = self.tracer.beginEvent("genFnParams", .{node_index});
    defer self.tracer.endEvent(event_id, "genFnParams", .{node_index});
    var nav = Ast.Navigator.init(self.hir.ast, node_index);
    nav.assertTag(.fn_proto);
    var params = self.newList();
    const params_iter = self.hir.ast.interned_lists.getSlice(nav.data.fn_proto.params_list);

    for (params_iter) |child| {
        var nav_param = Ast.Navigator.init(self.hir.ast, child);
        // const name_inst = try scope.pushInstruction(.{ .decl_param =  });
        const ty = try self.genInstruction(scope.parent.?, nav_param.data.fn_param.type);
        const inst_index = try self.pushInstruction(
            .{
                .param_decl = .{
                    .name_node = nav_param.data.fn_param.name,
                    .type = ty,
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
// test "HirBuilder" {
//     const test_allocator = std.testing.allocator;
//     const file = try std.fs.cwd().openFile("./playground.zig", .{});
//     defer file.close();
//     const source = try file.readToEndAlloc(test_allocator, 1024 * 1024);
//     defer test_allocator.free(source);
//     // std.debug.print("source:\n\n{s}\n", .{source});

//     var errors = try ErrorManager.init(test_allocator);
//     defer errors.deinit();
//     var ast = try Ast.parse(test_allocator, &errors, source);
//     defer ast.deinit();
//     std.debug.print("AST:\n", .{});
//     try ast.format(std.io.getStdErr().writer().any(), 0, .{});

//     var hir = try Self.build(test_allocator, &ast, &errors);
//     defer hir.deinit();
//     try serializer.writeJSON([]Hir.Inst, std.io.getStdErr().writer().any(), hir.insts.items, .{
//         .lists = &hir.lists,
//     });

//     std.debug.print("Hir:\n{}\n", .{hir});
// }
