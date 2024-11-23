const Ast = @import("Ast.zig");
const AstGen = @import("AstGen.zig");
const IrGen = @This();
const std = @import("std");
const Allocator = std.mem.Allocator;
const PackedLists = @import("PackedLists.zig").new;
const Array = std.ArrayListUnmanaged;
const HashMap = std.AutoArrayHashMapUnmanaged;
const Ir = @import("Ir.zig");
const Inst = Ir.Inst;
const InternedStrings = @import("InternedStrings.zig");
const ErrorManager = @import("ErrorManager.zig");
const InstList = PackedLists(Inst, .{ .tag = .end });
const ArenaAllocator = std.heap.ArenaAllocator;
const InternedSlice = InternedStrings.InternedSlice;
const assert = std.debug.assert;

ir: *Ir,
error_manager: *ErrorManager,
allocator: Allocator,
arena: ArenaAllocator,

type_map: HashMap(u64, Ir.Type.Index) = .{},

const Scope = struct {
    parent: ?*Scope = null,
    definitions_table: HashMap(InternedSlice, Ir.Def.Index) = .{},
    instructions: ?*Ir.IrLists.List = null,
    name: InternedSlice = .{
        .start = 0,
        .end = 0,
    },
    current_type: Ir.Type.Index = .unknown,

    pub fn contains(self: *Scope, id: InternedSlice) bool {
        return self.definitions_table.contains(id);
    }
    pub fn get(self: *Scope, id: InternedSlice) ?Ir.Def.Index {
        return self.definitions_table.get(id);
    }
    pub fn recursiveGet(self: *Scope, id: InternedSlice) ?Ir.Def.Index {
        if (self.get(id)) |index| return index;
        if (self.parent) |parent| return parent.recursiveGet(id);
        return null;
    }
    pub fn put(self: *Scope, allocator: Allocator, id: InternedSlice, index: Ir.Def.Index) !void {
        try self.definitions_table.put(allocator, id, index);
    }

    pub fn appendInstruction(self: *Scope, inst: Inst.Index) !void {
        if (self.instructions) |instructions| {
            try instructions.append(inst);
        }
    }
    // pub fn hashSlice(self: *Scope, slice: []const u8) u64 {
    //     var hasher = std.hash.Wyhash.init(self.hash);
    //     hasher.update(slice);
    //     return hasher.final();
    // }
    // pub fn makeChildScope(self: *Scope, id: InternedSlice) Scope {
    //     var hasher = std.hash.Wyhash.init(self.hash);
    //     hasher.update(id.slice());
    //     return .{
    //         .parent = self,
    //         .hash = hasher.final(),
    //     };
    // }
};

pub fn init(allocator: Allocator, ir: *Ir, error_manager: *ErrorManager) !IrGen {
    return IrGen{
        .ir = ir,
        .error_manager = error_manager,
        .allocator = allocator,
        .arena = ArenaAllocator.init(allocator),
    };
}

pub fn deinit(self: *IrGen) void {
    self.arena.deinit();
}

const Block = struct {
    gen: *IrGen,
    inst: InstList.List,
    parent: *Block,
};

pub fn gen(self: *IrGen) !void {
    var global_scope = Scope{};
    try self.genMod(&global_scope, 0);
}
pub fn genMod(self: *IrGen, parent_scope: *Scope, node_index: Ast.Node.Index) !void {
    var scope = Scope{ .parent = parent_scope };
    const ast = self.ir.ast;
    // const node_tag = self.ir.ast.getNodeTag(node_index);
    const node_tags = ast.getNodeTags();
    assert(node_tags[node_index] == .root);
    const node_data = ast.getNodeData(node_index);
    var pending_defs = std.ArrayListUnmanaged(PendingDef){};
    var iter = ast.node_lists.iterList(node_data.children_list);
    const arena_allocator = self.arena.allocator();
    while (iter.next()) |child_index| {
        const pending_def = switch (node_tags[child_index]) {
            .@"pub",
            .@"export",
            .@"extern",
            .fn_decl,
            .var_decl,
            .const_decl,
            .assign,
            => try self.genDefinition(&scope, child_index),
            else => unreachable,
        };

        if (pending_def.def_index != null) try pending_defs.append(arena_allocator, pending_def);
        // const def = self.ir.defs.items[index];
        // try scope.put(arena_allocator, def.name, index);
    }

    for (pending_defs.items) |pending_def| {
        std.debug.print("pending_def: {any}\n", .{pending_def});
        const def_index = pending_def.def_index.?;

        const expected_ty = blk: {
            if (pending_def.ty_node) |ty_node| {
                const ty_index = try self.typeDefinition(&scope, ty_node);

                // std.debug.print("def_type: {}\n", .{ty_index});
                self.ir.defs.items[def_index].ty = ty_index;
                if (@intFromEnum(ty_index) < Ir.Type.Index.INDEX_START) {
                    break :blk ty_index;
                }
                const ty: Ir.Type = self.ir.types.items[ty_index.toArrayIndex()];
                switch (ty) {
                    .function => |func| {
                        break :blk func.ret;
                    },

                    else => {
                        unreachable;
                    },
                }
            }
            break :blk null;
        };

        if (pending_def.init_node) |init_node| {
            const init_inst = try self.genBlock(&scope, init_node, expected_ty);
            self.ir.defs.items[def_index].init = init_inst;
            if (self.ir.defs.items[def_index].ty == .unknown) {
                self.ir.defs.items[def_index].ty = scope.current_type;
            }
            // if
        }
    }
}
pub fn genLiteral(self: *IrGen, scope: *Scope, node_index: Ast.Node.Index, expected_ty: ?Ir.Type.Index) IrGenError!Inst.Index {
    const ast = self.ir.ast;
    const node_tags = ast.getNodeTags();
    const tag = node_tags[node_index];
    const data = ast.getNodeData(node_index);

    // if ()
    switch (tag) {
        .number_literal => {
            if (expected_ty == .number or expected_ty == null) {
                const n_slice = ast.getTokenSlice(data.literal);
                const n_value = std.fmt.parseFloat(f64, n_slice) catch return error.FailedToParseFloat;
                scope.current_type = expected_ty orelse .number;
                return try self.pushInst(.{ .local = .{ .value = try self.pushValue(.{ .number = .{ .val = n_value } }), .ty = .number } });
            }
            const n_slice = ast.getTokenSlice(data.literal);
            const n_value = std.fmt.parseInt(i256, n_slice, 10) catch return error.FailedToParseInt;
            scope.current_type = expected_ty orelse .int256;
            const inst = try self.pushInst(.{ .local = .{ .value = try self.pushValue(.{ .big_number = .{ .val = n_value } }), .ty = .int256 } });
            // try scope.appendInstruction(inst);
            return inst;
        },
        .identifier => {
            const str = self.ir.ast.getTokenSlice(data.literal);
            const interned_slice = try self.ir.strings.intern(str);

            std.debug.print("interned_slice: {s}\n", .{self.ir.strings.getSlice(interned_slice)});
            if (scope.recursiveGet(interned_slice)) |def_index| {
                std.debug.print("defindex: {}\n", .{def_index});
                const def = self.ir.defs.items[def_index];
                scope.current_type = def.ty;
                // def.ty
                const value = try self.pushValue(.{ .ref = .{ .index = def_index } });
                const inst = try self.pushInst(.{ .local = .{ .value = value, .ty = def.ty } });
                // try scope.appendInstruction(inst);
                return inst;
            }

            // return try self.genIdentifier(scope, node_index, expected_ty);

            return 0;
        },
        else => {
            unreachable;
        },
    }
}

const IrGenError = error{
    OutOfMemory,
    Unimplemented,
    FailedToParseFloat,
    FailedToParseInt,
};
pub fn genExpr(
    self: *IrGen,
    scope: *Scope,
    node_index: Ast.Node.Index,
    expected_ty: ?Ir.Type.Index,
) IrGenError!Inst.Index {
    const ast = self.ir.ast;
    const node_tags = ast.getNodeTags();
    const tag = node_tags[node_index];
    const node_data = ast.getNodeData(node_index);
    const data = node_data;

    // var instructions = self.ir.lists.new(self.allocator);
    switch (tag) {
        // .block => {},
        .identifier,
        .number_literal,
        => {
            return try self.genLiteral(scope, node_index, expected_ty);
            // try scope.appendInstruction(inst);
        },
        .block => return try self.genBlock(scope, node_index, expected_ty),
        // try scope.appendInstruction(inst);
        .ret_expression => {
            const ret_expr = data.ret_expression;
            const expr_inst = try self.genExpr(scope, ret_expr, expected_ty);

            try scope.appendInstruction(expr_inst);
            return try self.pushInst(.{ .ret = .{ .inst = expr_inst, .ty = scope.current_type } });
        },
        .add => {
            const lhs = try self.genExpr(scope, data.binary_expression.lhs, expected_ty);
            const rhs = try self.genExpr(scope, data.binary_expression.rhs, expected_ty);
            try scope.appendInstruction(lhs);
            try scope.appendInstruction(rhs);
            const inst = try self.pushInst(.{ .add = .{ .lhs = lhs, .rhs = rhs, .ty = scope.current_type } });
            // try scope.appendInstruction(inst);
            return inst;
        },
        .const_decl => return try self.genConstDecl(scope, node_index, expected_ty),
        .if_expr => return try self.genIfExpr(scope, node_index, expected_ty),
        // .literal_string => {
        //     std.debug.print("block: {}\n", .{tag});
        // },
        // .literal_boolean => {
        //     std.debug.print("block: {}\n", .{tag});
        // },

        // . => {
        //     std.debug.print("block: {}\n", .{tag});
        // },
        // .literal_array => {
        //     std.debug.print("block: {}\n", .{tag});
        // },
        else => {
            std.debug.print("init: {}\n", .{tag});
        },
    }
    return error.Unimplemented;
    // return error.unimplemented;
}
pub fn genBlock(self: *IrGen, parent_scope: *Scope, node_index: Ast.Node.Index, expected_ty: ?Ir.Type.Index) !Inst.Index {
    var instructions = self.ir.lists.new(self.allocator);
    var scope = Scope{
        .parent = parent_scope,
        .instructions = &instructions,
    };

    const tag = self.ir.ast.getNodeTag(node_index);
    switch (tag) {
        .block => {
            const data = self.ir.ast.getNodeData(node_index).children_list;
            var iter = self.ir.ast.node_lists.iterList(data);
            while (iter.next()) |child_index| {
                const inst = try self.genExpr(&scope, child_index, expected_ty);
                try scope.appendInstruction(inst);
                // try instructions.append(inst);
            }

            const list_index = try instructions.commit();
            const inst = try self.pushInst(.{ .block = .{ .instructions = list_index } });
            parent_scope.current_type = scope.current_type;
            // try parent_scope.appendInstruction(inst);
            return inst;
        },
        else => {
            const inst = try self.genExpr(&scope, node_index, expected_ty);
            try scope.appendInstruction(inst);
            const list_index = try instructions.commit();
            parent_scope.current_type = scope.current_type;
            // try scope.appendInstruction(inst);
            return try self.pushInst(.{ .inline_block = .{ .instructions = list_index } });
            // try parent_scope.appendInstruction(inst);
        },
    }
    // self.genExpr(&scope, node_index, expected_ty);

}

pub fn genIfExpr(self: *IrGen, scope: *Scope, node_index: Ast.Node.Index, expected_ty: ?Ir.Type.Index) !Inst.Index {
    _ = self; // autofix
    _ = scope; // autofix
    _ = node_index; // autofix
    _ = expected_ty; // autofix
    // return error.Unimplemented;
    return 0;
}
pub fn genConstDecl(self: *IrGen, scope: *Scope, node_index: Ast.Node.Index, expected_ty: ?Ir.Type.Index) !Inst.Index {
    const pending_def = try self.genDefinition(scope, node_index);
    const ty = blk: {
        if (pending_def.ty_node) |ty_node| break :blk try self.typeDefinition(scope, ty_node);
        break :blk null;
    };
    const def_index = pending_def.def_index.?;
    _ = ty; // autofix
    std.debug.print("pending_def: {any}\n", .{pending_def});
    if (pending_def.init_node) |init_node| {
        const init_inst = try self.genBlock(scope, init_node, expected_ty);
        self.ir.defs.items[def_index].init = init_inst;
        if (self.ir.defs.items[def_index].ty == .unknown) {
            self.ir.defs.items[def_index].ty = scope.current_type;
        }
        return init_inst;
        // if
    }
    // try self.genInit(scope, init_node, expected_ty);

    // return error.Unimplemented;
    return 0;
}
// pub fn genInit(self: *IrGen, scope: *Scope, node_index: Ast.Node.Index, expected_ty: ?Ir.Type.Index) !void {
//     _ = expected_ty; // autofix
//     const ast = self.ir.ast;
//     const node_tags = ast.getNodeTags();
//     const tag = node_tags[node_index];
//     const node_data = ast.getNodeData(node_index);
//     const data = node_data;

//     _ = data; // autofix

//     switch (tag) {
//         // .block => {},
//         .number_literal => {
//             std.debug.print("number_literal: {}\n", .{tag});
//         },
//         // .literal_string => {
//         //     std.debug.print("block: {}\n", .{tag});
//         // },
//         // .literal_boolean => {
//         //     std.debug.print("block: {}\n", .{tag});
//         // },

//         // . => {
//         //     std.debug.print("block: {}\n", .{tag});
//         // },
//         // .literal_array => {
//         //     std.debug.print("block: {}\n", .{tag});
//         // },
//         else => {
//             std.debug.print("init: {}\n", .{tag});
//         },
//     }
//     _ = scope; // autofix
// }
pub fn pushType(self: *IrGen, ty: Ir.Type, hash: u64) !Ir.Type.Index {
    if (self.type_map.get(hash)) |index| return index;
    const index = Ir.Type.Index.fromArrayIndex(self.ir.types.items.len);
    try self.ir.types.append(self.allocator, ty);
    try self.type_map.put(self.arena.allocator(), hash, index);

    return index;
}

pub fn pushValue(self: *IrGen, value: Ir.Value) !Ir.Value.Index {
    const index = Ir.Value.Index.fromArrayIndex(self.ir.values.items.len);
    try self.ir.values.append(self.allocator, value);
    return index;
}
pub fn pushInst(self: *IrGen, inst: Inst) !Inst.Index {
    const index: Inst.Index = @intCast(self.ir.inst.items.len);
    try self.ir.inst.append(self.allocator, inst);
    return index;
}
pub fn typeDefinition(self: *IrGen, scope: *Scope, node_index: Ast.Node.Index) !Ir.Type.Index {
    const tags = self.ir.ast.getNodeTags();

    if (tags[node_index] == .fn_proto) return try self.typeFnDef(scope, node_index);
    // if (tags[node_index] == .assign) {
    //     const data = self.ir.ast.getNodeData(node_index).binary_expression;
    //     std.debug.print("data: {any}\n", .{data});
    return try self.typeExplicit(scope, node_index);
    // }

    // std.debug.print("def tag: {s}\n", .{@tagName(tags[node_index])}); return Ir.Type.Index.unknown;
    // assert(tags[node_index] == .fn_proto or tags[node_index] == );
}

pub fn typeExplicit(self: *IrGen, scope: *Scope, node_index: Ast.Node.Index) !Ir.Type.Index {
    const ast = self.ir.ast;
    const tag = self.ir.ast.getNodeTag(node_index);
    std.debug.print("tag: {s}\n", .{@tagName(tag)});
    switch (tag) {
        .ty_number => return Ir.Type.Index.number,
        .ty_boolean => return Ir.Type.Index.boolean,
        .ty_string => return Ir.Type.Index.string,
        .ty_void => return Ir.Type.Index.void,
        .ty_generic => {
            const data = self.ir.ast.getNodeData(node_index).ty_generic;
            const slice = ast.getTokenSlice(ast.getNodeStartToken(data.name));
            std.debug.print("slice: {s}\n", .{slice});
            if (std.mem.eql(u8, slice, "Option")) {

                // const ty: Ir.Type = .{
                //     .option = .{ .child = try self.typeExplicit(scope, data.args[0]) },
                // };
                // return try self.pushType(ty);
                // return try self.typeExplicit(scope, data.args[0]);
            }
            // return error.unimplemented;
            // const data = self.ir.ast.getNodeData(node_index).unary_expression;
            // const data = self.ir.ast.getNodeData(node_index).binary_expression;
            // return try self.typeExplicit(scope, data.lhs);
        },
        else => {},
    }
    _ = scope; // autofix
    return Ir.Type.Index.unknown;
}
pub fn typeFnDef(self: *IrGen, scope: *Scope, node_index: Ast.Node.Index) !Ir.Type.Index {
    const ast = self.ir.ast;
    assert(ast.getNodeTag(node_index) == .fn_proto);

    const data = ast.getNodeData(node_index).fn_proto;

    var params_tuple = Ir.Type.Tuple.WipTuple{
        .items = self.ir.lists.new(self.allocator),
    };
    var iter_params = ast.node_lists.iterList(data.params_list);

    var params_len: usize = 0;
    while (iter_params.next()) |param_node_index| {
        params_len += 1;
        const param_node = ast.getNodeData(param_node_index).fn_param;
        std.debug.print("param_node: {any}\n", .{param_node});
        const param_ty = try self.typeExplicit(scope, param_node.ty);
        std.debug.print("param_ty: {}\n", .{param_ty});
        try params_tuple.append(param_ty);

        // _ = param_ty; // autofix
        // std.debug.print("param_ty: {}\n", .{param_ty});
    }

    const params_ty = try params_tuple.commit();
    const params_ty_index = try self.pushType(.{ .tuple = params_ty.tuple }, params_ty.hash);
    std.debug.print("params_ty_index: {}\n", .{params_ty_index});

    const ret_ty = try self.typeExplicit(scope, data.ret_ty);

    var hasher = std.hash.Wyhash.init(0);
    hasher.update("function");
    hasher.update(params_ty_index.hash());
    hasher.update(ret_ty.hash());
    const fn_ty_hash = hasher.final();

    const fn_ty = Ir.Type{
        .function = .{
            .params_len = @intCast(params_len),
            .ret = ret_ty,
            .params_ty = params_ty_index,
        },
    };
    const fn_ty_index = try self.pushType(fn_ty, fn_ty_hash);

    // const ret_ty_index = try self.pushType(.{ .tuple = ret_ty.tuple }, ret_ty.hash);
    std.debug.print("ret_ty: {} {}\n", .{ fn_ty_index, fn_ty_hash });
    return fn_ty_index;
}
pub fn pushDefinition(self: *IrGen, def: Ir.Def) !Ir.Def.Index {
    const index: Ir.Def.Index = @intCast(self.ir.defs.items.len);
    try self.ir.defs.append(self.allocator, def);
    return index;
}
const PendingDef = struct {
    init_node: ?Ast.Node.Index = null,
    ty_node: ?Ast.Node.Index = null,
    def_index: ?Ir.Def.Index = null,
};

fn genDefinition(self: *IrGen, scope: *Scope, _node_index: Ast.Node.Index) !PendingDef {
    const ast = self.ir.ast;
    const node_tags = self.ir.ast.getNodeTags();
    var node_index = _node_index;

    const visibility: Ir.Def.Visibility = blk: {
        switch (node_tags[node_index]) {
            .@"pub" => {
                node_index = ast.getNodeData(node_index).unary_expression;
                break :blk .public;
            },
            else => break :blk .private,
        }
    };

    const is_extern: bool = blk: {
        switch (node_tags[node_index]) {
            .@"extern" => {
                node_index = ast.getNodeData(node_index).unary_expression;
                break :blk true;
            },
            else => break :blk false,
        }
    };

    const is_export: bool = blk: {
        switch (node_tags[node_index]) {
            .@"export" => {
                node_index = ast.getNodeData(node_index).unary_expression;
                break :blk true;
            },
            else => break :blk false,
        }
    };

    const mutable = switch (node_tags[node_index]) {
        .var_decl => true,
        else => false,
    };
    var def: Ir.Def = .{
        .visibility = visibility,
        .exported = is_export,
        .external = is_extern,
        .mutable = mutable,
        .name = undefined,
        .ty = Ir.Type.Index.unknown,
        .init = null,
    };

    switch (node_tags[node_index]) {
        .fn_decl => {
            return try self.genFnDef(scope, node_index, def);
        },
        .const_decl, .var_decl => {
            node_index = ast.getNodeData(node_index).unary_expression;
        },
        .assign => {
            try self.error_manager.addError(.{
                .tag = .expected_const_or_var,
                .start = ast.getNodeStartToken(node_index),
                .end = ast.getNodeEndToken(node_index),
                .payload = node_index,
            });
        },
        else => {
            unreachable;
        },
    }

    const init_node: ?Ast.Node.Index = blk: {
        switch (node_tags[node_index]) {
            .assign => {
                const data = ast.getNodeData(node_index).binary_expression;
                node_index = data.lhs;
                break :blk data.rhs;
            },
            else => break :blk null,
        }
    };

    if (init_node == null and !is_extern) {
        try self.error_manager.addError(.{
            .tag = .expected_init_value,
            .start = ast.getNodeStartToken(node_index),
            .end = ast.getNodeEndToken(node_index),
            .payload = node_index,
        });
    }
    const ty_node: ?Ast.Node.Index = blk: {
        switch (node_tags[node_index]) {
            .identifier => {
                try self.error_manager.addError(.{
                    .tag = .expected_explicit_type,
                    .start = ast.getNodeStartToken(node_index),
                    .end = ast.getNodeEndToken(node_index),
                    .payload = node_index,
                });
                break :blk null;
            },
            .ty_assign => {
                const data = ast.getNodeData(node_index).binary_expression;
                node_index = data.lhs;
                break :blk data.rhs;
            },
            else => break :blk null,
        }
    };

    switch (node_tags[node_index]) {
        .identifier => {},
        else => {
            try self.error_manager.addError(.{
                .tag = .expected_identifier,
                .start = ast.getNodeStartToken(node_index),
                .end = ast.getNodeEndToken(node_index),
                .payload = node_index,
            });
            return .{
                .init_node = null,
                .ty_node = null,
                .def_index = null,
            };
        },
    }

    const name_slice = ast.getTokenSlice(ast.getNodeStartToken(node_index));
    def.name = try self.ir.strings.intern(name_slice);
    // std.debug.print("name_slice: {s}\n", .{self.ir.strings.getSlice(def.name)});
    // def.hash = scope.hashSlice(name_slice);
    const pending_def: PendingDef = .{
        .init_node = init_node,
        .ty_node = ty_node,
        .def_index = try self.pushDefinition(def),
    };
    if (scope.contains(def.name)) {
        try self.error_manager.addError(.{
            .tag = .duplicate_definition,
            .start = ast.getNodeStartToken(node_index),
            .end = ast.getNodeEndToken(node_index),
            .payload = node_index,
        });
    } else {
        // const index = try self.pushDefinition(def);
        try scope.put(self.arena.allocator(), def.name, pending_def.def_index.?);
    }

    return pending_def;
}

fn genFnDef(self: *IrGen, scope: *Scope, node_index: Ast.Node.Index, _def: Ir.Def) !PendingDef {
    var def = _def;

    const ast = self.ir.ast;
    const node_tags = ast.getNodeTags();
    const node_data = ast.getNodeData(node_index);
    const data = node_data.fn_decl;

    const proto_node = data.proto;
    assert(proto_node != 0);

    const proto_node_tag = node_tags[proto_node];
    assert(proto_node_tag == .fn_proto);
    const proto_data = ast.getNodeData(proto_node).fn_proto;

    const name_node = proto_data.name;
    assert(name_node != 0);
    assert(node_tags[name_node] == .identifier);

    const name = ast.getNodeStartToken(name_node);
    const name_slice = ast.getTokenSlice(name);
    def.name = try self.ir.strings.intern(name_slice);
    // def.hash = scope.hashSlice(name_slice);

    const def_index = blk: {
        if (scope.contains(def.name)) {
            try self.error_manager.addError(.{
                .tag = .duplicate_definition,
                .start = ast.getNodeStartToken(name_node),
                .end = ast.getNodeEndToken(name_node),
                .payload = name_node,
            });
            break :blk null;
        } else {
            const index = try self.pushDefinition(def);
            try scope.put(
                self.arena.allocator(),
                def.name,
                index,
            );
            break :blk index;
        }
    };

    return .{
        .init_node = data.body,
        .ty_node = proto_node,
        .def_index = def_index,
    };
}
test "irgen" {
    const test_allocator = std.testing.allocator;
    const source =
        \\export fn add(a: number, b: number): number {
        \\  const c = 2;
        \\  if (a > b) {
        \\    return a + b + c;
        \\  }
        \\  return a + b;
        \\}
        // \\
        // \\pub const a: number = 2;
        // \\const b = a ;
        // \\const a;
        // \\pub fn test(a: number): number {
        // \\  return a + 1;
        // \\}
        // \\
    ;
    var errors = try ErrorManager.init(test_allocator);
    defer errors.deinit();
    var ast = try Ast.parse(test_allocator, &errors, source);
    std.debug.print("source:\n\n{s}\n", .{source});
    defer ast.deinit();
    var ir = try Ir.gen(test_allocator, &ast, &errors);
    defer ir.deinit();

    std.debug.print("AST:\n", .{});
    try ast.format(std.io.getStdErr().writer().any(), 0, .{});
    std.debug.print("IR:\n", .{});
    try ir.format(std.io.getStdErr().writer().any(), .{});
}
