const std = @import("std");
const Ir = @import("Ir.zig");
const Ast = @import("Ast.zig");
const ErrorManager = @import("ErrorManager.zig");
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const HashMap = std.AutoArrayHashMapUnmanaged;
const InternedStrings = @import("InternedStrings.zig");
const InternedSlice = InternedStrings.InternedSlice;
const Self = @This();
const Inst = Ir.Inst;
const Def = Ir.Def;
const Type = Ir.Type;
const Scope = @import("irgen/Scope.zig");
const assert = @import("assert.zig");
ir: *Ir,
error_manager: *ErrorManager,
allocator: Allocator,
arena: ArenaAllocator,

type_map: HashMap(u64, Ir.Type.Index) = .{},

pub fn init(allocator: Allocator, ir: *Ir, error_manager: *ErrorManager) !Self {
    return .{
        .ir = ir,
        .error_manager = error_manager,
        .allocator = allocator,
        .arena = ArenaAllocator.init(allocator),
    };
}
pub fn deinit(self: *Self) void {
    self.arena.deinit();
}
pub fn pushType(self: *Self, ty: Ir.Type, hash: u64) !Ir.Type.Index {
    if (self.type_map.get(hash)) |index| return index;
    const index = Ir.Type.Index.fromArrayIndex(self.ir.types.items.len);
    try self.ir.types.append(self.allocator, ty);
    try self.type_map.put(self.arena.allocator(), hash, index);

    return index;
}

pub fn pushValue(self: *Self, value: Ir.Value) !Ir.Value.Index {
    const index = Ir.Value.Index.fromArrayIndex(self.ir.values.items.len);
    try self.ir.values.append(self.allocator, value);
    return index;
}
pub fn pushInst(self: *Self, inst: Inst) !Inst.Index {
    const index: Inst.Index = @intCast(self.ir.inst.items.len);
    try self.ir.inst.append(self.allocator, inst);
    return index;
}
pub fn newList(self: *Self) Ir.IrLists.List {
    return self.ir.lists.new(self.ir.allocator);
}
pub fn gen(self: *Self) !void {
    try self.genMod(null, 0);
}
pub fn getNodeTag(self: *Self, node_index: Ast.Node.Index) Ast.Node.Tag {
    return self.ir.ast.nodes.items(.tag)[node_index];
}
pub fn getNodeData(self: *Self, node_index: Ast.Node.Index) Ast.Node.Data {
    return self.ir.ast.nodes.items(.data)[node_index];
}

pub fn genMod(self: *Self, parent: ?*Scope, node_index: Ast.Node.Index) !void {
    var scope = try Scope.initMod(self, parent);
    const tag = self.getNodeTag(node_index);
    var wip_defs = scope.makeDefWipList();
    defer wip_defs.deinit();
    assert.fmt(tag == .root, "expected root node, got {}", .{tag});
    const data = self.getNodeData(node_index).children_list;
    var iter = self.ir.ast.node_lists.iterList(data);
    while (iter.next()) |child| {
        var wip = try self.genDefWip(&scope, child);
        try scope.appendWipDefinition(&wip);
        try wip_defs.append(wip);
    }

    for (wip_defs.items) |*wip| {
        try self.typeWip(&scope, wip);

        try self.initializeWip(&scope, wip);

        // std.debug.print("wip {any}\n", .{wip});
        // try scope.appendWipDefinition(wip);
    }

    // for (data.) |item| {
    // switch (tag) {
    //     .root => {

    //     },
    //     else => {
    //         std.debug.panic("unhandled mod tag: {}\n", .{tag});
    //     },
    // }
}
pub fn initializeWip(self: *Self, scope: *Scope, wip: *Scope.DefWip) !void {
    switch (wip.*) {
        .@"fn" => |*fn_wip| {
            const init_node = fn_wip.init_node orelse return;
            // fn_wip.init_scope.data.block.inst_list = self.newList();
            fn_wip.init_inst = try self.genBlockInstruction(&fn_wip.init_scope, init_node);

            // var ast = Ast.Navigator.init(self.ir.ast, init_node);
            // var init_scope: *Scope = &fn_wip.init_scope;
            // _ = init_scope; // autofix
            // init_scope.data.block.result =
            // init_scope.
            // fn_wip.init

        },
        else => {},
    }
    _ = scope; // autofix
}
const IrGenError = error{
    Unimplemented,
    OutOfMemory,
};
pub fn genInstructionLiterals(self: *Self, scope: *Scope, node_index: Ast.Node.Index) IrGenError!Inst.Index {
    _ = scope; // autofix
    const ast = Ast.Navigator.init(self.ir.ast, node_index);
    switch (ast.tag) {
        .number_literal => {
            const interned = try self.internNodeSlice(ast.node);
            const value = try self.pushValue(.{ .comptime_number = .{ .val = interned } });
            return try self.pushInst(.{ .constant = .{ .value = value, .ty = .number } });

            //   if (expected_ty == .number or expected_ty == null) {
            // const n_slice = self.ir.ast.getTokenSlice(ast.data.literal);
            // const n_value = std.fmt.parseFloat(f64, n_slice) catch return error.FailedToParseFloat;
            // scope.current_type = .number;

            // }
            // const n_slice = ast.getTokenSlice(data.literal);
            // const n_value = std.fmt.parseInt(i256, n_slice, 10) catch return error.FailedToParseInt;
            // scope.current_type = expected_ty orelse .int256;
            // const inst = try self.pushInst(.{ .local = .{ .value = try self.pushValue(.{ .big_number = .{ .val = n_value } }), .ty = .int256 } });
            // try scope.appendInstruction(inst);
            // return inst;
        },

        else => {
            return error.Unimplemented;
        },
    }
}
pub fn genInstruction(self: *Self, scope: *Scope, node_index: Ast.Node.Index) IrGenError!Inst.Index {
    const ast = Ast.Navigator.init(self.ir.ast, node_index);
    switch (ast.tag) {
        // .assign => {},
        .number_literal => return try self.genInstructionLiterals(scope, node_index),
        .block => {
            var block_scope = Scope.initBlock(self, scope);

            const block_inst = try self.genBlockInstruction(&block_scope, node_index);
            try scope.appendInstruction(block_inst);
            return block_inst;
        },
        else => {},
    }
    return 0;
}
pub fn genBlockInstruction(self: *Self, scope: *Scope, node_index: Ast.Node.Index) IrGenError!Inst.Index {
    var ast = Ast.Navigator.init(self.ir.ast, node_index);
    ast.assertTag(.block);
    scope.data.block.inst_list = self.newList();
    var statements_iter = self.ir.ast.node_lists.iterList(ast.data.children_list);
    while (statements_iter.next()) |child| {
        const inst = try self.genInstruction(scope, child);
        try scope.appendInstruction(inst);
    }
    const block_list_index = try scope.data.block.inst_list.?.commit();
    scope.data.block.inst_list = null;

    const block_inst = try self.pushInst(.{ .block = .{ .instructions = block_list_index } });
    return block_inst;
}
pub fn typeWip(self: *Self, scope: *Scope, wip: *Scope.DefWip) !void {
    switch (wip.*) {
        .@"fn" => |*_fn_wip| {
            var fn_wip: *Scope.FnDefWip = _fn_wip;
            var ast = Ast.Navigator.init(self.ir.ast, fn_wip.proto_node.?);
            ast.assertTag(.fn_proto);

            // var params_list = fn_wip.newParamsList();
            var iter = self.ir.ast.node_lists.iterList(ast.data.fn_proto.params_list);
            var params_len: usize = 0;
            var params_tuple = Ir.Type.Tuple.WipTuple{
                .items = self.ir.lists.new(self.allocator),
            };
            var init_scope = fn_wip.init_scope;
            while (iter.next()) |param_node| {
                params_len += 1;
                var param_ast = Ast.Navigator.init(self.ir.ast, param_node);
                param_ast.assertTag(.fn_param);

                const ty_index = try self.typeExpr(scope, param_ast.data.fn_param.ty);
                const interned = try self.internNodeSlice(param_ast.data.fn_param.name);

                try init_scope.appendDefinition(.{
                    .name = interned,
                    .ty = ty_index,
                    .is_local = true,
                });
                // TODO: store this in the scope
                try params_tuple.append(ty_index);
            }
            const params_ty_tuple = try params_tuple.commit();
            const params_ty_index = try self.pushType(
                .{ .tuple = params_ty_tuple.tuple },
                params_ty_tuple.hash,
            );
            init_scope.data.block.result = params_ty_index;
            const result_ty = try self.typeExpr(scope, ast.data.fn_proto.ret_ty);
            fn_wip.params_ty = params_ty_index;
            fn_wip.result_ty = result_ty;
            var hasher = std.hash.Wyhash.init(0);
            hasher.update("function");
            hasher.update(params_ty_index.hash());
            hasher.update(result_ty.hash());
            const fn_ty_hash = hasher.final();

            const fn_ty = Ir.Type{
                .function = .{
                    .params_len = @intCast(params_len),
                    .ret = result_ty,
                    .params_ty = params_ty_index,
                },
            };
            const fn_ty_index = try self.pushType(fn_ty, fn_ty_hash);
            fn_wip.ty = fn_ty_index;

            const def_index = fn_wip.definition_index orelse {
                std.debug.panic("expected definition index", .{});
            };
            self.ir.defs.items[def_index].ty = fn_ty_index;
        },
        .global => |*global_wip| {
            // if explicit type, set the type, otherwise it's unknown until the initializer is typed
            if (global_wip.ty_node) |ty_node| {
                const ty_index = try self.typeExpr(scope, ty_node);
                self.ir.defs.items[global_wip.definition_index.?].ty = ty_index;
            }
        },
    }
    // return try self.genDefWip(scope, node_index);
}
fn typeExpr(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !Ir.Type.Index {
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

pub fn internToken(self: *Self, token_index: Ast.Token.Index) !InternedSlice {
    const tags = self.ir.ast.tokens.items(.tag);
    assert(tags[token_index] == .identifier);
    return try self.ir.strings.intern(self.ir.ast.getTokenSlice(token_index));
}
pub fn internNodeSlice(self: *Self, node_index: Ast.Node.Index) !InternedSlice {
    const tag = self.ir.ast.getNodeTag(node_index);
    assert.fmt(tag == .identifier or tag == .number_literal or tag == .string_literal, "expected identifier or number_literal or string_literal, got {}", .{tag});
    const token = self.ir.ast.getNodeStartToken(node_index);
    const slice = self.ir.ast.getTokenSlice(token);
    return try self.ir.strings.intern(slice);
}
// Partially generated module definitions. Still untyped.
// We do this first to get all symbols into the symbol table, so in the typing phase we can
// resolve references to them.
pub fn genDefWip(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !Scope.DefWip {
    var ast = Ast.Navigator.init(self.ir.ast, node_index);
    const visibility: Def.Visibility = visib: {
        if (ast.acceptData(.@"pub")) |data| {
            ast.move(data.unary_expression);
            break :visib .public;
        }
        break :visib .private;
    };

    const is_extern: bool = is_extern: {
        if (ast.acceptData(.@"extern")) |data| {
            ast.move(data.unary_expression);
            break :is_extern true;
        }
        break :is_extern false;
    };

    const is_export: bool = is_export: {
        if (ast.acceptData(.@"export")) |data| {
            ast.move(data.unary_expression);
            break :is_export true;
        }
        break :is_export false;
    };

    if (ast.acceptData(.fn_decl)) |data| {
        _ = data; // autofix
        var wip = try self.genFnWip(scope, ast.node);
        wip.exported = is_export;
        wip.visibility = visibility;
        wip.external = is_extern;
        wip.mutable = false;

        return .{ .@"fn" = wip };
    }

    var wip = Scope.GlobalDefWip.init(scope);
    wip.exported = is_export;
    wip.visibility = visibility;
    wip.external = is_extern;
    wip.mutable = ast.is(.var_decl);

    assert.fmt(ast.is(.var_decl) or ast.is(.const_decl), "expected var_decl or const_decl, got {}", .{ast.tag});
    ast.move(ast.data.unary_expression);
    // var identifier_node: Ast.Node.Index = 0;
    if (ast.acceptData(.assign)) |data| {
        ast.move(data.binary_expression.lhs);
        wip.init_node = ast.node;
    }
    if (ast.acceptData(.ty_assign)) |data| {
        ast.move(data.binary_expression.lhs);
        wip.ty_node = data.binary_expression.rhs;
    }
    ast.assertTag(.identifier);
    wip.name = try self.internNodeSlice(ast.node);

    return .{ .global = wip };
}
pub fn genFnWip(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !Scope.FnDefWip {
    var ast = Ast.Navigator.init(self.ir.ast, node_index);
    ast.assertTag(.fn_decl);
    const data = ast.data.fn_decl;
    var wip = Scope.FnDefWip.init(scope);
    wip.init_node = if (data.body > 0) data.body else null;

    var ast_proto = ast.forkTo(data.proto);
    ast_proto.assertTag(.fn_proto);
    wip.proto_node = ast_proto.node;
    const proto_data = ast_proto.data.fn_proto;
    var ast_name = ast_proto.forkTo(proto_data.name);

    ast_name.assertTag(.identifier);
    wip.name = try self.internNodeSlice(ast_name.node);
    return wip;
}
test "irgen" {
    const test_allocator = std.testing.allocator;
    const file = try std.fs.cwd().openFile("./playground.zig", .{});
    defer file.close();
    const source = try file.readToEndAlloc(test_allocator, 1024 * 1024);
    defer test_allocator.free(source);

    // const source =
    //     \\export fn add(a: number, b: number): number {
    //     \\  const c = 2;
    //     \\  if (a > b) {
    //     \\    return a + b + c;
    //     \\  }
    //     \\  return a + b;
    //     \\}
    //     // \\
    //     // \\pub const a: number = 2;
    //     // \\const b = a ;
    //     // \\const a;
    //     // \\pub fn test(a: number): number {
    //     // \\  return a + 1;
    //     // \\}
    //     // \\
    // ;
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
