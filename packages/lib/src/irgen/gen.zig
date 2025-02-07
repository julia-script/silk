const Ast = @import("../Ast.zig");
const Visibility = @import("../shared.zig").Visibility;
const std = @import("std");
const Module = @import("../ir/Module.zig");
const ErrorManager = @import("../ErrorManager.zig");
const Allocator = std.mem.Allocator;
const debug = @import("../debug.zig");
const Scope = @import("./scopes.zig").Scope;

mod: *Module,
ast: *Ast,
allocator: Allocator,

const Self = @This();
pub inline fn gen(allocator: Allocator, ast: *Ast) !Module {
    var module = Module.init(allocator);
    var self = Self{
        .mod = &module,
        .ast = ast,
        .allocator = allocator,
    };
    try self.parseRoot();

    return module;
}

pub fn parseRoot(self: *Self) !void {
    var scope = try self.collectNamespaceSymbols(null, 0);
    defer scope.deinit();
    const scope_ptr = &scope;
    try self.parseStructDecl(scope_ptr);
    try self.parseStructDef(scope_ptr);
}
pub fn parse(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !void {
    _ = scope; // autofix
    const node = self.ast.getNodeTag(node_index);

    switch (node) {
        // noop
        .comment_line => {},

        else => {
            std.debug.panic(
                "Can't parse node {d}, unhandled node type: .{s}\n",
                .{
                    node_index,
                    @tagName(node),
                },
            );
        },
    }
}

pub fn collectMemberDecls(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !void {
    const node = self.ast.getNode(node_index);

    switch (node.data) {
        // noop
        .comment_line => {},
        .fn_decl,
        .var_decl,
        .const_decl,
        .type_decl,
        .@"pub",
        .@"export",
        .@"extern",
        => try self.collectDecl(scope, node_index),
        else => {
            std.debug.panic(
                "Can't parse member decl {d}, unhandled node type: .{s}\n",
                .{ node_index, @tagName(node.data) },
            );
        },
    }
}

pub fn collectDecl(self: *Self, scope: *Scope, node_index: Ast.Node.Index) !void {
    const node = self.ast.getNode(node_index);

    var index = node_index;

    var is_exported = false;
    var visibility: Visibility = .private;
    if (self.ast.nodeIf(index, .@"export")) |export_node| {
        is_exported = true;
        index = export_node.data.@"export".node;
    }

    if (self.ast.nodeIf(index, .@"pub")) |pub_node| {
        visibility = .public;
        index = pub_node.data.@"pub".node;
    }

    if (self.ast.nodeIf(index, .fn_decl)) |fn_node| {
        const ref = try self.mod.decls.reserve();
        const fn_scope = try Scope.initFunctionDeclaration(
            self.allocator,
            self.mod,
            scope,
            ref,
            fn_node.data.fn_decl.proto,
            if (fn_node.data.fn_decl.body > 0) fn_node.data.fn_decl.body else null,
            visibility,
            is_exported,
        );
        const proto_data = self.ast.getNode(fn_node.data.fn_decl.proto);
        const name = self.ast.getNodeSlice(proto_data.data.fn_proto.name);
        try scope.namespace.symbols_table.put(name, .{ .global = ref });
        std.debug.print("name: {*} {d}\n", .{ &scope.namespace, scope.namespace.symbols_table.count() });
        try scope.namespace.member_scopes.put(name, fn_scope);
        return;

        // return try self.parseFnDecl(scope, index, visibility, is_exported);
    }

    if (self.ast.nodeIs(index, .var_decl) or self.ast.nodeIs(index, .const_decl)) {
        std.debug.panic("var_decl or const_decl not implemented", .{});
    }

    if (self.ast.nodeIs(index, .type_decl)) {
        std.debug.panic("type_decl not implemented", .{});
    }

    std.debug.panic("Unexpected token: '{s}'", .{@tagName(node.data)});
}

pub fn parseTypeDecl(self: *Self, scope: *Scope, node_index: Ast.Node.Index, visibility: Visibility, is_exported: bool) !void {
    _ = scope; // autofix
    _ = visibility; // autofix
    _ = is_exported; // autofix
    const node = self.ast.getNode(node_index);
    const data = node.data.type_decl;
    _ = data; // autofix
}
pub fn parseGlobalDecl(self: *Self, scope: *Scope, node_index: Ast.Node.Index, visibility: Visibility, is_exported: bool) !void {
    _ = scope; // autofix
    _ = visibility; // autofix
    _ = is_exported; // autofix
    const node = self.ast.getNode(node_index);
    const data = node.data.var_decl;
    _ = data; // autofix
}
pub fn parseFnDecl(self: *Self, scope: *Scope) !void {
    const proto_data = self.ast.getNode(scope.fn_decl.proto_node);
    const name = self.ast.getNodeSlice(proto_data.data.fn_proto.name);
    var signature = Module.Signature.init(self.allocator);

    const param_nodes = self.ast.getList(proto_data.data.fn_proto.params_list);
    // scope.fn_decl.definition_builder = try Module.DefinitionBuilder.init(self.allocator, self.mod, .{ .global_body = {} });
    for (param_nodes) |param_node| {
        const fn_param = self.ast.getNode(param_node).data.fn_param;

        const param_type = try self.parseTypeExpression(
            scope,
            &scope.fn_decl.definition_builder,
            fn_param.type,
        );
        // const param_name = self.ast.getNodeSlice(fn_param.name);
        // _ = param_name; // autofix

        try signature.addParam(param_type, false);
    }

    const ret_type = try self.parseTypeExpression(
        scope,
        &scope.fn_decl.definition_builder,
        proto_data.data.fn_proto.return_type,
    );
    signature.setReturn(ret_type);

    const signature_ref = try self.mod.signatures.append(signature);

    const fn_decl_ref = try self.mod.setFunctionDeclaration(
        scope.fn_decl.ref,
        scope.fn_decl.parent.namespace.ref,
        name,
        switch (scope.fn_decl.visibility) {
            .public => .Export,
            .private => .Local,
        },
        signature_ref,
    );
    _ = fn_decl_ref; // autofix
}
pub fn parseTypeExpression(self: *Self, scope: *Scope, builder: *Module.DefinitionBuilder, node_index: Ast.Node.Index) !Module.Ty {
    const typeValue = try self.parseExpression(scope, builder, node_index);
    switch (typeValue) {
        .immediate => |immediate| {
            if (immediate.ty == .type) {
                return immediate.data.ty;
            }
        },
        else => {},
    }
    std.debug.panic("Unexpected type value: '{}'", .{typeValue});
}

pub fn parseExpression(self: *Self, scope: *Scope, builder: *Module.DefinitionBuilder, node_index: Ast.Node.Index) anyerror!Module.Value {
    const node = self.ast.getNode(node_index);

    switch (node.data) {
        // Parse easy ones first
        .ty_f32 => return Module.Value.ImmTy(.f32),
        .ty_f64 => return Module.Value.ImmTy(.f64),

        .ty_i8 => return Module.Value.ImmTy(.i8),
        .ty_i16 => return Module.Value.ImmTy(.i16),
        .ty_i32 => return Module.Value.ImmTy(.i32),
        .ty_i64 => return Module.Value.ImmTy(.i64),

        .ty_u8 => return Module.Value.ImmTy(.u8),
        .ty_u16 => return Module.Value.ImmTy(.u16),
        .ty_u32 => return Module.Value.ImmTy(.u32),
        .ty_u64 => return Module.Value.ImmTy(.u64),

        .ty_boolean => return Module.Value.ImmTy(.bool),
        .ty_void => return Module.Value.ImmTy(.void),
        .ty_type => return Module.Value.ImmTy(.type),
        .number_literal => |number_literal| {
            const slice = self.ast.getTokenSlice(number_literal.token);
            if (std.mem.containsAtLeast(u8, slice, 1, ".")) {
                const value = try std.fmt.parseFloat(f64, slice);
                return Module.Value.Const(.float, value);
            } else {
                const value = try std.fmt.parseInt(i64, slice, 10);
                return Module.Value.Const(.int, value);
            }
        },
        // .comment_line => {
        //     return null;
        // },
        .gt,
        .ge,
        .lt,
        .le,
        .eq,
        .ne,
        => |bin_expr| {
            const left = try self.parseExpression(scope, builder, bin_expr.lhs);
            const right = try self.parseExpression(scope, builder, bin_expr.rhs);
            const ty = left.getTy();

            switch (node.data) {
                .gt => return try builder.gt(ty, left, right, false),
                .ge => return try builder.ge(ty, left, right, false),
                .lt => return try builder.lt(ty, left, right, false),
                .le => return try builder.le(ty, left, right, false),
                .eq => return try builder.eq(ty, left, right, false),
                .ne => return try builder.ne(ty, left, right, false),
                else => {
                    unreachable;
                },
            }
        },
        .add,
        .sub,
        .mul,
        => |bin_expr| {
            const left = try self.parseExpression(scope, builder, bin_expr.lhs);
            const right = try self.parseExpression(scope, builder, bin_expr.rhs);
            const ty = left.getTy();

            switch (node.data) {
                .add => return try builder.add(ty, left, right, false),
                .sub => return try builder.sub(ty, left, right, false),
                .mul => return try builder.mul(ty, left, right, false),
                else => {
                    unreachable;
                },
            }
        },
        .identifier => |identifier| {
            const name = self.ast.getTokenSlice(identifier.token);
            const symbol = scope.getSymbolRecursive(name) orelse {
                std.debug.panic("Symbol not found: '{s}'", .{name});
            };
            switch (symbol) {
                .global => |global| {
                    return scope.block.definition_builder.useGlobal(self.mod, global, false);
                },
                .local => |local| {
                    return scope.block.definition_builder.useLocal(local);
                },
            }
            //     _ = identifier; // autofix
            //     // const name = self.ast.getNodeSlice(identifier.name);
            //     // _ = name; // autofix
            //     // return try builder.load(ty, name);

        },
        .block => |block| {
            const statements = self.ast.getList(block.list);
            for (statements) |statement| {
                try self.parseStatement(scope, builder, statement);
            }
            return Module.Value.Imm(.void, .void);
        },

        else => {
            std.debug.panic("Unexpected token: '{s}'", .{@tagName(node.data)});
        },
    }
}
pub inline fn collectNamespaceSymbols(self: *Self, parent_scope: ?*Scope, node_index: Ast.Node.Index) !Scope {
    const node = self.ast.getNode(node_index);
    const data = node.data.struct_decl;
    const members = self.ast.getList(data.members_list);
    const namespace = try self.mod.declareNamespace("struct");
    var scope = Scope.initNamespace(self.allocator, parent_scope, namespace);

    // Collect phase creates the scopes for each member and collect their symbols, but doesn't parse them.
    // the reason being that we want to be able to hoist declarations up in case they depend on each other
    // so when we do parse them, we can know if they exist or not.
    for (members) |member_index| {
        try self.collectMemberDecls(&scope, member_index);
    }
    return scope;
}

pub fn parseStructDecl(self: *Self, scope: *Scope) !void {
    var iter = scope.namespace.member_scopes.iterator();
    while (iter.next()) |entry| {
        const member_scope = entry.value_ptr;
        switch (member_scope.*) {
            .fn_decl => {
                try self.parseFnDecl(member_scope);
            },
            else => {
                std.debug.panic("Unexpected member scope: '{s}'", .{@tagName(member_scope.*)});
            },
        }
        // try self.parseDecl(&member_scope);
    }
}

pub fn parseStructDef(self: *Self, scope: *Scope) !void {
    var iter = scope.namespace.member_scopes.iterator();
    while (iter.next()) |entry| {
        const member_scope = entry.value_ptr;
        switch (member_scope.*) {
            .fn_decl => {
                try self.parseFnDef(member_scope);
                member_scope.deinit();
            },
            else => {
                std.debug.panic("Unexpected member scope: '{s}'", .{@tagName(member_scope.*)});
            },
        }
    }
}

pub fn parseFnDef(self: *Self, scope: *Scope) !void {
    const body_node_index = scope.fn_decl.body_node orelse return;
    const body_node = self.ast.getNode(body_node_index);
    _ = body_node; // autofix
    // scope.fn_decl.definition_builder = try Module.DefinitionBuilder.init(
    //     self.allocator,
    //     self.mod,
    //     .{ .function_body = scope.fn_decl.ref },
    // );

    scope.fn_decl.definition_builder.kind = .{ .function_body = scope.fn_decl.ref };
    const fn_decl = self.mod.getFunctionDeclaration(scope.fn_decl.ref);
    const signature = self.mod.getSignature(fn_decl.signature);
    var dfg = scope.fn_decl.definition_builder.dfg;
    for (signature.params.items, 0..) |param, i| {
        const local = try dfg.pushLocal(param.ty, true, param.is_comptime);
        const param_name = scope.fn_decl.getParamName(@intCast(i), self.ast);
        try scope.fn_decl.params.put(param_name, .{ .local = local });
    }
    try self.parseEntrypoint(scope, &scope.fn_decl.definition_builder, body_node_index);

    _ = try scope.fn_decl.definition_builder.commit();
}

pub fn parseEntrypoint(self: *Self, parent_scope: *Scope, builder: *Module.DefinitionBuilder, node_index: Ast.Node.Index) !void {
    const node = self.ast.getNode(node_index);
    const block = switch (node.data) {
        .block => node.data.block,
        else => {
            std.debug.panic("Unexpected token: '{s}'", .{@tagName(node.data)});
        },
    };
    var scope = Scope.initBlock(self.allocator, parent_scope, builder);
    defer scope.deinit();

    const statements = self.ast.getList(block.list);
    for (statements) |statement| {
        try self.parseStatement(&scope, builder, statement);
    }
    // switch (node.data) {
    //     .block => {
    //         const block_node = node.data.block;
    //         _ = block_node; // autofix
    //     },
    //     else => {
    //         std.debug.panic("Unexpected token: '{s}'", .{@tagName(node.data)});
    //     },
    // }
}

pub fn parseStatement(self: *Self, scope: *Scope, builder: *Module.DefinitionBuilder, node_index: Ast.Node.Index) !void {
    const node = self.ast.getNode(node_index);
    switch (node.data) {
        .var_decl, .const_decl => |decl| {
            const name = self.ast.getNodeSlice(decl.name);

            const maybeTy: ?Module.Ty = if (decl.type > 0)
                try self.parseTypeExpression(scope, builder, decl.type)
            else
                null;

            const maybeValue: ?Module.Value = if (decl.value > 0)
                try self.parseExpression(scope, builder, decl.value)
            else
                null;
            var ty: Module.Ty = undefined;
            if (maybeTy) |_ty| {
                ty = _ty;
            } else {
                if (maybeValue) |value| {
                    ty = value.getTy();
                } else {
                    std.debug.panic("No value or type", .{});
                }
            }

            const local = try builder.declareLocal(ty, false);
            // try scope.symbols_table.put(name, .{ .local = local });
            // if (ty)

            if (maybeValue) |value| {
                const value_ty = value.getTy();
                if (value_ty.eql(ty)) {
                    try builder.store(local, value);
                } else {
                    const casted = try builder.cast(ty, value, false);
                    try builder.store(local, casted);
                }
            }
            try scope.block.symbols_table.put(name, .{ .local = local });
            // const ty = try self.parseTypeExpression(scope, var_decl.type);
            // const local = try builder.declareLocal(ty, false);
            // _ = ty; // autofix
        },
        .assign => |assign| {
            // const left = try self.parseExpression(scope, builder, assign.lhs);
            const local = scope.getSymbolRecursive(self.ast.getNodeSlice(assign.lhs)) orelse {
                std.debug.panic("Symbol not found: '{s}'", .{self.ast.getNodeSlice(assign.lhs)});
            };
            const right = try self.parseExpression(scope, builder, assign.rhs);

            switch (local) {
                .local => |ref| {
                    try builder.store(ref, right);
                },
                .global => |ref| {
                    try builder.storeGlobal(ref, right);
                },
            }
        },
        .if_expr => |if_expr| {
            const condition = try self.parseExpression(scope, builder, if_expr.condition);
            const branch = try builder.makeBranch(condition);
            const then_block = try builder.makeBlock(branch);
            _ = try self.parseExpression(scope, builder, if_expr.then_branch);
            // const then_branch = try self.parseExpression(scope, builder, if_expr.then_branch);
            // const then_branch = try self.parseExpression(scope, builder, if_expr.then_branch);
            const else_block = blk: {
                if (if_expr.else_branch == 0) break :blk null;
                const block = try builder.makeBlock(branch);
                _ = try self.parseExpression(scope, builder, if_expr.else_branch);
                break :blk block;
            };
            const finally_block = try builder.makeBlock(branch);
            try builder.setBranchTarget(branch, then_block, else_block, finally_block);
        },
        .while_loop => |while_loop| {
            const loop_expr = try builder.makeLoop();
            const loop_body = try builder.makeBlock(loop_expr);
            const condition = try self.parseExpression(scope, builder, while_loop.condition);

            const branch = try builder.makeBranch(condition);
            const branch_body = try builder.makeBlock(branch);

            _ = try self.parseExpression(scope, builder, while_loop.body);
            const break_inst = try builder.breakTo(loop_expr, null);
            try builder.setBranchTarget(branch, branch_body, null, null);
            _ = break_inst; // autofix

            const finally_block = try builder.makeBlock(loop_expr);
            try builder.setLoopTarget(loop_expr, loop_body, finally_block);
            // return Module.Value.Imm(.void, .void);
        },
        .comment_line => {},
        .ret_expression => |ret_expr| {
            const value = if (ret_expr.node > 0)
                try self.parseExpression(scope, builder, ret_expr.node)
            else
                Module.Value.Imm(.void, .void);
            const ty = value.getTy();
            const ret_ty = blk: {
                var s = scope;
                while (std.meta.activeTag(s.*) != .fn_decl) {
                    s = s.getParent() orelse std.debug.panic("No function scope found", .{});
                }
                const func_decl = self.mod.getFunctionDeclaration(s.fn_decl.ref);
                const signature = self.mod.getSignature(func_decl.signature);
                break :blk signature.ret;
            };
            if (!ty.eql(ret_ty)) {
                const casted = try builder.cast(ret_ty, value, false);
                try builder.ret(casted, false);
            } else {
                try builder.ret(value, false);
            }
        },
        .fn_call => |fn_call| {
            const callee = try self.parseExpression(scope, builder, fn_call.callee);
            std.debug.print("callee: {}\n", .{callee});
            const args = self.ast.getList(fn_call.args_list);
            var args_list = std.ArrayList(Module.Value).init(self.allocator);
            defer args_list.deinit();
            for (args) |arg| {
                const value = try self.parseExpression(scope, builder, arg);
                try args_list.append(value);
            }
            _ = try builder.call(callee, args_list.items);
        },
        else => {
            std.debug.panic("Unexpected token: '{s}'", .{@tagName(node.data)});
        },
    }
}

test "gen" {
    const test_allocator = std.testing.allocator;
    const file = try std.fs.cwd().openFile("./playground.sk", .{});
    defer file.close();
    const source = try file.readToEndAlloc(test_allocator, 1024 * 1024);
    defer test_allocator.free(source);

    var errors = try ErrorManager.init(test_allocator);
    defer errors.deinit();
    var ast = try Ast.parse(test_allocator, &errors, source, .{});
    defer ast.deinit();
    std.debug.print("AST:\n", .{});
    try ast.format(std.io.getStdErr().writer().any(), 0, .{});
    var module = try gen(std.testing.allocator, &ast);
    defer module.deinit();
    std.debug.print("{}", .{module});
}

const T = struct {
    hash: std.StringHashMap(i32),
    list: std.ArrayList(i32),

    pub fn init(allocator: std.mem.Allocator) T {
        return T{ .hash = std.StringHashMap(i32).init(allocator), .list = std.ArrayList(i32).init(allocator) };
    }

    pub fn set(self: *T, key: []const u8, value: i32) !void {
        try self.hash.put(key, value);
    }

    pub fn deinit(self: *T) void {
        self.list.deinit();
        self.hash.deinit();
    }
};

fn add(t: *T) !void {
    try t.set("test", 123);
}

fn create() !T {
    var t = T.init(std.testing.allocator);
    _ = &t;
    for (0..1000) |i| {
        _ = i; // autofix
        try add(&t);
    }
    try t.set("aaa", 123);
    // try add(&t);
    // try add(&t);
    return t;
}

test "Alloc" {
    var t = try create();
    try add(&t);
    for (0..1000) |i| {
        try t.list.append(@intCast(i));
    }
    defer t.deinit();
    std.debug.print("t: {any}\n", .{t.hash.get("aaa")});
    // var t = T.init(std.testing.allocator);
    // var a = Scope.initNamespace(std.testing.allocator, null, .{ .ref = 1 });
    // defer a.deinit();

    // var b = Scope.initNamespace(std.testing.allocator, &a, .{ .ref = 2 });
    // defer b.deinit();
    // try a.namespace.member_scopes.put("test", undefined);

    // var c = Scope.initNamespace(std.testing.allocator, &b, .{ .ref = 3 });
    // defer c.deinit();
    // var scope = Scope.initFunctionDeclaration(std.testing.allocator, &c, .{ .ref = 4 }, 0, 0, .public, false);
    // defer scope.deinit();
    // try a.namespace.symbols_table.put("test", .{ .global = .{ .ref = 5 } });
    // // _ = try scope.fn_decl.symbols_table.(1);

    // // std.debug.print("scope: {any}\n", .{a.namespace.symbols_table.count()});
    // std.debug.print("scope: {any}\n", .{scope.getSymbolRecursive("test")});
    // for (0..10) |i| {
    //     _ = i; // autofix
    //     try t.someFn(&t);
    // }
    // // try t.append(1);
    // // try t.append(2);
    // // try t.append(3);
    // std.debug.print("t: {any}\n", .{t.list.get("test")});

    // const test_allocator = std.testing.allocator;
    // const scope = Scope.initNamespace(test_allocator, null, undefined);
    // _ = scope; // autofix
}
