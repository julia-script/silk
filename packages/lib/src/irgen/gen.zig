const Ast = @import("../Ast.zig");
const Visibility = @import("../shared.zig").Visibility;
const std = @import("std");
const Module = @import("../ir/Module.zig");
const ErrorManager = @import("../ErrorManager.zig");
const Allocator = std.mem.Allocator;
const debug = @import("../debug.zig");
const Scope = @import("./scopes.zig").Scope;
const Set = @import("../data_structures.zig").AutoSetUnmanaged;
const SemaPass = @import("../ir/passes/sema.zig");

mod: *Module,
ast: *Ast,
allocator: Allocator,
arena: std.heap.ArenaAllocator,
decl_status: std.AutoHashMapUnmanaged(Module.Decl.Ref, DeclProgress) = .{},
const DeclProgress = struct {
    scope: *Scope,
    decl_status: Status = .idle,
    def_status: Status = .idle,
};
const Status = enum {
    idle,
    resolving,
    done,
};

const Self = @This();
pub fn deinit(self: *Self) void {
    self.arena.deinit();
}
pub fn initStatus(self: *Self, ref: Module.Decl.Ref, scope: *Scope) !void {
    try self.decl_status.put(self.arena.allocator(), ref, .{ .scope = scope });
}
pub fn updateDeclStatus(self: *Self, ref: Module.Decl.Ref, status: Status) void {
    self.decl_status.getPtr(ref).?.decl_status = status;
}
pub fn getDeclStatus(self: *Self, ref: Module.Decl.Ref) Status {
    return self.decl_status.getPtr(ref).?.decl_status;
}
pub fn updateDefStatus(self: *Self, ref: Module.Decl.Ref, status: Status) void {
    self.decl_status.getPtr(ref).?.def_status = status;
}
pub fn getDefStatus(self: *Self, ref: Module.Decl.Ref) Status {
    return self.decl_status.getPtr(ref).?.def_status;
}
pub fn gen(allocator: Allocator, ast: *Ast) !Module {
    var module = Module.init(allocator);
    var self = Self{
        .mod = &module,
        .ast = ast,
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
    };
    defer self.deinit();
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
        .struct_field => {},
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
        const ref = try self.mod.decls.reserve(self.mod.arena.allocator());
        var fn_scope = try Scope.initFunctionDeclaration(
            self.allocator,
            self.mod,
            scope,
            ref,
            fn_node.data.fn_decl.proto,
            if (fn_node.data.fn_decl.body > 0) fn_node.data.fn_decl.body else null,
            visibility,
            is_exported,
        );
        try self.initStatus(ref, &fn_scope);

        const proto_data = self.ast.getNode(fn_node.data.fn_decl.proto);
        const name = self.ast.getNodeSlice(proto_data.data.fn_proto.name);
        try scope.namespace.symbols_table.put(name, .{ .global = ref });
        try scope.namespace.member_scopes.put(name, fn_scope);
        return;

        // return try self.parseFnDecl(scope, index, visibility, is_exported);
    }

    // if (self.ast.nodeIs(index, .var_decl) or self.ast.nodeIs(index, .const_decl)) {
    //     std.debug.panic("var_decl or const_decl not implemented", .{});
    // }
    const data = switch (node.data) {
        .type_decl, .var_decl, .const_decl => |d| d,
        else => {
            std.debug.panic("Unexpected token: '{s}'", .{@tagName(node.data)});
        },
    };

    const name = self.ast.getNodeSlice(data.name);
    const ref = try self.mod.decls.reserve(self.mod.arena.allocator());
    try scope.namespace.symbols_table.put(name, .{ .global = ref });
    // try scope.namespace.member_scopes.put(name, .{
    //     .global_decl = .{
    //         .parent = scope,
    //         .ref = ref,
    //         .name = name,
    //         .ty_node = data.type,
    //         .value_node = data.value,
    //         .is_type_decl = self.ast.nodeIs(index, .type_decl),
    //         .is_mutable = self.ast.nodeIs(index, .var_decl),
    //         .visibility = visibility,
    //         .is_exported = is_exported,
    //     },
    // });
    var global_scope = try Scope.initGlobalDeclaration(
        self.allocator,
        self.mod,
        scope,
        ref,
        name,
        data.type,
        data.value,
        self.ast.nodeIs(index, .type_decl),
        self.ast.nodeIs(index, .var_decl),
        visibility,
        is_exported,
    );
    try scope.namespace.member_scopes.put(
        name,
        global_scope,
    );
    try self.initStatus(ref, &global_scope);
}
pub fn parseDecl(self: *Self, scope: *Scope) !void {
    switch (scope.*) {
        .fn_decl => {
            self.updateDeclStatus(scope.fn_decl.ref, .resolving);
            defer self.updateDeclStatus(scope.fn_decl.ref, .done);
            try self.parseFnDecl(scope);
        },
        .global_decl => {
            self.updateDeclStatus(scope.global_decl.ref, .resolving);
            defer self.updateDeclStatus(scope.global_decl.ref, .done);
            try self.parseGlobalDecl(scope);
        },
        else => {
            std.debug.panic("Unexpected scope: '{s}'", .{@tagName(scope.*)});
        },
    }
}

pub fn parseGlobalDecl(
    self: *Self,
    scope: *Scope,
) !void {
    if (scope.global_decl.is_type_decl) {
        const ref = scope.global_decl.ref;
        const name = scope.global_decl.name;

        try self.mod.setGlobalDeclaration(
            ref,
            scope.global_decl.parent.namespace.ref,
            name,
            Module.TypedValue.init(
                .type,
                .{ .global = ref },
                true,
            ),
        );
    }
}
pub fn parseFnDecl(self: *Self, scope: *Scope) !void {
    const proto_data = self.ast.getNode(scope.fn_decl.proto_node);
    const name = self.ast.getNodeSlice(proto_data.data.fn_proto.name);
    var signature = Module.Signature.init();

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

        try signature.addParam(self.mod.arena.allocator(), param_type, false);
    }

    const ret_type = try self.parseTypeExpression(
        scope,
        &scope.fn_decl.definition_builder,
        proto_data.data.fn_proto.return_type,
    );
    signature.setReturn(ret_type);

    const signature_ref = try self.mod.signatures.append(self.mod.arena.allocator(), signature);

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
    // if (typeValue.isType()) {
    //     return typeValue.value.ty;
    // }
    switch (typeValue.value) {
        .ty => |ty| {
            return ty;
        },
        .global => |ref| {
            return .{ .global = ref };
        },
        else => {
            std.debug.panic("Unexpected type value: '{}'", .{typeValue.display(self.mod)});
        },
    }
}

pub fn parseExpression(self: *Self, scope: *Scope, builder: *Module.DefinitionBuilder, node_index: Ast.Node.Index) anyerror!Module.TypedValue {
    const node = self.ast.getNode(node_index);

    switch (node.data) {
        // Parse easy ones first
        .ty_f32 => return Module.TypedValue.Type(.f32),
        .ty_f64 => return Module.TypedValue.Type(.f64),

        .ty_i8 => return Module.TypedValue.Type(.i8),
        .ty_i16 => return Module.TypedValue.Type(.i16),
        .ty_i32 => return Module.TypedValue.Type(.i32),
        .ty_i64 => return Module.TypedValue.Type(.i64),

        .ty_u8 => return Module.TypedValue.Type(.u8),
        .ty_u16 => return Module.TypedValue.Type(.u16),
        .ty_u32 => return Module.TypedValue.Type(.u32),
        .ty_u64 => return Module.TypedValue.Type(.u64),

        .ty_boolean => return Module.TypedValue.Type(.bool),
        .ty_void => return Module.TypedValue.Type(.void),
        .ty_type => return Module.TypedValue.Type(.type),
        .ty_array => |ty_array| {
            const ty = try self.parseTypeExpression(scope, builder, ty_array.type);

            const size = try self.parseExpression(scope, builder, ty_array.size_expr);
            // const slice = self.ast.getNodeSlice();
            // const len = try std.fmt.parseInt(usize, slice, 10);
            // return Module.Value.ImmTy(.{ .array = .{ .type = ty, .size = size } });
            const array_ty = try self.mod.declareTy(.{ .array = .{ .type = ty, .size = size } });
            return Module.TypedValue.Type(array_ty);
        },
        .number_literal => |number_literal| {
            const slice = self.ast.getTokenSlice(number_literal.token);
            if (std.mem.containsAtLeast(u8, slice, 1, ".")) {
                const value = try std.fmt.parseFloat(f64, slice);
                return Module.TypedValue.Imm(.float, value, false);
            } else {
                const value = try std.fmt.parseInt(i64, slice, 10);
                return Module.TypedValue.Imm(.int, value, false);
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
            const ty = left.ty;

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
            const ty = left.ty;

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
                    const value = try scope.block.definition_builder.useGlobal(self.mod, global, false);
                    return value;
                    // switch (value.value) {
                    //     .global => |ref| {
                    //         std.debug.print("ref: {}\n", .{ref});
                    //         return value;
                    //     },
                    //     else => {
                    //         return value;
                    //     },
                    // }
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
            return Module.TypedValue.Void;
        },
        .array_init => |array_init| {
            const ty = try self.parseTypeExpression(scope, builder, array_init.type);

            const items_list = self.ast.getList(array_init.items_list);
            var items = std.ArrayList(Module.TypedValue).init(self.allocator);
            defer items.deinit();

            for (items_list) |item| {
                const value = try self.parseExpression(scope, builder, item);
                try items.append(value);
            }
            const array = try builder.initArray(ty, items.items, false);
            return array;
        },
        .type_init => |type_init| {
            const ty = try self.parseTypeExpression(scope, builder, type_init.type);
            const fields_list = self.ast.getList(type_init.field_init_list);
            var keys = std.ArrayList([]const u8).init(self.allocator);
            defer keys.deinit();
            var values = std.ArrayList(Module.TypedValue).init(self.allocator);
            defer values.deinit();
            for (fields_list) |field| {
                const field_init = self.ast.getNode(field).data.field_init;
                const name = self.ast.getNodeSlice(field_init.name);
                const value = try self.parseExpression(scope, builder, field_init.value);
                try keys.append(name);
                try values.append(value);
            }
            const init_struct = try builder.initStruct(ty, keys.items, values.items, false);
            return init_struct;
        },

        .struct_decl => |struct_decl| {
            const members = self.ast.getList(struct_decl.members_list);
            var allocator = self.mod.arena.allocator();

            var fields = std.ArrayList(Module.Ty.TyData.Struct.Field).init(allocator);
            var has_decl = false;

            // defer fields.deinit();
            for (members, 0..) |member, i| {
                const member_data = self.ast.getNode(member).data;
                const struct_field = switch (member_data) {
                    .struct_field => member_data.struct_field,
                    else => {
                        has_decl = true;
                        continue;
                    },
                };
                const name = self.ast.getNodeSlice(struct_field.name);
                const ty = try self.parseTypeExpression(scope, builder, struct_field.type);
                try fields.append(.{
                    .name = try allocator.dupe(u8, name),
                    .ty = ty,
                    .source_order_index = @intCast(i),
                });
            }
            const struct_ty = try self.mod.declareTy(.{
                .@"struct" = .{
                    .fields = try fields.toOwnedSlice(),
                    .sealed = false,
                },
            });
            if (has_decl) {
                var ns_scope = try self.collectNamespaceSymbols(scope, node_index);
                self.mod.namespaces.getPtr(ns_scope.namespace.ref).ty = struct_ty;
                defer ns_scope.deinit();
                const ns_scope_ptr = &ns_scope;
                try self.parseStructDecl(ns_scope_ptr);
                try self.parseStructDef(ns_scope_ptr);
            }
            return Module.TypedValue.Type(struct_ty);
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
        try self.parseDecl(member_scope);
        // switch (member_scope.*) {
        //     .fn_decl => {
        //         try self.parseFnDecl(member_scope);
        //     },
        //     .global_decl => {
        //         try self.parseGlobalDecl(member_scope);
        //     },
        //     else => {
        //         std.debug.panic("Unexpected member scope: '{s}'", .{@tagName(member_scope.*)});
        //     },
        // }
        // try self.parseDecl(&member_scope);
    }
}

pub fn parseStructDef(self: *Self, scope: *Scope) !void {
    var iter = scope.namespace.member_scopes.iterator();
    while (iter.next()) |entry| {
        const member_scope = entry.value_ptr;
        try self.parseDefinition(member_scope);
        member_scope.deinit();
    }
}

pub fn parseDefinition(self: *Self, scope: *Scope) !void {
    const decl = switch (scope.*) {
        .fn_decl => scope.fn_decl.ref,
        .global_decl => scope.global_decl.ref,
        else => {
            std.debug.panic("Unexpected scope: '{s}'", .{@tagName(scope.*)});
        },
    };
    const status = self.getDefStatus(decl);
    if (status == .done) {
        return;
    }
    self.updateDefStatus(decl, .resolving);
    defer self.updateDefStatus(decl, .done);
    switch (scope.*) {
        .fn_decl => try self.parseFnDef(scope),
        .global_decl => try self.parseGlobalDef(scope),
        else => {
            std.debug.panic("Unexpected scope: '{s}'", .{@tagName(scope.*)});
        },
    }
}
pub fn parseFnDef(self: *Self, scope: *Scope) !void {
    const body_node_index = scope.fn_decl.body_node orelse return;
    const body_node = self.ast.getNode(body_node_index);
    _ = body_node; // autofix

    const fn_decl = self.mod.getFunctionDeclaration(scope.fn_decl.ref);
    const signature = self.mod.getSignature(fn_decl.signature);
    for (signature.params.items, 0..) |param, i| {
        var dfg = scope.fn_decl.definition_builder.getDfg();
        const local = try dfg.pushLocal(param.ty, true, param.is_comptime);
        const param_name = scope.fn_decl.getParamName(@intCast(i), self.ast);
        try scope.fn_decl.params.put(param_name, .{ .local = local });
    }
    try self.parseEntrypoint(scope, &scope.fn_decl.definition_builder, body_node_index);

    _ = try scope.fn_decl.definition_builder.commit(scope.fn_decl.ref);
}

pub fn parseGlobalDef(self: *Self, scope: *Scope) !void {
    const body_node_index = scope.global_decl.value_node;
    const body_node = self.ast.getNode(body_node_index);
    _ = body_node; // autofix

    const value = try self.parseExpression(
        scope,
        &scope.global_decl.definition_builder,
        scope.global_decl.value_node,
    );
    std.debug.print("value node {} value: {}\n", .{ scope.global_decl.value_node, value.display(self.mod) });
    self.mod.getDeclaration(scope.global_decl.ref).global.value = value;
    // try self.mod.setGlobalDeclaration(
    //     scope.global_decl.ref,
    //     scope.global_decl.parent.namespace.ref,
    //     scope.global_decl.name,
    //     value,
    // );

    _ = try scope.global_decl.definition_builder.commit(scope.global_decl.ref);
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

            const maybeValue: ?Module.TypedValue = if (decl.value > 0)
                try self.parseExpression(scope, builder, decl.value)
            else
                null;
            var ty: Module.Ty = undefined;
            if (maybeTy) |_ty| {
                ty = _ty;
            } else {
                if (maybeValue) |value| {
                    ty = value.ty;
                } else {
                    std.debug.panic("No value or type", .{});
                }
            }

            const local = try builder.declareLocal(ty, false);
            // try scope.symbols_table.put(name, .{ .local = local });
            // if (ty)

            if (maybeValue) |value| {
                if (value.ty.eql(ty)) {
                    try builder.store(local, value);
                } else {
                    const casted = try builder.cast(ty, value, false);
                    try builder.store(local, casted);
                }
            }
            try scope.block.symbols_table.put(name, .{ .local = local });
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
            const break_inst = try builder.breakTo(loop_expr, null, false);
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
                Module.TypedValue.Imm(.void, .void, false);
            const ty = value.ty;
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
            var args_list = std.ArrayList(Module.TypedValue).init(self.allocator);
            defer args_list.deinit();
            for (args) |arg| {
                const value = try self.parseExpression(scope, builder, arg);
                try args_list.append(value);
            }
            _ = try builder.call(callee, args_list.items, false);
        },
        else => {
            std.debug.panic("Unexpected token: '{s}'", .{@tagName(node.data)});
        },
    }
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
    // std.debug.print("t: {any}\n", .{t.hash.get("aaa")});
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
    try SemaPass.run(test_allocator, &module);
    std.debug.print("{}", .{module});
}
