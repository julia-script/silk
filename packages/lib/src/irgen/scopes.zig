const std = @import("std");
const Visibility = @import("../shared.zig").Visibility;
const Allocator = std.mem.Allocator;
const Module = @import("../ir/Module.zig");
const Ast = @import("../Ast.zig");
const Local = @import("../ir/Dfg.zig").Local;

pub const Scope = union(enum) {
    namespace: NamespaceScope,
    fn_decl: FunctionDeclarationScope,
    block: BlockScope,
    global_decl: GlobalDeclarationScope,
    pub fn initNamespace(allocator: Allocator, parent: ?*Scope, ref: Module.Namespace.Ref) Scope {
        return Scope{
            .namespace = .{
                .allocator = allocator,
                .parent = parent,
                .ref = ref,
                .symbols_table = std.StringArrayHashMap(Symbol).init(allocator),
                .member_scopes = std.StringArrayHashMap(Scope).init(allocator),
            },
        };
    }

    pub fn initFunctionDeclaration(
        allocator: Allocator,
        mod: *Module,
        parent: *Scope,
        ref: Module.Decl.Ref,
        proto_node: Ast.Node.Index,
        body_node: ?Ast.Node.Index,
        visibility: Visibility,
        is_exported: bool,
    ) !Scope {
        return Scope{
            .fn_decl = .{
                .allocator = allocator,
                .parent = parent,
                .ref = ref,
                .proto_node = proto_node,
                .body_node = body_node,
                .visibility = visibility,
                .is_exported = is_exported,
                .params = std.StringArrayHashMap(Symbol).init(allocator),
                .definition_builder = try Module.DefinitionBuilder.init(allocator, mod, .global_body),
            },
        };
    }
    pub fn initBlock(allocator: Allocator, parent: *Scope, definition_builder: *Module.DefinitionBuilder) Scope {
        return Scope{
            .block = .{
                .allocator = allocator,
                .parent = parent,
                .symbols_table = std.StringArrayHashMap(Symbol).init(allocator),
                .definition_builder = definition_builder,
            },
        };
    }
    pub fn initGlobalDeclaration(
        allocator: Allocator,
        mod: *Module,
        parent: *Scope,
        ref: Module.Decl.Ref,
        name: []const u8,
        ty_node: Ast.Node.Index,
        value_node: Ast.Node.Index,
        is_type_decl: bool,
        is_mutable: bool,
        visibility: Visibility,
        is_exported: bool,
    ) !Scope {
        return Scope{
            .global_decl = .{
                .definition_builder = try Module.DefinitionBuilder.init(allocator, mod, .global_body),
                .parent = parent,
                .ref = ref,
                .name = name,
                .ty_node = ty_node,
                .value_node = value_node,
                .is_type_decl = is_type_decl,
                .is_mutable = is_mutable,
                .visibility = visibility,
                .is_exported = is_exported,
            },
        };
    }
    pub fn deinit(self: *Scope) void {
        switch (self.*) {
            .namespace => |*ns| {
                ns.member_scopes.deinit();
                ns.symbols_table.deinit();
            },
            .fn_decl => |*fn_decl| {
                fn_decl.params.deinit();
            },
            .block => |*block| {
                block.symbols_table.deinit();
            },
            .global_decl => |*global_decl| {
                _ = global_decl; // autofix
                // global_decl.definition_builder.deinit();
            },
        }
    }
    pub fn getSymbol(self: *Scope, name: []const u8) ?Symbol {
        return switch (self.*) {
            .namespace => |*ns| {
                std.debug.print("namespace: {*} {d}\n", .{ ns, ns.symbols_table.count() });
                return ns.symbols_table.get(name);
            },
            .fn_decl => |*fn_decl| {
                return fn_decl.params.get(name);
            },
            .block => |*block| {
                return block.symbols_table.get(name);
            },
            .global_decl => {
                return null;
            },
        };
    }
    pub fn getParent(self: *Scope) ?*Scope {
        return switch (self.*) {
            .namespace => |*ns| ns.parent,
            .fn_decl => |*fn_decl| fn_decl.parent,
            .block => |*block| block.parent,
            .global_decl => |*global_decl| global_decl.parent,
        };
    }
    pub fn getSymbolRecursive(self: *Scope, name: []const u8) ?Symbol {
        return self.getSymbol(name) orelse {
            if (self.getParent()) |parent| {
                return parent.getSymbolRecursive(name);
            }
            return null;
        };
    }
};

const NamespaceScope = struct {
    allocator: Allocator,
    parent: ?*Scope,
    ref: Module.Namespace.Ref,
    member_scopes: std.StringArrayHashMap(Scope),
    symbols_table: SymbolsTable,
};

const FunctionDeclarationScope = struct {
    allocator: Allocator,
    parent: *Scope,
    ref: Module.Decl.Ref,
    proto_node: Ast.Node.Index,
    body_node: ?Ast.Node.Index,
    visibility: Visibility,
    is_exported: bool,
    definition_builder: Module.DefinitionBuilder,
    params: SymbolsTable,
    pub fn getParamName(self: *FunctionDeclarationScope, param_index: u32, ast: *Ast) []const u8 {
        const node = ast.getNode(self.proto_node);
        const params_list = ast.getList(node.data.fn_proto.params_list);
        const param_node = params_list[param_index];
        const param_node_data = ast.getNode(param_node).data.fn_param;
        return ast.getNodeSlice(param_node_data.name);
    }
};
const GlobalDeclarationScope = struct {
    parent: *Scope,
    definition_builder: Module.DefinitionBuilder,
    ref: Module.Decl.Ref,
    name: []const u8,
    ty_node: Ast.Node.Index,
    value_node: Ast.Node.Index,
    is_type_decl: bool,
    is_mutable: bool,
    visibility: Visibility,
    is_exported: bool,
};

const SymbolsTable = std.StringArrayHashMap(Symbol);
pub const Symbol = union(enum) {
    local: Local.Ref,
    global: Module.Decl.Ref,
};

const BlockScope = struct {
    allocator: Allocator,
    parent: *Scope,
    symbols_table: SymbolsTable,
    definition_builder: *Module.DefinitionBuilder,
};
const ExpressionScope = struct {
    parent: *Scope,
};
