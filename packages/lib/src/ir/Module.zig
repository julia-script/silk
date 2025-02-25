const std = @import("std");
pub const Signature = @import("./Signature.zig");
pub const FunctionDeclaration = @import("./FunctionDeclaration.zig");
const Map = std.AutoHashMapUnmanaged;
pub const Dfg = @import("./Dfg.zig");
pub const Function = @import("./Function.zig");
pub const Definition = @import("./Definition.zig");
pub const DefinitionBuilder = @import("./DefinitionBuilder.zig");
const debug = @import("../debug.zig");
pub const Block = @import("./Block.zig");
pub const Namespace = @import("./Namespace.zig");
const Array = std.ArrayList;
pub const Ty = @import("./ty.zig").Ty;
pub const utils = @import("./utils.zig");
pub const InstData = @import("./inst.zig").InstData;
pub const GlobalDeclaration = @import("./GlobalDeclaration.zig");
pub const TypedValue = @import("./TypedValue.zig");
pub const Value = @import("./value.zig").Value;
pub const Op = @import("./opcodes.zig").Op;
const Set = @import("../data_structures.zig").AutoSetUnmanaged;
const Box = @import("../pointer.zig").Box;
const BoxedDefinition = Box(Definition);

pub const Decl = union(enum) {
    func: FunctionDeclaration,
    global: GlobalDeclaration,

    pub const Ref = utils.MakeRef(.decl, Decl, "\x1b[36mdecl{d}\x1b[0m");
    pub fn deinit(self: *Decl) void {
        switch (self.*) {
            .func => |*func| {
                func.deinit();
            },
            .global => |*global| {
                global.deinit();
            },
        }
    }
    pub fn getValue(self: *Decl) TypedValue {
        return switch (self.*) {
            .func => |*func| {
                _ = func; // autofix
                // const def =
                // return def.dfg.getValue(def.dfg.entry);
            },
            .global => |*global| global.value,
        };
    }
    pub fn getName(self: *Decl) []const u8 {
        return switch (self.*) {
            .func => |*func| func.name,
            .global => |*global| global.name,
        };
    }
};
allocator: std.mem.Allocator,

// Lives for the lifetime of the module
arena: std.heap.ArenaAllocator,

namespaces: Namespace.Ref.ListUnmanaged(Namespace) = .{},
signatures: Signature.Ref.ListUnmanaged(Signature) = .{},
definition_map: Map(Decl.Ref, Definition.Ref) = .{},
definitions: Definition.Ref.ListUnmanaged(BoxedDefinition) = .{},
tys: Ty.Ref.ListUnmanaged(Ty.TyData) = .{},
decls: Decl.Ref.ListUnmanaged(Decl) = .{},
hashed_ty_map: Map(u64, Ty.Ref) = .{},
arch: Arch = .@"64-bit",
pub const Arch = enum {
    @"32-bit",
    @"64-bit",
};
const Self = @This();
pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
    };
}
pub fn deinit(self: *Self) void {
    for (self.definitions.slice()) |*def| {
        def.ptr.dfg.deinit();
    }
    self.arena.deinit();

    // // self.signatures.deinitRecursive();
    // self.tys.deinitRecursive();
    // // self.function_declarations.deinitRecursive();
    // self.definitions.deinitRecursive();
    // self.definition_map.deinit();
    // self.namespaces.deinitRecursive();
    // self.decls.deinitRecursive();
}
pub fn declareNamespace(self: *Self, name: []const u8) !Namespace.Ref {
    const namespace = try Namespace.init(
        self.arena.allocator(),
        name,
    );

    return try self.namespaces.append(self.arena.allocator(), namespace);
}
pub fn declareFunction(
    self: *Self,
    namespace: Namespace.Ref,
    name: []const u8,
    linkage: FunctionDeclaration.Linkage,
    signature: Signature.Ref,
) !Decl.Ref {
    return try self.decls.append(.{
        .func = try FunctionDeclaration.init(
            self.arena.allocator(),
            namespace,
            name,
            linkage,
            signature,
        ),
    });
}

pub fn setFunctionDeclaration(
    self: *Self,
    ref: Decl.Ref,
    namespace: Namespace.Ref,
    name: []const u8,
    linkage: FunctionDeclaration.Linkage,
    signature: Signature.Ref,
    ty: Ty,
) !void {
    self.decls.getPtr(ref).* = .{ .func = try FunctionDeclaration.init(
        self.arena.allocator(),
        namespace,
        name,
        linkage,
        signature,
        ty,
    ) };
}

pub fn makeDefinition(self: *Self, is_comptime: bool, is_inline: bool, dfg: ?Dfg) !Definition.Ref {
    var def = try BoxedDefinition.init(self.arena.allocator());
    def.ptr.dfg = dfg orelse try self.makeDfg(is_comptime, is_inline);
    return try self.definitions.append(self.arena.allocator(), def);
}

pub fn setDefinition(self: *Self, ref: Definition.Ref, dfg: Dfg) !void {
    self.definitions.getPtr(ref).ptr.dfg = dfg;
}
pub fn getDefinition(self: *const Self, ref: Definition.Ref) *Definition {
    return self.definitions.getPtr(ref).ptr;
}
pub fn makeDfg(self: *Self, is_comptime: bool, is_inline: bool) !Dfg {
    return try Dfg.init(self.allocator, is_comptime, is_inline);
}

pub fn declareGlobal(
    self: *Self,
    parent_namespace: Namespace.Ref,
    namespace: Namespace.Ref,
    name: []const u8,
    value: TypedValue,
) !Decl.Ref {
    return try self.decls.append(.{ .global = try GlobalDeclaration.init(
        self.arena.allocator(),
        parent_namespace,
        namespace,
        name,
        value,
    ) });
}
pub fn setGlobalDeclaration(
    self: *Self,
    ref: Decl.Ref,
    parent_namespace: Namespace.Ref,
    namespace: Namespace.Ref,
    name: []const u8,
    ty: Ty,
) !void {
    self.decls.getPtr(ref).* = .{ .global = try GlobalDeclaration.init(
        self.arena.allocator(),
        parent_namespace,
        namespace,
        name,
        ty,
    ) };
}
fn dupeTyData(self: *Self, ty_data: Ty.TyData) !Ty.TyData {
    return switch (ty_data) {
        .@"struct" => |_struct_ty| {
            const struct_ty: Ty.TyData.Struct = _struct_ty;
            const allocator = self.arena.allocator();

            const fields = try allocator.dupe(Ty.TyData.Struct.Field, struct_ty.fields);
            for (fields) |*field| {
                field.name = try allocator.dupe(u8, field.name);
            }
            const size = struct_ty.size orelse Ty.tryComputeSize(fields, self);
            if (size == null) {
                return ty_data;
            }
            return .{
                .@"struct" = .{
                    .fields = fields,
                    .associated_ns = struct_ty.associated_ns,
                    .size = size,
                    .sealed = size != null,
                },
            };
        },
        .array => |array_ty| {
            const field_size = array_ty.type.maybeGetSize(self) orelse return ty_data;
            const size = if (array_ty.len.isResolved())
                array_ty.len.value.readAs(u32) * field_size
            else {
                return ty_data;
            };

            return .{
                .array = .{
                    .type = array_ty.type,
                    .len = array_ty.len,
                    .size = size,
                },
            };
        },
        else => ty_data,
    };
}
pub fn declareTy(self: *Self, ty_data: Ty.TyData) !Ty {
    const hash = Ty.hashTyData(ty_data, self);
    const gop = try self.hashed_ty_map.getOrPut(self.arena.allocator(), hash);
    if (gop.found_existing) {
        return .{ .ref = gop.value_ptr.* };
    }

    const ref = try self.tys.append(
        self.arena.allocator(),
        try self.dupeTyData(ty_data),
    );
    gop.value_ptr.* = ref;
    return .{ .ref = ref };
}
pub fn getTy(self: *Self, ref: anytype) *Ty.TyData {
    const tyref = switch (@TypeOf(ref)) {
        Ty => ref.ref,
        Ty.Ref => ref,
        else => unreachable,
    };
    return self.tys.getPtr(tyref);
}
pub fn acceptTyData(self: *Self, ty: Ty, tag: std.meta.FieldEnum(Ty.TyData)) ?*Ty.TyData {
    return switch (ty) {
        .ref => |ref| {
            const tydata = self.getTy(ref);
            if (std.meta.activeTag(tydata.*) == tag) {
                return tydata;
            }
            return null;
        },
        else => null,
    };
}

pub fn modString(self: *Self, str: []const u8) []const u8 {
    return self.arena.allocator.dupe(u8, str);
}

pub fn getSignature(self: *Self, ref: Signature.Ref) *Signature {
    return self.signatures.getPtr(ref);
}
pub fn getFunctionDeclaration(self: *Self, ref: Decl.Ref) *FunctionDeclaration {
    return &self.decls.getPtr(ref).func;
}
pub fn getDeclaration(self: *Self, ref: Decl.Ref) *Decl {
    return self.decls.getPtr(ref);
}
pub fn getDefinitionByDeclRef(self: *const Self, ref: Decl.Ref) *Definition {
    const decl_ref = self.definition_map.get(ref) orelse std.debug.panic("Definition not found: {}", .{ref});
    return self.definitions.getPtr(decl_ref).ptr;
}
pub fn maybeGetDefinitionByDeclRef(self: *const Self, ref: Decl.Ref) ?*Definition {
    const maybe_def = self.definition_map.get(ref);
    if (maybe_def) |def_ref| {
        return self.definitions.getPtr(def_ref).ptr;
    }
    return null;
}
pub fn getFunction(self: *Self, ref: Decl.Ref) Function {
    return self.functions.get(ref) orelse debug.assertPrint("Function not found: {}", .{ref});
}

pub fn getBuiltinTy(self: *Self, builtin: Value.Builtin) !Ty {
    switch (builtin) {
        .length => {
            const signature = try Signature.from(self.arena.allocator(), &.{
                .{ .ty = .any, .is_comptime = false },
            }, .u64);
            const sig_ref = try self.signatures.append(self.arena.allocator(), signature);
            return try self.declareTy(.{ .func = .{
                .declaration = .{ .idx = 0 },
                .signature = sig_ref,
            } });
        },
        .sizeof => {
            const signature = try Signature.from(self.arena.allocator(), &.{
                .{ .ty = .type, .is_comptime = true },
            }, .u64);
            const sig_ref = try self.signatures.append(self.arena.allocator(), signature);
            return try self.declareTy(.{ .func = .{
                .declaration = .{ .idx = 0 },
                .signature = sig_ref,
            } });
        },
        else => std.debug.panic("Builtin '{s}' not implemented", .{
            @tagName(builtin),
        }),
    }
}
pub fn format(
    self: Self,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: std.io.AnyWriter,
) !void {
    try writer.print(";; Module\n\n", .{});
    var decl_iter = self.decls.iter();
    var i: usize = 0;

    // var def_to_print: Set(Dfg.Dependency) = .{};

    while (decl_iter.next()) |entry| {
        try writer.print("\n", .{});
        i += 1;
        const decl = entry.item;
        const ref = entry.ref;
        {
            const def = self.getDefinitionByDeclRef(ref);

            if (def.dfg.dependencies.count() > 0) {
                var deps_iter = def.dfg.dependencies.iterator();
                try writer.print("[deps: ", .{});
                var first = true;
                while (deps_iter.next()) |dep_entry| {
                    const dep = dep_entry.*;
                    if (!first) {
                        try writer.print(", ", .{});
                    }
                    first = false;
                    switch (dep) {
                        .declaration => |dep_ref| try writer.print("{}", .{dep_ref}),
                        .definition => |dep_ref| try writer.print("{}", .{dep_ref}),
                    }
                    // try writer.print("{}", .{decl_ref});
                }
                try writer.print("]\n", .{});
            }
        }
        switch (decl.*) {
            .func => |*func_decl| {
                var sig = self.signatures.getPtr(func_decl.signature);
                try writer.print("{}: function {s} {}\n", .{ ref, func_decl.name, sig.formatable(@constCast(&self)) });
                const def_ref = self.definition_map.get(ref) orelse continue;
                var def = self.getDefinition(def_ref);
                try writer.print("{}", .{def.display(&self)});
            },
            .global => |*global| {
                const def = self.getDefinitionByDeclRef(ref);

                try writer.print("{}: {s}", .{ ref, global.name });
                if (def.dfg.result) |result| {
                    try writer.print(" = {}", .{result.display(&self)});
                } else {
                    try writer.print(" {}", .{global.ty.display(&self)});
                }
                // try writer.print("{}", .{def.display(&self)});
                try writer.print("\n", .{});
            },
        }
    }
}
test "Module" {
    var module = Self.init(std.testing.allocator);
    defer module.deinit();
    var sig = Signature.init(std.testing.allocator);
    try sig.addParam(Ty.i32, false);
    sig.setReturn(Ty.i32);
    const sigRef = try module.signatures.append(sig);
    const ns = try module.declareNamespace("main");

    const func_decl = try module.declareFunction(ns, "main", .Export, sigRef);
    var builder = try DefinitionBuilder.init(
        std.testing.allocator,
        &module,
        .{ .function_body = func_decl },
    );

    const local0 = try builder.declareLocal(.u8, false);
    const local0_val = builder.useLocal(local0);
    const param0 = builder.useParam(0);

    const a = TypedValue.Imm(.u8, 1, false);
    _ = a; // autofix
    const b = TypedValue.Imm(.u8, 2, false);
    _ = b; // autofix

    const ins0 = try builder.add(.u8, param0, local0_val, true);
    const c = TypedValue.Imm(.u8, 3, false);
    const ins1 = try builder.sub(.u8, ins0, c, false);
    // const call = try builder.call(func_decl, &[_]Value{ins1});
    // _ = call; // autofix
    const cond = try builder.lt(.u8, ins1, c, false);
    const branch = try builder.makeBranch(cond);

    const branch_then_ref = try builder.makeBlock(branch);
    const branch_else_ref = try builder.makeBlock(branch);
    const branch_finally_ref = try builder.makeBlock(branch);
    try builder.setBranchTarget(branch, branch_then_ref, branch_else_ref, branch_finally_ref);

    const loop = try builder.makeLoop();
    const loop_body_ref = try builder.makeBlock(loop);
    const loop_finally_ref = try builder.makeBlock(loop);

    try builder.setLoopTarget(loop, loop_body_ref, loop_finally_ref);
    const def = try builder.commit();
    _ = def; // autofix

    const stderr = std.io.getStdErr().writer().any();
    _ = stderr; // autofix

    std.debug.print("{}\n", .{module});
}
