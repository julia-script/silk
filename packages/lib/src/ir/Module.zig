const std = @import("std");
pub const Signature = @import("./Signature.zig");
pub const FunctionDeclaration = @import("./FunctionDeclaration.zig");
const Map = std.AutoHashMap;
pub const Dfg = @import("./Dfg.zig");
pub const Function = @import("./Function.zig");
pub const Definition = @import("./Definition.zig");
pub const DefinitionBuilder = @import("./DefinitionBuilder.zig");
const debug = @import("../debug.zig");
pub const Block = @import("./Block.zig");
pub const Namespace = @import("./Namespace.zig");
const Array = std.ArrayList;
pub const Ty = @import("./tyval.zig").Ty;
pub const Value = @import("./val.zig").Value;
const utils = @import("./utils.zig");
pub const GlobalDeclaration = @import("./GlobalDeclaration.zig");
pub const Decl = union(enum) {
    func: FunctionDeclaration,
    global: GlobalDeclaration,

    pub const Ref = utils.MakeRef(.decl, Decl);
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
};
allocator: std.mem.Allocator,

// Lives for the lifetime of the module
arena: std.heap.ArenaAllocator,

namespaces: Namespace.Ref.List(Namespace),
signatures: Signature.Ref.List(Signature),
functions: Map(Decl.Ref, Function),
function_definitions_map: Map(Decl.Ref, Definition.Ref),
definitions: Definition.Ref.List(Definition),
tys: Ty.Ref.List(Ty.TyData),
decls: Decl.Ref.List(Decl),
const Self = @This();
pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .signatures = Signature.Ref.List(Signature).init(allocator),
        .functions = Map(Decl.Ref, Function).init(allocator),
        .function_definitions_map = Map(Decl.Ref, Definition.Ref).init(allocator),
        .definitions = Definition.Ref.List(Definition).init(allocator),
        .namespaces = Namespace.Ref.List(Namespace).init(allocator),
        .decls = Decl.Ref.List(Decl).init(allocator),
        .tys = Ty.Ref.List(Ty.TyData).init(allocator),
    };
}
pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.signatures.deinitRecursive();
    self.tys.deinitRecursive();
    // self.function_declarations.deinitRecursive();
    self.definitions.deinitRecursive();
    self.functions.deinit();
    self.function_definitions_map.deinit();
    self.namespaces.deinitRecursive();
    self.decls.deinitRecursive();
}
pub fn declareNamespace(self: *Self, name: []const u8) !Namespace.Ref {
    const namespace = try Namespace.init(
        self.allocator,
        name,
    );

    return try self.namespaces.append(namespace);
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
            self.allocator,
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
) !void {
    self.decls.getPtr(ref).* = .{ .func = try FunctionDeclaration.init(
        self.allocator,
        namespace,
        name,
        linkage,
        signature,
    ) };
}

pub fn declareGlobalDeclaration(
    self: *Self,
    namespace: Namespace.Ref,
    name: []const u8,
    ty: Ty,
) !Decl.Ref {
    return try self.decls.append(.{ .global = try GlobalDeclaration.init(
        self.allocator,
        namespace,
        name,
        ty,
    ) });
}
pub fn setGlobalDeclaration(
    self: *Self,
    ref: Decl.Ref,
    namespace: Namespace.Ref,
    name: []const u8,
    ty: Ty,
) !void {
    self.decls.getPtr(ref).* = .{ .global = try GlobalDeclaration.init(
        self.allocator,
        namespace,
        name,
        ty,
    ) };
}
pub fn declareTy(self: *Self, ty: Ty.TyData) !Ty {
    const ref = try self.tys.append(ty);
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

pub fn getSignature(self: *Self, ref: Signature.Ref) Signature {
    return self.signatures.get(ref);
}
pub fn getFunctionDeclaration(self: *Self, ref: Decl.Ref) *FunctionDeclaration {
    return &self.decls.getPtr(ref).func;
}
pub fn getDeclaration(self: *Self, ref: Decl.Ref) *Decl {
    return self.decls.getPtr(ref);
}
pub fn getFunction(self: *Self, ref: Decl.Ref) Function {
    return self.functions.get(ref) orelse debug.assertPrint("Function not found: {}", .{ref});
}

pub fn format(
    self: Self,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: std.io.AnyWriter,
) !void {
    try writer.print(";; Module\n\n", .{});
    var func_decl_iter = self.decls.iter();

    var i: usize = 0;
    while (func_decl_iter.next()) |entry| {
        try writer.print("\n", .{});
        i += 1;
        const decl = entry.item;
        const ref = entry.ref;
        switch (decl.*) {
            .func => |*func_decl| {

                // var sig = self.signatures.getPtr(func_decl.signature);
                var sig = self.signatures.getPtr(func_decl.signature);

                try writer.print("{}: function {s} {}\n", .{ ref, func_decl.name, sig.formatable(@constCast(&self)) });
                const def_ref = self.function_definitions_map.get(ref) orelse continue;
                var def = self.definitions.getPtr(def_ref);
                try writer.print("{}", .{def.formatable(&self)});
            },
            .global => |*global| {
                switch (global.ty) {
                    .type => {
                        try writer.print("{}: type {s}\n", .{ ref, global.name });
                    },
                    else => {
                        try writer.print("{}: {s} {}\n", .{ ref, global.name, global.ty });
                    },
                }
            },
        }
    }
}

const Inst = @import("./inst.zig").Inst;
const InstVal = Inst.InstVal;

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

    const a = Value.Const(.u8, 1);
    _ = a; // autofix
    const b = Value.Const(.u8, 2);
    _ = b; // autofix

    const ins0 = try builder.add(.u8, param0, local0_val, true);
    const c = Value.Const(.u8, 3);
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
