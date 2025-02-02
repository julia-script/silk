const std = @import("std");
const Signature = @import("./Signature.zig");
const FunctionDeclaration = @import("./FunctionDeclaration.zig");
const Map = std.AutoHashMap;
const Dfg = @import("./Dfg.zig");
const Function = @import("./Function.zig");
const Definition = @import("./Definition.zig");
const DefinitionBuilder = @import("./DefinitionBuilder.zig");

allocator: std.mem.Allocator,
// Lives for the lifetime of the module
arena: std.heap.ArenaAllocator,

signatures: Signature.Ref.List(Signature),
function_declarations: FunctionDeclaration.Ref.List(FunctionDeclaration),
functions: Map(FunctionDeclaration.Ref, Function),
function_definitions_map: Map(FunctionDeclaration.Ref, Definition.Ref),

definitions: Definition.Ref.List(Definition),

const Self = @This();
pub fn init(allocator: std.mem.Allocator) Self {
    return .{
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .signatures = Signature.Ref.List(Signature).init(allocator),
        .function_declarations = FunctionDeclaration.Ref.List(FunctionDeclaration).init(allocator),
        .functions = Map(FunctionDeclaration.Ref, Function).init(allocator),
        .function_definitions_map = Map(FunctionDeclaration.Ref, Definition.Ref).init(allocator),
        .definitions = Definition.Ref.List(Definition).init(allocator),
    };
}
pub fn declareFunction(self: *Self, name: []const u8, linkage: FunctionDeclaration.Linkage, signature: Signature.Ref) !FunctionDeclaration.Ref {
    const func_decl = try FunctionDeclaration.init(self.allocator, name, linkage, signature);
    return try self.function_declarations.append(func_decl);
}

pub fn modString(self: *Self, str: []const u8) []const u8 {
    return self.arena.allocator.dupe(u8, str);
}

pub fn deinit(self: *Self) void {
    self.arena.deinit();
    self.signatures.deinitRecursive();
    self.function_declarations.deinitRecursive();
    self.definitions.deinitRecursive();
    self.functions.deinit();
    self.function_definitions_map.deinit();
}

pub fn format(
    self: Self,
    comptime _: []const u8,
    _: std.fmt.FormatOptions,
    writer: std.io.AnyWriter,
) !void {
    try writer.print(";; Module\n\n", .{});
    const instruction_indent = "        ";
    _ = instruction_indent; // autofix
    var func_decl_iter = self.function_declarations.iter();
    while (func_decl_iter.next()) |entry| {
        const func_decl = entry.item;
        const ref = entry.ref;

        var sig = self.signatures.getPtr(func_decl.signature);
        try writer.print("function {s} {}\n", .{ func_decl.name, sig.formatable(@constCast(&self)) });
        const def_ref = self.function_definitions_map.get(ref) orelse continue;
        var def = self.definitions.getPtr(def_ref);
        try writer.print("{}", .{def.formatable(&self)});
    }
}

const Inst = @import("./inst.zig").Inst;
const InstVal = Inst.InstVal;
const Ty = @import("./tyval.zig").Ty;

test "Module" {
    var module = Self.init(std.testing.allocator);
    defer module.deinit();
    var sig = Signature.init(std.testing.allocator);
    try sig.addParam(Ty.i32);
    sig.setReturn(Ty.i32);
    const sigRef = try module.signatures.append(sig);

    const func_decl = try module.declareFunction("main", .Export, sigRef);
    var builder = try DefinitionBuilder.init(
        std.testing.allocator,
        &module,
        .{ .function_body = func_decl },
    );

    // var dfg = try Dfg.init(std.testing.allocator);
    // defer dfg.deinit();
    const a = try InstVal.fromImm(.u8, 1);
    const b = try InstVal.fromImm(.u8, 2);

    const ref = try builder.dfg.pushBinary(Dfg.EntryBlock, .add, a, b);
    const c = try InstVal.fromImm(.u8, 3);
    const y = try builder.dfg.pushBinary(
        Dfg.EntryBlock,
        .sub,
        InstVal.fromRef(ref),
        c,
    );
    _ = y; // autofix
    const def = try builder.commit();
    _ = def; // autofix

    const stderr = std.io.getStdErr().writer().any();
    _ = stderr; // autofix

    // try stderr.print("sig: {}\n", .{module.signatures.getPtr(sigRef).formatable(&module)});
    // // try builder.dfg.formatInst(stderr, ref);
    // try stderr.print("\n", .{});
    // // try builder.dfg.formatInst(stderr, y);
    // try stderr.print("\n", .{});

    std.debug.print("{}\n", .{module});
}
