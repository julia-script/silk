const std = @import("std");

const Ast = @import("../Ast.zig");
const cranelift = @import("../cranelift/builder.zig");
const ErrorManager = @import("../ErrorManager.zig");
const Module = @import("../ir/Module.zig");
const IrGen = @import("../irgen/IrGen.zig");

const EmitFileFn = fn (path: []const u8, content: []const u8) anyerror!void;
pub const EmitOptions = struct {
    emitFileFn: EmitFileFn,
    entrypoint: []const u8,
};
const Self = @This();
module: *Module,
object_module: *cranelift.ObjectModule,
object_builder: *cranelift.ObjectBuilder,
allocator: std.mem.Allocator,

pub fn emit(allocator: std.mem.Allocator, module: *Module, options: EmitOptions) !void {
    var settings = cranelift.SettingsBuilder.new();

    settings.set("opt_level", "speed");
    var flags = cranelift.Flags.new(&settings);
    var isa_builder = cranelift.IsaBuilder.new();

    var isa = isa_builder.finish(&flags);
    var object_builder = cranelift.ObjectBuilder.new(&isa, "obj");
    var object_module = cranelift.ObjectModule.new(&object_builder);

    const entry_ref = module.getDeclarationRefByName(.{ .idx = 0 }, options.entrypoint) orelse std.debug.panic("no entrypoint called '{s}'", .{options.entrypoint});
    const entry_decl = module.getDeclaration(entry_ref);
    const entry_def = module.getDefinitionByDeclRef(entry_ref);
    var codegen = Self{
        .module = module,
        .object_module = &object_module,
        .object_builder = &object_builder,
        .allocator = allocator,
    };
    try codegen.emitDeclaration(entry_ref);

    _ = entry_def; // autofix
    _ = entry_decl; // autofix
}
fn translateType(self: *Self, src_ty: Module.Ty) cranelift.Type {
    return switch (src_ty) {
        .i8 => .I8,
        .i16 => .I16,
        .i32 => .I32,
        .i64 => .I64,
        .f32 => .F32,
        .f64 => .F64,

        else => std.debug.panic("type {} is not supported", .{src_ty.display(self.module)}),
    };
}
pub fn translateSignature(self: *Self, src_signature_ref: Module.Signature.Ref) !cranelift.Signature {
    const src_signature = self.module.getSignature(src_signature_ref);
    var sig = cranelift.Signature.init(cranelift.CallConv.SystemV);

    for (src_signature.params.items) |param| {
        sig.addParam(self.translateType(param.ty));
    }
    // sig.ret = self.translateType(src_signature.ret);
    if (src_signature.ret != .void) {
        sig.addReturn(self.translateType(src_signature.ret));
    }
    return sig;
}
pub fn emitDeclaration(self: *Self, src_decl_ref: Module.Decl.Ref) !void {
    const src_decl = self.module.getDeclaration(src_decl_ref);
    const src_def = self.module.getDefinitionByDeclRef(src_decl_ref);

    const signature = try self.translateSignature(src_decl.func.signature);
    const null_terminated_name = try self.allocator.dupeZ(u8, src_decl.func.name);
    defer self.allocator.free(null_terminated_name);
    const function_id = self.object_module.declareFunction(null_terminated_name, &signature);
    _ = function_id; // autofix
    const src_ns = self.module.namespaces.get(src_decl.func.namespace);
    _ = src_ns; // autofix
    var func = cranelift.Function.withNameSignature(
        .{
            .namespace = 0,
            .index = src_decl_ref.idx,
        },
        &signature,
    );

    var ctx = cranelift.FunctionBuilderContext.init();
    defer ctx.deinit();

    const builder = cranelift.FunctionBuilder.init(&func, &ctx);

    _ = builder; // autofix
    // defer builder.deinit();

    // const block0 = builder.createBlock();
    _ = src_def; // autofix
}

fn emitFileFn(path: []const u8, content: []const u8) !void {
    std.debug.print("path: {s}\n```\n{s}\n```", .{ path, content });
}
test "codegen" {
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
    var module = try IrGen.gen(std.testing.allocator, &ast);
    defer module.deinit();

    std.debug.print("{}", .{module});
    try emit(test_allocator, &module, .{
        .emitFileFn = emitFileFn,
        .entrypoint = "main",
    });
}
