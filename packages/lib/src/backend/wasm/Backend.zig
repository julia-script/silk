const Codegen = @import("../Codegen.zig");
const Sema = @import("../../sema/Sema.zig");
const Program = @import("../../program/Program.zig");
const std = @import("std");
const Self = @This();
const WasmBuilder = @import("./WasmBuilder.zig");
const Scope = @import("./instructions/Scope.zig");

program: *Program,
allocator: std.mem.Allocator,
module: WasmBuilder.Module,
pointer_type: WasmBuilder.Type,
stack_pointer: ?u32 = null,
main_memory: ?u32 = null,

pub fn init(program: *Program, allocator: std.mem.Allocator) Self {
    return Self{
        .allocator = allocator,
        .program = program,
        .module = WasmBuilder.Module.init(allocator),
        .pointer_type = switch (program.target.arch) {
            .wasm32 => .i32,
            .wasm64 => .i64,
        },
    };
}
pub fn deinit(self: *Self) void {
    self.module.deinit();
}
pub fn getStackPointer(self: *Self) !u32 {
    if (self.stack_pointer) |stack_pointer| return stack_pointer;

    var stack_pointer = try self.module.makeGlobal("__stack_pointer", self.pointer_type);
    stack_pointer.mutable = true;

    self.stack_pointer = @intCast(stack_pointer.index);

    try stack_pointer.pushInstruction(.{ .@"i32.const" = @intCast(
        std.math.pow(i32, 2, 10),
    ) });
    try stack_pointer.commit();

    return @intCast(stack_pointer.index);
}
pub fn getMainMemory(self: *Self) !u32 {
    if (self.main_memory) |main_memory| return main_memory;
    const memory = try self.module.makeMemory("memory", 1, null);
    self.main_memory = @intCast(memory);
    return @intCast(memory);
}
pub fn codegen(self: *Self) Codegen {
    return Codegen{
        .context = self,
        .program = self.program,
        .emitDeclarationFn = emitDeclarationFn,
        .deinitFn = deinitFn,
    };
}
fn deinitFn(ctx: *anyopaque) void {
    const self: *Self = @alignCast(@ptrCast(ctx));
    self.deinit();
}

fn emitDeclarationFn(ctx: *anyopaque, declaration_index: Sema.Declaration.Index) !void {
    std.debug.print("emitting {}\n", .{declaration_index});
    const self: *Self = @alignCast(@ptrCast(ctx));
    try self.emitDeclaration(declaration_index);
}
pub fn emitDeclaration(self: *Self, declaration_index: Sema.Declaration.Index) !void {
    const declaration = self.program.getDeclaration(declaration_index);

    const value = declaration.typed_value;
    switch (value.type) {
        .complex => |complex| switch (self.program.sema.builder.getComplexType(complex).data) {
            .function => try self.emitFunctionDeclaration(declaration_index),
            else => {
                @panic("can't emit non-function declaration");
            },
        },
        .simple => |simple| switch (simple) {
            else => {
                @panic("TODO: emit simple declaration");
            },
        },
    }
}

fn emitFunctionDeclaration(self: *Self, declaration_index: Sema.Declaration.Index) !void {
    const declaration = self.program.getDeclaration(declaration_index);

    var sema = self.program.sema;
    const fn_value = sema.getComplexValue(declaration.typed_value.value);
    const fn_type = sema.builder.getComplexType(declaration.typed_value.type);
    const name = sema.getSlice(declaration.name);
    const fn_init_instruction_index = fn_value.data.function.init orelse @panic("function init is null");
    const init_instruction = self.program.sema.getInitInstruction(fn_init_instruction_index).data.block;
    const init_instruction_list = self.program.sema.getList(init_instruction.instructions_list);
    _ = init_instruction_list; // autofix
    var func = try self.module.makeFunction();
    func.@"export" = declaration.exported;

    func.name = name;
    var scope = Scope.init(
        self.allocator,
        &func,
        self.program,
        sema.instructions.items[fn_init_instruction_index..],
        self,
    );
    defer scope.deinit();

    switch (fn_type.data.function.ret) {
        .simple => |simple| switch (simple) {
            .i8, .i16, .i32, .u8, .u16, .u32 => try func.pushResult(.i32),
            .i64, .u64 => try func.pushResult(.i64),
            .f32 => try func.pushResult(.f32),
            .f64 => try func.pushResult(.f64),
            .void => {},
            else => unreachable,
        },
        .complex => |complex| switch (complex) {
            else => unreachable,
        },
    }

    // const params = sema.getSlice(fn_type.data.function.params);
    // _ = params; // autofix
    // for (params) |param| {
    //     try  func.pushParam(param)
    //     // try scope.pushLocal(param.index, param.type);
    // }

    try scope.emitRoot();
    const func_a_index = try self.module.putFunction(&func);
    const stderr = std.io.getStdErr().writer().any();
    try self.module.toWat(stderr);
    try self.module.dumpBytes();

    _ = func_a_index; // autofix
    // for (init_instruction_list) |instruction_index| {
    //     const instruction = self.program.sema.getInstruction(fn_init_instruction_index, instruction_index);
    //     if (instruction.liveness == 0) continue;

    //     std.debug.print("%{} {s} {}\n", .{
    //         instruction_index,
    //         @tagName(instruction.op),
    //         self.program.sema.builder.getFormattableTypedValue(instruction.typed_value),
    //     });
    // }
}
