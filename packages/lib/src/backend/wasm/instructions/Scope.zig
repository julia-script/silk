const std = @import("std");
const WasmBuilder = @import("../WasmBuilder.zig");
const Program = @import("../../../program/Program.zig");
const Sema = @import("../../../sema/Sema.zig");
const Self = @This();

const Block = @import("./control-block.zig");
const Alloc = @import("./alloc.zig");
const Constant = @import("./constant.zig");
const Cast = @import("./cast.zig");
const Store = @import("./store.zig");
const Param = @import("./param.zig");
const LocalGet = @import("./local-get.zig");
const LocalSet = @import("./local-set.zig");
const Comparison = @import("./comparison.zig");
const ControlIf = @import("./control-if.zig");
const ControlLoop = @import("./control-loop.zig");
const Arithmetic = @import("./arithmetic.zig");
const Load = @import("./load.zig");
const Return = @import("./return.zig");
const Break = @import("./break.zig");
const GetElementPointer = @import("./get-element-pointer.zig");

const Backend = @import("../Backend.zig");

function: *WasmBuilder.Function,
program: *Program,
locals: std.AutoArrayHashMapUnmanaged(u64, Local) = .{},
instructions: []const Sema.Instruction,
allocator: std.mem.Allocator,
stack_alloc_size: u32 = 0,
has_fn_call: bool = false,
stack_pointer_local: ?u32 = null,
backend: *Backend,
const Local = struct {
    index: u32,
    name: []const u8,
};

pub fn init(allocator: std.mem.Allocator, function: *WasmBuilder.Function, program: *Program, instructions: []const Sema.Instruction, backend: *Backend) Self {
    return Self{
        .function = function,
        .program = program,
        .instructions = instructions,
        .allocator = allocator,
        .backend = backend,
    };
}

pub fn deinit(self: *Self) void {
    self.locals.deinit(self.allocator);
}
pub fn getInstruction(self: *Self, index: Sema.Instruction.Index) Sema.Instruction {
    return self.instructions[index];
}
pub fn pushLocal(self: *Self, index: ?Sema.Instruction.Index, local_name: []const u8, ty: WasmBuilder.Type) !u32 {
    const local_index = try self.function.pushLocal(local_name, ty);
    if (index) |i| {
        try self.locals.put(self.allocator, i, .{ .index = local_index, .name = local_name });
    }
    return local_index;
}
pub fn setLocal(self: *Self, index: Sema.Instruction.Index, value: Local) !void {
    try self.locals.put(self.allocator, index, value);
}
pub fn getLocal(self: *Self, index: Sema.Instruction.Index) Local {
    return self.locals.get(index).?;
}

pub fn emitRoot(self: *Self) Error!void {
    try self.function.pushFormatting(.{ .comment = "-- Body --" });
    try self.emit(0);
    // Emit the prologue
    if (self.stack_alloc_size == 0) return;

    const stack_pointer = try self.backend.getStackPointer();
    const base_pointer = self.stack_pointer_local orelse unreachable;

    try self.function.pushPrologueFormatting(.{ .comment = "-- Prologue --" });
    const old_sp = try self.function.pushLocal("__old_stack_pointer", .i32);
    try self.function.pushPrologue(.{ .@"global.get" = stack_pointer });
    try self.function.pushPrologue(.{ .@"local.tee" = old_sp });
    try self.function.pushPrologue(.{ .@"i32.const" = @intCast(self.stack_alloc_size) });
    try self.function.pushPrologue(.{ .@"i32.sub" = {} });
    try self.function.pushPrologue(.{ .@"local.tee" = base_pointer });
    try self.function.pushPrologue(.{ .@"global.set" = stack_pointer });
    try self.function.pushPrologueFormatting(.linebreak);

    // Reset the stack pointer
    try self.function.pushFormatting(.linebreak);
    try self.function.pushFormatting(.{ .comment = "-- Epilogue --" });
    try self.function.pushInstruction(.{ .@"local.get" = old_sp });
    try self.function.pushInstruction(.{ .@"global.set" = stack_pointer });

    // try try self.function.pushEpilogue(.{ .global_set = stack_pointer });
}

const Error = error{} || std.mem.Allocator.Error;
pub fn emit(self: *Self, index: Sema.Instruction.Index) Error!void {
    const instruction = self.getInstruction(index);
    if (instruction.liveness == 0) return;
    const stderr = std.io.getStdErr().writer().any();
    std.debug.print("emit %{}: ", .{index});
    self.program.sema.formatInstruction(stderr, .{
        .instruction = .{ .data = instruction, .index = index },
    }) catch {};
    std.debug.print("\n", .{});
    if (instruction.op == .block) {
        return try Block.emit(self, index);
    }

    var header = std.BoundedArray(u8, 1024).init(0) catch unreachable;
    const header_writer = header.writer().any();
    // header_writer.writeAll(" ") catch unreachable;
    self.program.sema.formatInstruction(header_writer, .{
        .instruction = .{ .data = instruction, .index = index },
    }) catch {};
    try self.function.pushFormatting(.{ .comment = header.slice() });

    switch (instruction.op) {
        .alloc => try Alloc.emit(self, index),
        .constant => try Constant.emit(self, index),
        .cast => {
            if (instruction.typed_value.isComptimeKnown()) {
                try Constant.emit(self, index);
            } else {
                try Cast.emit(self, index);
            }
        },
        .store => try Store.emit(self, index),
        .param => try Param.emit(self, index),
        .param_get => try LocalGet.emit(self, index),
        .param_set => try LocalSet.emit(self, index),
        .gt, .ge, .lt, .le, .eq, .ne => try Comparison.emit(self, index),
        .@"if" => try ControlIf.emit(self, index),
        .load => try Load.emit(self, index),
        .ret => try Return.emit(self, index),
        .loop => try ControlLoop.emit(self, index),
        .add, .sub, .mul, .div => try Arithmetic.emit(self, index),
        .br => try Break.emit(self, index),
        .get_element_pointer => try GetElementPointer.emit(self, index),
        else => |op| {
            std.debug.panic("unsupported instruction: {}\n", .{op});
        },
    }
}
