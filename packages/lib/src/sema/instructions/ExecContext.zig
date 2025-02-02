const Sema = @import("../Sema.zig");
const std = @import("std");
const Self = @This();

builder: *Sema.Builder,
args: []const Sema.TypedValue,
values: std.AutoHashMapUnmanaged(Sema.Instruction.Index, Sema.TypedValue) = .{},
root_block: Sema.Instruction.Index,
indent: usize = 0,
allocator: std.mem.Allocator,
active_node: ?Sema.Instruction.Index = 0,

pub fn init(
    allocator: std.mem.Allocator,
    builder: *Sema.Builder,
    args: []const Sema.TypedValue,
    callee_value: Sema.TypedValue,
) Self {
    std.debug.print("{any}\n", .{callee_value.value});
    const fn_val: Sema.Value = builder.getValue(callee_value.value) orelse @panic("not a fn");
    const init_block_inst_index = fn_val.data.function.init orelse @panic("no init block");
    // const init_block_inst = builder.sema.instructions.items[init_block_inst_index];
    // const init_block_value = builder.getComplexValue(callee_value.value);
    // const instructions = builder.sema.lists.getSlice(init_block_inst.data.block.instructions_list);
    // _ = instructions; // autofix
    // const instructions = builder.sema.instructions.items[init_block_inst_index .. init_block_inst_index + init_block_inst.data.block.instructions_list.len];

    return .{
        .builder = builder,
        .args = args,
        .root_block = init_block_inst_index,
        .allocator = allocator,
        // .values = try builder.sema.allocator.alloc(?Sema.TypedValue, instructions.len),
    };
}

pub fn deinit(self: *Self) void {
    self.values.deinit(self.allocator);
}
pub fn getInstruction(self: *Self, index: Sema.Instruction.Index) Sema.Instruction {
    var instruction = self.builder.sema.instructions.items[self.root_block + index];

    instruction.typed_value = self.values.get(index) orelse return instruction;
    return instruction;
}
pub fn setInstruction(self: *Self, index: Sema.Instruction.Index, instruction: Sema.Instruction) !void {
    try self.values.put(self.allocator, index, instruction.typed_value);
}
pub fn setValue(self: *Self, index: Sema.Instruction.Index, value: Sema.TypedValue) !void {
    try self.values.put(self.allocator, index, value);
}

pub fn setIdMap(self: *Self, id: u32, index: Sema.Instruction.Index) !void {
    try self.values.put(self.allocator, index, Sema.TypedValue{
        .type = self.args[id].type,
        .value = self.args[id].value,
    });
}

pub fn getParamValue(self: *Self, index: u32) Sema.Value.Key {
    return self.args[index].value;
}
pub inline fn exec(self: *Self) !Sema.TypedValue {
    // var inst_context = self.getInstContext();
    std.debug.print("exec\n", .{});
    // try Index.exec(&inst_context, 0);
    try self.execInstruction(0);
    std.debug.print("exec done\n", .{});
    std.debug.assert(self.values.count() > 0);
    const value = self.values.get(0) orelse Sema.TypedValue.VOID;
    std.debug.print("value: {any}\n", .{self.builder.getFormattableTypedValue(value)});

    return value;
}

pub fn execInstruction(self: *Self, index: Sema.Instruction.Index) anyerror!void {
    const instruction = self.getInstruction(index);

    const stderr = std.io.getStdErr().writer();
    const indent = self.indent;
    if (indent > 1000) {
        std.debug.panic("indent too deep: {d}", .{indent});
    }
    self.indent += 1;
    try stderr.writeBytesNTimes("  ", indent);
    if (instruction.liveness == 0) {
        try stderr.print("<exec.{s}(%{d}) dead />\n", .{ @tagName(instruction.op), index });
        self.indent = indent;
        return;
    }
    try stderr.print("<exec.{s}(%{d})>\n", .{ @tagName(instruction.op), index });
    switch (instruction.op) {
        .constant => try @import("./constant.zig").exec(self, index),
        .block => try @import("./control-block.zig").exec(self, index),
        .add, .sub, .mul, .div => try @import("./arithmetic.zig").exec(self, index),
        .store => try @import("./store.zig").exec(self, index),
        .load => try @import("./load.zig").exec(self, index),
        .alloc => try @import("./alloc.zig").exec(self, index),
        .cast => try @import("./as.zig").exec(self, index),
        .loop => try @import("./control-loop.zig").exec(self, index),
        .eq, .ne, .lt, .le, .gt, .ge => try @import("./comparison.zig").exec(self, index),
        .br => try @import("./control-break.zig").exec(self, index),
        .ret => try @import("./control-return.zig").exec(self, index),
        .global_get => try @import("./global-get.zig").exec(self, index),
        .fn_call => try @import("./fncall.zig").exec(self, index),
        .param => try @import("./param.zig").exec(self, index),
        .param_set => try @import("./param-set.zig").exec(self, index),
        .param_get => try @import("./param-get.zig").exec(self, index),
        .@"if" => try @import("./control-if.zig").exec(self, index),
        .get_element_pointer => try @import("./get-element-ptr.zig").exec(self, index),
        .reinterpret => try @import("./reinterpret.zig").exec(self, index),

        else => |op| {
            std.debug.panic("unhandled op: {s}", .{@tagName(op)});
        },
    }
    try stderr.writeBytesNTimes("  ", indent);
    try stderr.print("</exec.{s}(%{d})>\n", .{ @tagName(instruction.op), index });
}
