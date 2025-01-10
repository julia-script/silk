const std = @import("std");
const Sema = @import("../Sema.zig");
const Hir = @import("../../hir.zig");
const Index = @import("./inst-index.zig");

builder: *Sema.Builder,
depth: usize = 0,
context: *anyopaque,
getInstructionFn: *const fn (context: *anyopaque, index: Sema.Instruction.Index) Sema.Instruction,
setInstructionFn: *const fn (context: *anyopaque, index: Sema.Instruction.Index, instruction: Sema.Instruction) void,
getParamValueFn: *const fn (context: *anyopaque, index: u32) Sema.Value.Key,
setIdMapFn: *const fn (context: *anyopaque, id: u32, index: Sema.Instruction.Index) void,
goToFn: *const fn (context: *anyopaque, index: Sema.Instruction.Index) void,
is_comptime: bool = false,
indent: usize = 0,
pushInstructionFn: *const fn (context: *anyopaque, id: ?u32, instruction: Sema.Instruction) Sema.Instruction.Index,
active_node: ?Sema.Instruction.Index = 0,
// returned: bool = false,

const Self = @This();

pub fn getInstruction(self: *@This(), index: Sema.Instruction.Index) Sema.Instruction {
    return self.getInstructionFn(self.context, index);
}
pub fn getParamValue(self: *@This(), index: u32) Sema.Value.Key {
    return self.getParamValueFn(self.context, index);
}

pub fn getTypedValue(self: *@This(), index: Sema.Instruction.Index) Sema.TypedValue {
    return self.getInstruction(index).typed_value;
}
pub fn setIdMap(self: *@This(), id: u32, index: Sema.Instruction.Index) void {
    self.setIdMapFn(self.context, id, index);
}
pub fn setInstruction(self: *@This(), index: Sema.Instruction.Index, instruction: Sema.Instruction) void {
    const stderr = std.io.getStdErr().writer();

    self.setInstructionFn(self.context, index, instruction);
    stderr.writeBytesNTimes("  ", self.indent + 1) catch {};
    stderr.print("[SET] %{d}: {s} ", .{ index, @tagName(instruction.op) }) catch {};
    stderr.print("({s})\n", .{self.builder.getFormattableTypedValue(instruction.typed_value)}) catch {};
}
pub fn execInstruction(self: *@This(), index: Sema.Instruction.Index) void {
    Index.exec(self, index) catch {};
}
pub fn pushWithoutExec(self: *@This(), id: u32, instruction: Sema.Instruction) Sema.Instruction.Index {
    return self.pushInstructionFn(self.context, id, instruction);
}

pub fn pushInstruction(self: *@This(), id: ?u32, instruction: Sema.Instruction) Sema.Instruction.Index {
    const index = self.pushInstructionFn(self.context, id, instruction);
    const stderr = std.io.getStdErr().writer();
    stderr.writeBytesNTimes("  ", self.indent + 1) catch {};
    stderr.print("[PUSH] %{d}:", .{index}) catch {};
    self.builder.sema.formatInstruction(stderr.any(), .{ .instruction = instruction }) catch {};
    // stderr.print("({s})\n", .{self.builder.getFormattableTypedValue(instruction.typed_value)}) catch {};
    stderr.print("\n", .{}) catch {};
    // self.execInstruction(index);

    return index;
}

pub fn goTo(self: *@This(), index: Sema.Instruction.Index) void {
    const stderr = std.io.getStdErr().writer();

    stderr.writeBytesNTimes("  ", self.indent + 1) catch {};
    const instruction = self.getInstruction(index);
    stderr.print("[GOTO] %{d}: {s}\n", .{ index, @tagName(instruction.op) }) catch {};
    self.active_node = index;
    self.depth += 1;
    if (self.depth >= 30) {
        @panic("reached max depth");
    }

    // if (self.is_comptime) {
    //     self.execInstruction(index);
    // }
}

pub fn setValue(self: *@This(), index: Sema.Instruction.Index, value: Sema.TypedValue) void {
    var instruction = self.getInstruction(index);
    instruction.typed_value = value;
    self.setInstruction(index, instruction);
}
pub fn setData(self: *@This(), index: Sema.Instruction.Index, data: Sema.Instruction.Data) void {
    var instruction = self.getInstruction(index);
    instruction.data = data;
    self.setInstruction(index, instruction);
}

pub fn markDead(self: *@This(), index: Sema.Instruction.Index) void {
    var instruction = self.getInstruction(index);
    instruction.liveness = 0;
    self.setInstruction(index, instruction);
}

// pub fn gen(hir_inst_index: Hir.Inst.Index) !usize {
//     _ = hir_inst_index; // autofix
//     return 0;
// }

pub fn pushMaybeCastInstruction(
    self: *@This(),
    hir_inst_index: Hir.Inst.Index,
    instruction_index: Sema.Instruction.Index,
    type_inst_index: Sema.Instruction.Index,
) !Sema.Instruction.Index {
    const type_inst = self.getInstruction(type_inst_index);
    const type_index = self.builder.unwrapTypeValue(type_inst.typed_value.value);
    if (try self.pushMaybeCastInstructionToType(hir_inst_index, instruction_index, type_index)) |index| {
        self.markDead(type_inst_index);
        return index;
    }
    return instruction_index;
}
pub fn pushMaybeCastInstructionToType(
    self: *@This(),
    hir_inst_index: ?Hir.Inst.Index,
    instruction_index: Sema.Instruction.Index,
    type_index: Sema.Type.Key,
) !?Sema.Instruction.Index {
    const instruction = self.getInstruction(instruction_index);

    if (self.builder.getType(type_index)) |expected_type| {
        switch (expected_type.data) {
            .any => {
                if (self.builder.getType(instruction.typed_value.type)) |inst_type| {
                    switch (inst_type.data) {
                        .any => return null,
                        else => {},
                    }
                }
                return try self.pushCastInstruction(hir_inst_index, instruction_index, try self.builder.internTypeData(.{ .any = .{
                    .concrete = instruction.typed_value.type,
                } }));
            },
            .flat_union => {
                const fields = self.builder.sema.lists.getSlice(expected_type.data.flat_union.fields);
                var active_field_index: ?Sema.Type.Key = null;
                for (fields) |field| {
                    const field_key = Sema.Type.Key.decode(field);
                    switch (self.builder.canCastImplicitly(instruction.typed_value.type, field_key) catch |err| {
                        std.debug.panic("{s}", .{@errorName(err)});
                    }) {
                        .unnecessary => {
                            active_field_index = field_key;
                            break;
                        },
                        .allowed => {
                            active_field_index = field_key;
                        },
                        .not_allowed => {},
                    }
                }

                if (active_field_index) |field_key| {
                    return self.pushInstruction(hir_inst_index, .{
                        .op = .cast,
                        .typed_value = .{
                            .type = type_index,
                            .value = try self.builder.internValueData(.{ .flat_union = .{
                                .active_field = .{ .resolved = .{
                                    .type = field_key,
                                    .value = instruction.typed_value.value,
                                } },
                            } }),
                        },
                        .data = .{ .operand = instruction_index },
                    });
                }

                std.debug.panic("TODO: pushMaybeCastInstructionToType", .{});
            },
            else => {},
        }
    }
    const can_cast = self.builder.canCastImplicitly(instruction.typed_value.type, type_index) catch |err| {
        std.debug.panic("{s}", .{@errorName(err)});
    };
    switch (can_cast) {
        .allowed => return try self.pushCastInstruction(hir_inst_index, instruction_index, type_index),
        .unnecessary => return instruction_index,
        .not_allowed => return null,
    }
}
pub fn pushCastInstruction(
    self: *@This(),
    hir_inst_index: ?Hir.Inst.Index,
    instruction_index: Sema.Instruction.Index,
    type_index: Sema.Type.Key,
) !Sema.Instruction.Index {
    const value_inst = self.getInstruction(instruction_index);
    const typed_value = try self.maybeCoerceValue(value_inst.typed_value, type_index);

    std.debug.assert(!type_index.isEqualSimple(.type));
    if (value_inst.typed_value.isComptimeKnown()) {
        self.markDead(instruction_index);
    }
    return self.pushInstruction(hir_inst_index, .{
        .op = .cast,
        .typed_value = .{
            .type = typed_value.type,
            .value = typed_value.value,
        },
        .data = .{ .operand = instruction_index },
    });
}
pub fn maybeCoerceValue(self: *@This(), typed_value: Sema.TypedValue, type_index: Sema.Type.Key) !Sema.TypedValue {
    // const ty = switch (type_index) {
    //     .simple => |simple| simple,
    //     .complex => return typed_value,
    // };
    switch (typed_value.value) {
        .simple => |simple| switch (simple) {
            .exec_time => return typed_value,
            .runtime => return typed_value,
            else => {},
        },
        else => {},
    }
    return try self.builder.convertNumberType(typed_value, type_index);
}

test "InstContext" {
    // const CustomBuilder = struct {
    //     a: u32 = 1,
    //     // pub fn getInstructionAny(self: *anyopaque, index: Sema.Instruction.Index) Sema.Instruction {
    //     //     _ = self; // autofix
    //     //     return index + 2;
    //     // }
    //     // pub fn builder(self: *@This()) Self {
    //     //     return Self{
    //     //         .context = @ptrCast(self),
    //     //         .getInstructionFn = getInstructionAny,
    //     //     };
    //     // }
    // };
    // var builder = CustomBuilder{};
    // var ctx = builder.builder();
    // std.debug.print("ctx: {any}\n", .{ctx.context});
    // std.debug.print("ctx: {}\n", .{ctx.getInstruction(1)});
}
