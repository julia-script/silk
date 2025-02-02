const std = @import("std");
const Sema = @import("../Sema.zig");
const Hir = @import("../../Hir.zig");
const Index = @import("./inst-index.zig");
const Entity = @import("../gen.zig").Entity;

allocator: std.mem.Allocator,
builder: *Sema.Builder,
entity_key: Entity.Key,
depth: usize = 0,
context: *anyopaque,
instructions: std.ArrayListUnmanaged(Sema.Instruction) = .{},
getInstructionFn: *const fn (context: *anyopaque, index: Sema.Instruction.Index) Sema.Instruction,
setInstructionFn: *const fn (context: *anyopaque, index: Sema.Instruction.Index, instruction: Sema.Instruction) void,
getParamValueFn: *const fn (context: *anyopaque, index: u32) Sema.Value.Key,
setIdMapFn: *const fn (context: *anyopaque, id: u32, index: Sema.Instruction.Index) void,
goToFn: *const fn (context: *anyopaque, index: Sema.Instruction.Index) void,
indent: usize = 0,
pushInstructionFn: *const fn (context: *anyopaque, id: ?u32, instruction: Sema.Instruction) Sema.Instruction.Index,
active_node: ?Sema.Instruction.Index = 0,
instructions_by_hir_inst: std.AutoArrayHashMapUnmanaged(Hir.Inst.Index, Sema.Instruction.Index) = .{},
root_hir_index: Hir.Inst.Index,
// block_by_hir_index

const Self = @This();
pub fn getEntity(self: *Self) *Entity {
    return self.builder.getEntity(self.entity_key);
}
pub fn getHirInstruction(self: *Self, hir_inst_index: Hir.Inst.Index) Hir.Inst {
    return self.getEntity().getHirInstruction(hir_inst_index);
}
pub const Block = struct {
    // index: Sema.Instruction.Index,

    instruction_list: std.ArrayListUnmanaged(Sema.Instruction.Index) = .{},
    ctx: *Self,
    parent: ?*Block,
    is_comptime: bool,
    hir_inst_index: Hir.Inst.Index,
    depth: usize = 0,
    done: bool = false,

    pub fn makeChild(self: *Block, hir_inst_index: Hir.Inst.Index) !Block {
        return Block{

            // .index = try self.ctx.appendInstruction(hir_inst_index, .{
            //     .op = .block,
            //     .typed_value = .{
            //         .type = Sema.Type.simple(.void),
            //         .value = Sema.Value.simple(.void),
            //     },
            //     .data = .{ .block = .{
            //         .instructions_list = Sema.Instruction.List.empty,
            //         .is_comptime = self.is_comptime or is_comptime,
            //     } },
            // }),
            .ctx = self.ctx,
            .parent = self,
            .is_comptime = self.is_comptime,
            .hir_inst_index = hir_inst_index,
            .depth = self.depth + 1,
            .done = false,
        };
    }
    pub fn breakTo(self: *Block, target: Hir.Inst.Index, typed_value: Sema.TypedValue) void {
        var curr = self;
        while (curr.parent) |parent| {
            curr.done = true;
            if (curr.hir_inst_index == target) {
                break;
            }
            curr = parent;
        }

        const index = curr.ctx.getInstructionByHirIndex(curr.hir_inst_index);
        const curr_typed_value = curr.ctx.getInstruction(index).typed_value;
        if (!curr_typed_value.type.isEqualSimple(.void) and !typed_value.type.isEqual(curr_typed_value.type)) {
            std.debug.print("Type mismatch, expected {} got {}", .{ self.ctx.builder.getFormattableType(typed_value.type), self.ctx.builder.getFormattableType(curr_typed_value.type) });
        }
        curr.ctx.setValue(index, typed_value);
    }

    pub fn emit(self: *Block) !Sema.Instruction.Index {
        const index = try self.emitInstruction(self.hir_inst_index);
        const instruction = self.ctx.getInstruction(index);
        if (instruction.op == .block) {
            return index;
        }

        return index;
    }
    pub fn appendInstruction(self: *Block, id: ?u32, instruction: Sema.Instruction) !Sema.Instruction.Index {
        const index = try self.ctx.appendInstruction(id, instruction);
        const stderr = std.io.getStdErr().writer();
        stderr.writeBytesNTimes("  ", self.ctx.indent + 1) catch {};
        stderr.print("[APPEND]", .{}) catch {};
        self.ctx.builder.sema.formatInstruction(stderr.any(), .{ .instruction = .{ .data = instruction, .index = index } }) catch {};
        stderr.print("\n", .{}) catch {};

        try self.instruction_list.append(self.ctx.allocator, index);

        return index;
    }
    pub const Error = error{
        CircularDependency,
        SymbolNotFound,
        NoSpaceLeft,
        SymbolAlreadyExists,
        Overflow,
    } || std.mem.Allocator.Error || std.fmt.ParseIntError || std.fmt.ParseFloatError || std.io.AnyWriter.Error;

    pub fn emitInstruction(self: *Block, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
        const hir_inst = self.ctx.getHirInstruction(hir_inst_index);
        const stderr = std.io.getStdErr().writer();
        stderr.writeBytesNTimes("  ", self.ctx.indent) catch {};
        stderr.print("<emit.{s} hir({d})>\n", .{ @tagName(hir_inst), hir_inst_index }) catch {};
        // self.ctx.depth += 1;
        const index: Sema.Instruction.Index = switch (hir_inst) {
            .block, .inline_block => try @import("./control-block.zig").emit(self, hir_inst_index),
            .ty_number,
            .ty_boolean,
            .ty_i8,
            .ty_i16,
            .ty_i32,
            .ty_i64,
            .ty_u8,
            .ty_u16,
            .ty_u32,
            .ty_u64,
            .ty_usize,
            .ty_array,
            .ty_f32,
            .ty_f64,
            .ty_void,
            .ty_pointer,
            .ty_global,
            => try @import("./type-literal.zig").emit(self, hir_inst_index),
            .param => try @import("./param.zig").emit(self, hir_inst_index),
            .struct_decl => try @import("./struct-decl.zig").emit(self, hir_inst_index),
            .br => try @import("./control-break.zig").emit(self, hir_inst_index),

            .number_literal,
            .char_literal,
            .boolean_literal,
            .string_literal,
            => try @import("./literal.zig").emit(self, hir_inst_index),
            .alloc => try @import("./alloc.zig").emit(self, hir_inst_index),
            .param_get => try @import("./param-get.zig").emit(self, hir_inst_index),
            .param_set => try @import("./param-set.zig").emit(self, hir_inst_index),
            .eq, .ne, .lt, .le, .gt, .ge => try @import("./comparison.zig").emit(self, hir_inst_index),
            .if_expr => try @import("./control-if.zig").emit(self, hir_inst_index),
            .load => try @import("./load.zig").emit(self, hir_inst_index),
            .store => try @import("./store.zig").emit(self, hir_inst_index),
            .ret => try @import("./control-return.zig").emit(self, hir_inst_index),
            .loop => try @import("./control-loop.zig").emit(self, hir_inst_index),
            .add, .sub, .mul, .div => try @import("./arithmetic.zig").emit(self, hir_inst_index),
            .as => try @import("./as.zig").emit(self, hir_inst_index),
            .array_init => try @import("./array-init.zig").emit(self, hir_inst_index),
            .field_init => try @import("./field-init.zig").emit(self, hir_inst_index),
            .global_get => try @import("./global-get.zig").emit(self, hir_inst_index),
            .type_init => try @import("./type-init.zig").emit(self, hir_inst_index),
            .typeof => try @import("./typeof.zig").emit(self, hir_inst_index),
            .get_element_pointer, .get_property_pointer => try @import("./get-element-ptr.zig").emit(self, hir_inst_index),
            .fn_call => try @import("./fncall.zig").emit(self, hir_inst_index),
            else => {
                std.debug.panic("unhandled hir inst: {s}\n", .{@tagName(std.meta.activeTag(hir_inst))});
            },
        };

        // self.ctx.depth -= 1;
        stderr.writeBytesNTimes("  ", self.ctx.indent) catch {};
        stderr.print("<emit.{s} hir({d})/>\n", .{ @tagName(hir_inst), hir_inst_index }) catch {};
        return index;
    }

    pub fn markDeadIfComptimeKnown(self: *Block, index: Sema.Instruction.Index) void {
        const instruction = self.ctx.getInstruction(index);
        if (instruction.typed_value.isComptimeKnown()) {
            self.ctx.markDead(index);
        }
    }

    pub fn pushMaybeCastInstruction(
        self: *Block,
        hir_inst_index: Hir.Inst.Index,
        instruction_index: Sema.Instruction.Index,
        type_inst_index: Sema.Instruction.Index,
    ) !Sema.Instruction.Index {
        const type_inst = self.ctx.getInstruction(type_inst_index);
        const type_index = self.ctx.builder.unwrapTypeValue(type_inst.typed_value.value);
        if (try self.pushMaybeCastInstructionToType(hir_inst_index, instruction_index, type_index)) |index| {
            self.ctx.markDead(type_inst_index);
            return index;
        }
        return instruction_index;
    }
    pub fn pushMaybeCastInstructionToType(
        self: *Block,
        hir_inst_index: ?Hir.Inst.Index,
        instruction_index: Sema.Instruction.Index,
        type_index: Sema.Type.Key,
    ) !?Sema.Instruction.Index {
        const instruction = self.ctx.getInstruction(instruction_index);

        if (self.ctx.builder.getType(type_index)) |expected_type| {
            switch (expected_type.data) {
                .any => {
                    if (self.ctx.builder.getType(instruction.typed_value.type)) |inst_type| {
                        switch (inst_type.data) {
                            .any => return null,
                            else => {},
                        }
                    }
                    return try self.pushCastInstruction(hir_inst_index, instruction_index, try self.ctx.builder.internTypeData(.{ .any = .{
                        .concrete = instruction.typed_value.type,
                    } }));
                },
                .flat_union => {
                    const fields = self.ctx.builder.sema.lists.getSlice(expected_type.data.flat_union.fields);
                    var active_field_index: ?Sema.Type.Key = null;
                    for (fields) |field| {
                        const field_key = Sema.Type.Key.decode(field);
                        switch (self.ctx.builder.canCastImplicitly(instruction.typed_value.type, field_key) catch |err| {
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
                        return try self.appendInstruction(hir_inst_index, .{
                            .op = .cast,
                            .typed_value = .{
                                .type = type_index,
                                .value = try self.ctx.builder.internValueData(.{ .flat_union = .{
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
        const can_cast = self.ctx.builder.canCastImplicitly(instruction.typed_value.type, type_index) catch |err| {
            std.debug.panic("{s}", .{@errorName(err)});
        };
        switch (can_cast) {
            .allowed => return try self.pushCastInstruction(hir_inst_index, instruction_index, type_index),
            .unnecessary => return instruction_index,
            .not_allowed => return null,
        }
    }
    pub fn pushCastInstruction(
        self: *Block,
        hir_inst_index: ?Hir.Inst.Index,
        instruction_index: Sema.Instruction.Index,
        type_index: Sema.Type.Key,
    ) !Sema.Instruction.Index {
        const value_inst = self.ctx.getInstruction(instruction_index);
        const typed_value = try self.ctx.maybeCoerceValue(value_inst.typed_value, type_index);

        std.debug.assert(!type_index.isEqualSimple(.type));
        if (value_inst.typed_value.isComptimeKnown()) {
            self.ctx.markDead(instruction_index);
        }
        return try self.appendInstruction(hir_inst_index, .{
            .op = .cast,
            .typed_value = .{
                .type = typed_value.type,
                .value = typed_value.value,
            },
            .data = .{ .operand = instruction_index },
        });
    }
};

// returned: bool = false,

// pub fn makeBlock(self: *Self, parent: ?*Block, hir_inst_index: Hir.Inst.Index, is_comptime: bool) Block {
//     return Block{
//         .instruction_list = self.builder.sema.lists.new(),
//         .ctx = self,
//         .parent = parent,
//         .is_comptime = is_comptime,
//         .hir_inst_index = hir_inst_index,
//     };
// }
pub fn emitRoot(self: *Self, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    var block = Block{
        // .index = try self.appendInstruction(hir_inst_index, .{
        //     .op = .block,
        //     .typed_value = .{
        //         .type = Sema.Type.simple(.void),
        //         .value = Sema.Value.simple(.void),
        //     },
        //     .data = .{ .block = .{
        //         .instructions_list = Sema.Instruction.List.empty,
        //         .is_comptime = false,
        //     } },
        // }),
        .ctx = self,
        .parent = null,
        .is_comptime = false,
        .hir_inst_index = hir_inst_index,
    };

    const base_index = try block.emit();
    const index: Sema.Instruction.Index = self.builder.sema.instructions.items.len;

    std.debug.assert(base_index == 0);

    // std.debug.print("test {}\n", .{self.builder.sema.instructions.items.len});
    // std.debug.assert(index == self.builder.sema.instructions.items.len);

    try self.builder.sema.instructions.appendSlice(self.builder.sema.allocator, self.instructions.items);

    // std.debug.print("emitted\n", .{});
    const stderr = std.io.getStdErr().writer().any();

    self.builder.sema.formatInstruction(stderr, .{
        .index = index,
    }) catch {};
    return index;
}
pub fn getInstruction(self: *@This(), index: Sema.Instruction.Index) Sema.Instruction {
    // return self.getInstructionFn(self.context, index);
    return self.instructions.items[index];
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

    // self.setInstructionFn(self.context, index, instruction);
    self.instructions.items[index] = instruction;
    stderr.writeBytesNTimes("  ", self.indent + 1) catch {};
    stderr.print("[SET] %{d}: {s} ", .{ index, @tagName(instruction.op) }) catch {};
    stderr.print("({s})\n", .{self.builder.getFormattableTypedValue(instruction.typed_value)}) catch {};
}
pub fn execInstruction(ctx: *Self, index: Sema.Instruction.Index) void {
    Index.exec(ctx, index) catch {};
}
pub fn pushWithoutExec(self: *@This(), id: u32, instruction: Sema.Instruction) Sema.Instruction.Index {
    std.debug.panic("deprecated", .{});
    return self.pushInstructionFn(self.context, id, instruction);
}

pub fn pushInstructionToContext(self: *@This(), id: ?u32, instruction: Sema.Instruction) Sema.Instruction.Index {
    std.debug.panic("deprecated", .{});
    const index = self.pushInstructionFn(self.context, id, instruction);
    const stderr = std.io.getStdErr().writer();
    stderr.writeBytesNTimes("  ", self.indent + 1) catch {};
    stderr.print("[PUSH]", .{}) catch {};
    self.builder.sema.formatInstruction(stderr.any(), .{ .instruction = .{ .data = instruction, .index = index } }) catch {};
    // stderr.print("({s})\n", .{self.builder.getFormattableTypedValue(instruction.typed_value)}) catch {};
    stderr.print("\n", .{}) catch {};
    // self.execInstruction(index);

    return index;
}
pub fn appendInstruction(self: *@This(), id: ?u32, instruction: Sema.Instruction) !Sema.Instruction.Index {
    const index: Sema.Instruction.Index = @intCast(self.instructions.items.len);
    try self.instructions.append(self.allocator, instruction);
    if (id) |_id| {
        try self.instructions_by_hir_inst.put(self.allocator, _id, index);
    }
    return index;
}
pub fn getInstructionByHirIndex(self: *@This(), hir_inst_index: Hir.Inst.Index) Sema.Instruction.Index {
    return self.instructions_by_hir_inst.get(hir_inst_index) orelse
        std.debug.panic("instruction not found for hir_inst_index: {d}", .{hir_inst_index});
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
pub fn markDeadIfComptimeKnown(self: *@This(), index: Sema.Instruction.Index) void {
    const instruction = self.getInstruction(index);
    if (instruction.typed_value.isComptimeKnown()) {
        self.markDead(index);
    }
}

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
// pub fn pushMaybeCastInstructionToType(
//     self: *@This(),
//     hir_inst_index: ?Hir.Inst.Index,
//     instruction_index: Sema.Instruction.Index,
//     type_index: Sema.Type.Key,
// ) !?Sema.Instruction.Index {
//     const instruction = self.getInstruction(instruction_index);

//     if (self.builder.getType(type_index)) |expected_type| {
//         switch (expected_type.data) {
//             .any => {
//                 if (self.builder.getType(instruction.typed_value.type)) |inst_type| {
//                     switch (inst_type.data) {
//                         .any => return null,
//                         else => {},
//                     }
//                 }
//                 return try self.pushCastInstruction(hir_inst_index, instruction_index, try self.builder.internTypeData(.{ .any = .{
//                     .concrete = instruction.typed_value.type,
//                 } }));
//             },
//             .flat_union => {
//                 const fields = self.builder.sema.lists.getSlice(expected_type.data.flat_union.fields);
//                 var active_field_index: ?Sema.Type.Key = null;
//                 for (fields) |field| {
//                     const field_key = Sema.Type.Key.decode(field);
//                     switch (self.builder.canCastImplicitly(instruction.typed_value.type, field_key) catch |err| {
//                         std.debug.panic("{s}", .{@errorName(err)});
//                     }) {
//                         .unnecessary => {
//                             active_field_index = field_key;
//                             break;
//                         },
//                         .allowed => {
//                             active_field_index = field_key;
//                         },
//                         .not_allowed => {},
//                     }
//                 }

//                 if (active_field_index) |field_key| {
//                     return self.pushInstruction(hir_inst_index, .{
//                         .op = .cast,
//                         .typed_value = .{
//                             .type = type_index,
//                             .value = try self.builder.internValueData(.{ .flat_union = .{
//                                 .active_field = .{ .resolved = .{
//                                     .type = field_key,
//                                     .value = instruction.typed_value.value,
//                                 } },
//                             } }),
//                         },
//                         .data = .{ .operand = instruction_index },
//                     });
//                 }

//                 std.debug.panic("TODO: pushMaybeCastInstructionToType", .{});
//             },
//             else => {},
//         }
//     }
//     const can_cast = self.builder.canCastImplicitly(instruction.typed_value.type, type_index) catch |err| {
//         std.debug.panic("{s}", .{@errorName(err)});
//     };
//     switch (can_cast) {
//         .allowed => return try self.pushCastInstruction(hir_inst_index, instruction_index, type_index),
//         .unnecessary => return instruction_index,
//         .not_allowed => return null,
//     }
// }
// // pub fn pushCastInstruction(
// //     self: *@This(),
// //     hir_inst_index: ?Hir.Inst.Index,
// //     instruction_index: Sema.Instruction.Index,
// //     type_index: Sema.Type.Key,
// // ) !Sema.Instruction.Index {
// //     const value_inst = self.getInstruction(instruction_index);
// //     const typed_value = try self.maybeCoerceValue(value_inst.typed_value, type_index);

// //     std.debug.assert(!type_index.isEqualSimple(.type));
// //     if (value_inst.typed_value.isComptimeKnown()) {
// //         self.markDead(instruction_index);
// //     }
// //     return self.pushInstruction(hir_inst_index, .{
// //         .op = .cast,
// //         .typed_value = .{
// //             .type = typed_value.type,
// //             .value = typed_value.value,
// //         },
// //         .data = .{ .operand = instruction_index },
// //     });
// // }
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

pub fn getReturnType(self: *@This()) !Sema.Type.Key {
    var entity = self.getEntity();
    const ty = try entity.resolveType();
    const complex_type = self.builder.getComplexType(ty);
    return complex_type.data.function.ret;
}
// // pub fn getInstructionAsTypeByHirInst(self: *@This(), hir_inst_index: Hir.Inst.Index, type_index: Sema.Type.Key) Error!Sema.Instruction.Index {
// //     // const trace = self.builder.tracer.begin(
// //     //     @src(),
// //     //         .{ "getInstructionAsTypeByHirInst", "Scope.getInstructionAsTypeByHirInst({s})", .{
// //     //             try formatHirIndex(self.entity.getHir(), hir_inst_index),
// //     //         } },
// //     //         .{
// //     //             .before = self.instructions.items,
// //     //         },
// //     //     );
// //     //     defer trace.end(.{
// //     //         .instructions = self.instructions.items,
// //     //     });
// //     const instruction_index = self.getInstructionIndex(hir_inst_index);

// //     return (try self.pushMaybeCastInstructionToType(
// //         hir_inst_index,
// //         instruction_index,
// //         type_index,
// //     )) orelse instruction_index;
// // }
// test "InstContext" {
//     // const CustomBuilder = struct {
//     //     a: u32 = 1,
//     //     // pub fn getInstructionAny(self: *anyopaque, index: Sema.Instruction.Index) Sema.Instruction {
//     //     //     _ = self; // autofix
//     //     //     return index + 2;
//     //     // }
//     //     // pub fn builder(self: *@This()) Self {
//     //     //     return Self{
//     //     //         .context = @ptrCast(self),
//     //     //         .getInstructionFn = getInstructionAny,
//     //     //     };
//     //     // }
//     // };
//     // var builder = CustomBuilder{};
//     // var ctx = builder.builder();
//     // std.debug.print("ctx: {any}\n", .{ctx.context});
//     // std.debug.print("ctx: {}\n", .{ctx.getInstruction(1)});
// }
