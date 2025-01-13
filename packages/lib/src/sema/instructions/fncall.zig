const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
const Index = @import("./inst-index.zig");
const Error = @import("../gen.zig").Error;
const Scope = @import("../gen.zig").Scope;

pub const SignatureCheckResult = struct {
    args_list: Sema.Instruction.List,
    all_args_comptime_known: bool,
    ctx: *InstContext,

    pub fn getArg(self: *SignatureCheckResult, arg_index: usize) Sema.Instruction.Index {
        const list = self.ctx.builder.sema.lists.getSlice(self.args_list);
        return list[arg_index];
    }
    pub fn getArgInstruction(self: *SignatureCheckResult, arg_index: usize) Sema.Instruction {
        return self.ctx.getInstruction(self.getArg(arg_index));
    }
};
pub fn checkSignaturae(ctx: *InstContext, scope: *GenScope, type_params: []const Sema.Type.Key, hir_args_list_range: Hir.Inst.List, comptime cast_if_needed: bool) Error!SignatureCheckResult {
    if (type_params.len != hir_args_list_range.len) {
        std.debug.panic("error: function has {d} params but {d} args were provided", .{ type_params.len, hir_args_list_range.len });
    }
    const hir_args_list = scope.entity.getHirList(hir_args_list_range);

    var args_list = ctx.builder.newList();
    var all_args_comptime_known = true;
    for (type_params, hir_args_list) |type_param, hir_arg_index| {
        var arg_inst_index: Sema.Instruction.Index = undefined;

        if (cast_if_needed) {
            const instruction_index = scope.getInstructionIndex(hir_arg_index);
            arg_inst_index = (try ctx.pushMaybeCastInstructionToType(
                hir_arg_index,
                instruction_index,
                type_param,
            )) orelse instruction_index;
            try args_list.append(arg_inst_index);
        } else {
            arg_inst_index = scope.getInstructionIndex(hir_arg_index);
            const arg_inst = ctx.getInstruction(arg_inst_index);
            const can_cast = scope.builder.canCastImplicitly(arg_inst.typed_value.type, type_param) catch |err| {
                std.debug.panic("{s}", .{@errorName(err)});
            };
            // std.debug.print("{} {} {}\n", .{ arg_inst.type, type_param, can_cast });

            if (can_cast == .not_allowed) {
                const writer = std.io.getStdErr().writer().any();
                try writer.print("error: argument type mismatch: ", .{});
                try scope.builder.sema.formatType(writer, arg_inst.typed_value.type);
                try writer.print(" != ", .{});
                try scope.builder.sema.formatType(writer, type_param);
                try writer.print("\n", .{});
                @panic("Type mismatch");
            }
            try args_list.append(arg_inst_index);
        }

        const arg_inst = ctx.getInstruction(arg_inst_index);

        if (!arg_inst.typed_value.isComptimeKnown()) {
            all_args_comptime_known = false;
        }
    }

    return .{
        .args_list = try args_list.commit(),
        .all_args_comptime_known = all_args_comptime_known,
        .ctx = ctx,
    };
}

fn genBuiltinCall(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const callee_inst_index = scope.getInstructionIndex(hir_inst.fn_call.callee);
    const callee_inst = ctx.getInstruction(callee_inst_index);
    // const callee_value: Sema.Value = self.builder.getValue(callee_inst.value) orelse {
    //     std.debug.panic("error: builtin call value is not a builtin global", .{});
    // };
    const builtin_global_key = scope.builder.unwrapTypeValue(callee_inst.typed_value.value);
    const any_type = try scope.builder.internTypeData(.{ .any = .{} });
    switch (scope.builder.getComplexType(builtin_global_key).data.builtin_global) {
        .comptime_log => {
            // const writer = std.io.getStdErr().writer().any();
            // var args_list = self.builder.newList();
            // for (hir_args_list) |arg_hir_index| {
            //     const arg_inst_index = try self.getInstructionAsTypeByHirInst(arg_hir_index, any_type);
            //     try args_list.append(arg_inst_index);
            // }
            const signature_check_result = try checkSignaturae(ctx, scope, &.{any_type}, hir_inst.fn_call.args_list, false);
            ctx.markDead(callee_inst_index);

            return ctx.pushInstruction(hir_inst_index, .{
                .op = .fn_call,
                .typed_value = Sema.TypedValue.VOID,
                .data = .{ .builtin_call = .{
                    .builtin = .comptime_log,
                    .args_list = signature_check_result.args_list,
                } },
            });
        },
        .as => {
            var signature_check_result = try checkSignaturae(
                ctx,
                scope,
                &.{
                    Sema.Type.simple(.type),
                    any_type,
                },
                hir_inst.fn_call.args_list,
                false,
            );
            const type_arg_inst_index = signature_check_result.getArg(0);
            const value_arg_inst_index = signature_check_result.getArg(1);
            return ctx.pushMaybeCastInstruction(hir_inst_index, value_arg_inst_index, type_arg_inst_index);
        },
        .float_demote => {
            var signature_check_result = try checkSignaturae(
                ctx,
                scope,
                &.{
                    try scope.builder.internFlatUnionTypeData(&.{
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.f64),
                        } }),
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.f32),
                        } }),
                    }),
                    try scope.builder.internFlatUnionTypeData(&.{
                        Sema.Type.simple(.f64),
                        Sema.Type.simple(.f32),
                    }),
                },
                hir_inst.fn_call.args_list,
                false,
            );

            ctx.markDeadIfComptimeKnown(signature_check_result.getArg(0));
            ctx.markDeadIfComptimeKnown(signature_check_result.getArg(1));

            return ctx.pushInstruction(hir_inst_index, .{
                .op = .float_demote,
                .typed_value = .{
                    .type = scope.builder.unwrapTypeValue(signature_check_result.getArgInstruction(0).typed_value.value),
                    .value = signature_check_result.getArgInstruction(1).typed_value.value,
                },
                .data = .{ .operand = signature_check_result.getArg(1) },
            });
        },
        .Result => {
            var signature_check_result = try checkSignaturae(
                ctx,
                scope,
                &.{any_type},
                hir_inst.fn_call.args_list,
                false,
            );
            const type_arg_inst = signature_check_result.getArgInstruction(0);
            const ty = try scope.builder.internTypeData(.{
                .result = .{
                    .ok = type_arg_inst.typed_value.type,
                    .err = Sema.Type.simple(.unknown),
                },
            });
            return ctx.pushInstruction(hir_inst_index, .{
                .op = .type,
                .typed_value = .{
                    .type = try scope.builder.internTypeData(.{
                        .typeof = .{ .child = ty },
                    }),
                    .value = try scope.builder.internValueData(.{
                        .type = ty,
                    }),
                },
                .data = .void,
                // .data = .{ .operand = signature_check_result.getArg(0) },
                //     .data = .{ .operand = signature_check_result.getArg(0) },
            });
        },

        else => |builtin| {
            std.debug.panic("unimplemented builtin: @{s}", .{@tagName(builtin)});
        },
    }
}
fn genBuiltinMemberCall(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const callee_inst_index = scope.getInstructionIndex(hir_inst.fn_call.callee);
    const callee_inst = ctx.getInstruction(callee_inst_index);
    // const callee_value: Sema.Value = self.builder.getValue(callee_inst.value) orelse {
    //     std.debug.panic("error: builtin call value is not a builtin global", .{});
    // };
    const builtin_global_key = scope.builder.unwrapTypeValue(callee_inst.typed_value.value);
    std.debug.print("callee_inst: {}\n", .{callee_inst});
    // const builtin_global_key = self.builder.unwrapTypeValue(callee_inst.value);
    // const any_type = try self.builder.internTypeData(.{ .any = .{} });
    switch (scope.builder.getComplexType(builtin_global_key).data.builtin_member.member) {
        .as => {
            const lhs_inst_index = callee_inst.data.operand;
            const lhs_inst = ctx.getInstruction(lhs_inst_index);
            const lhs_type = lhs_inst.typed_value.type;

            var signature_check_result = try checkSignaturae(
                ctx,
                scope,
                &.{
                    try scope.builder.internFlatUnionTypeData(&.{
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.f64),
                        } }),
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.f32),
                        } }),
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.i8),
                        } }),
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.i16),
                        } }),
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.i32),
                        } }),
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.i64),
                        } }),
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.u8),
                        } }),
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.u16),
                        } }),
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.u32),
                        } }),
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.u64),
                        } }),
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.f32),
                        } }),
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.f64),
                        } }),
                        try scope.builder.internTypeData(.{ .typeof = .{
                            .child = Sema.Type.simple(.bchar),
                        } }),
                    }),
                },
                hir_inst.fn_call.args_list,
                false,
            );
            const rhs_inst_index = signature_check_result.getArg(0);
            const rhs_inst = ctx.getInstruction(rhs_inst_index);
            const rhs_type = scope.builder.unwrapTypeValue(rhs_inst.typed_value.value);

            if (lhs_type.isEqual(rhs_type)) {
                std.debug.panic("error: unsupported cast", .{});
            }
            const lhs_bits = scope.builder.numberBits(lhs_type);
            const rhs_bits = scope.builder.numberBits(rhs_type);

            const lhs_is_float = scope.builder.isFloat(lhs_type);
            const rhs_is_float = scope.builder.isFloat(rhs_type);
            ctx.markDead(callee_inst_index);
            ctx.markDeadIfComptimeKnown(lhs_inst_index);
            ctx.markDead(rhs_inst_index);

            if (lhs_is_float != rhs_is_float) {
                const typed_value = try ctx.builder.maybeCoerceValue(lhs_inst.typed_value, rhs_type);
                if (lhs_is_float) {
                    return ctx.pushInstruction(hir_inst_index, .{
                        .op = .truncate_float_to_int,
                        .typed_value = typed_value,
                        .data = .{ .operand = lhs_inst_index },
                    });
                } else {
                    return ctx.pushInstruction(hir_inst_index, .{
                        .op = .convert_int_to_float,
                        .typed_value = typed_value,
                        .data = .{ .operand = lhs_inst_index },
                    });
                }
            }

            switch (lhs_type.simple) {
                .float, .int => {
                    return ctx.pushMaybeCastInstruction(hir_inst_index, lhs_inst_index, rhs_inst_index);
                },
                .f32, .f64 => {
                    // const lhs_bits = self.builder.numberBits(lhs_type);
                    // const rhs_bits = self.builder.numberBits(rhs_type);
                    const typed_value = try ctx.builder.maybeCoerceValue(lhs_inst.typed_value, rhs_type);
                    if (rhs_bits > lhs_bits) {
                        return ctx.pushInstruction(hir_inst_index, .{
                            .op = .float_promote,
                            .typed_value = typed_value,
                            .data = .{ .operand = lhs_inst_index },
                        });
                    }

                    return ctx.pushInstruction(hir_inst_index, .{
                        .op = .float_demote,
                        .typed_value = typed_value,
                        .data = .{ .operand = lhs_inst_index },
                    });
                },
                .u8, .u16, .u32, .u64, .usize => {
                    // const lhs_bits = self.builder.numberBits(lhs_type);
                    // const rhs_bits = self.builder.numberBits(rhs_type);
                    const typed_value = try ctx.builder.maybeCoerceValue(lhs_inst.typed_value, rhs_type);
                    const rhs_signed = scope.builder.isSigned(rhs_type);
                    if (rhs_signed) {
                        return ctx.pushInstruction(hir_inst_index, .{
                            .op = .reinterpret,
                            .typed_value = typed_value,
                            .data = .{ .operand = lhs_inst_index },
                        });
                    }
                    if (rhs_bits > lhs_bits) {
                        return ctx.pushInstruction(hir_inst_index, .{
                            .op = .int_extend,
                            .typed_value = typed_value,
                            .data = .{ .operand = lhs_inst_index },
                        });
                    }

                    return ctx.pushInstruction(hir_inst_index, .{
                        .op = .int_wrap,
                        .typed_value = typed_value,
                        .data = .{ .operand = lhs_inst_index },
                    });
                },

                .i8, .i16, .i32, .i64 => {
                    // const lhs_bits = self.builder.numberBits(lhs_type);
                    // const rhs_bits = self.builder.numberBits(rhs_type);
                    const typed_value = try ctx.builder.maybeCoerceValue(lhs_inst.typed_value, rhs_type);
                    const rhs_signed = scope.builder.isSigned(rhs_type);
                    if (!rhs_signed) {
                        return ctx.pushInstruction(hir_inst_index, .{
                            .op = .reinterpret,
                            .typed_value = typed_value,
                            .data = .{ .operand = lhs_inst_index },
                        });
                    }

                    if (rhs_bits > lhs_bits) {
                        return ctx.pushInstruction(hir_inst_index, .{
                            .op = .int_extend,
                            .typed_value = typed_value,
                            .data = .{ .operand = lhs_inst_index },
                        });
                    }

                    return ctx.pushInstruction(hir_inst_index, .{
                        .op = .int_wrap,
                        .typed_value = typed_value,
                        .data = .{ .operand = lhs_inst_index },
                    });
                },

                // .number => {},
                else => {
                    std.debug.panic("error: cannot cast '{s}' to '{s}'", .{ @tagName(lhs_type.simple), @tagName(rhs_type.simple) });
                },
            }
        },
        else => std.debug.panic("unimplemented builtin member call", .{}),
    }
}

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);

    const callee_inst_index = scope.getInstructionIndex(hir_inst.fn_call.callee);
    const callee_inst = ctx.getInstruction(callee_inst_index);

    const callee_fn_type_key = callee_inst.typed_value.type;
    const callee_fn_type = scope.builder.getComplexType(callee_fn_type_key);

    const fn_entity = switch (callee_fn_type.data) {
        .builtin_global => return try genBuiltinCall(ctx, scope, hir_inst_index),
        .builtin_member => return try genBuiltinMemberCall(ctx, scope, hir_inst_index),
        .function => |function| scope.builder.getEntity(function.entity),
        else => std.debug.panic("error: not callable {s}", .{@tagName(callee_fn_type.data)}),
    };
    // const callee_fn_value_key = try scope.maybeResolveDependency(fn_entity.key);

    const hir_args_list = scope.entity.getHirList(hir_inst.fn_call.args_list);
    const params_list = scope.builder.sema.lists.getSlice(callee_fn_type.data.function.params);

    if (hir_args_list.len != params_list.len) {
        std.debug.panic("error: function has {d} params but {d} args were provided", .{ params_list.len, hir_args_list.len });
    }

    var args_list = scope.builder.newList();

    var all_args_resolved = true;

    for (hir_args_list, params_list) |arg_hir_index, param_type_key| {
        const param_type = Sema.Type.Key.decode(param_type_key);
        const instruction_index = scope.getInstructionIndex(arg_hir_index);
        const arg_index = (try ctx.pushMaybeCastInstructionToType(
            arg_hir_index,
            instruction_index,
            param_type,
        )) orelse instruction_index;

        const arg_inst = ctx.getInstruction(arg_index);
        if (!arg_inst.typed_value.type.isEqual(param_type) and !param_type.isEqual(try scope.builder.internTypeData(.{ .any = .{} }))) {
            std.debug.panic("error: argument type mismatch: {} != {}", .{ arg_inst.typed_value.type, param_type });
        }

        try args_list.append(arg_index);
        // const arg_inst = ctx.getInstruction(arg_index);
        if (!arg_inst.typed_value.isComptimeKnown()) {
            all_args_resolved = false;
        }
    }

    // var value: Sema.Value.Key = Sema.Value.simple(.exec_time);
    // if (self.is_comptime and !callee_fn_value_key.isEqualSimple(.exec_time) and !callee_fn_value_key.isEqualSimple(.runtime) and all_args_resolved) {
    //     if (self.builder.getValue(callee_fn_value_key)) |callee_fn_value| {
    //         if (callee_fn_value.data.function.init) |init_inst| {
    //             var arena = std.heap.ArenaAllocator.init(self.builder.allocator);
    //             defer arena.deinit();
    //             var args_typed_values: std.ArrayList(Sema.TypedValue) = std.ArrayList(Sema.TypedValue).init(self.builder.allocator);
    //             defer args_typed_values.deinit();
    //             for (args_list.list.items) |arg_index| {
    //                 const arg_inst = self.getInstruction(arg_index);
    //                 self.markDead(arg_index);
    //                 try args_typed_values.append(.{
    //                     .type = arg_inst.type,
    //                     .value = arg_inst.value,
    //                 });
    //             }
    //             var exec_context = ExecContext.init(
    //                 arena.allocator(),
    //                 self.builder,
    //                 args_typed_values.items,
    //                 callee_fn_type_key,
    //                 callee_fn_value_key,
    //                 init_inst,
    //             );
    //             value = try exec_context.exec();
    //         }
    //     }
    // }
    const index = ctx.pushInstruction(hir_inst_index, .{
        .op = .fn_call,
        .typed_value = .{
            .type = callee_fn_type.data.function.ret,
            .value = Sema.Value.simple(.exec_time),
        },
        .data = .{
            .fn_call = .{
                .callee_entity = fn_entity.key,
                .callee_declaration = fn_entity.data.function_declaration.declaration_index,
                .callee = callee_inst_index,
                .args_list = try args_list.commit(),
            },
        },
    });
    const callee_typed_value = ctx.getTypedValue(callee_inst_index);

    if (ctx.is_comptime and callee_typed_value.isComptimeKnown()) {
        std.debug.print("exec fn call {}\n", .{callee_typed_value});
        try Index.exec(ctx, index);
    }

    return index;
}

const ExecContext = struct {
    builder: *Sema.Builder,
    args: []const Sema.TypedValue,
    values: std.AutoHashMapUnmanaged(Sema.Instruction.Index, Sema.TypedValue) = .{},
    root_block: Sema.Instruction.Index,
    indent: usize = 0,
    // instructions: []const Sema.Instruction.Index,
    // callee_value: Sema.TypedValue,
    pub fn init(builder: *Sema.Builder, args: []const Sema.TypedValue, callee_value: Sema.TypedValue) ExecContext {
        std.debug.print("{any}\n", .{callee_value.value});
        const fn_val: Sema.Value = builder.getValue(callee_value.value) orelse @panic("not a fn");
        const init_block_inst_index = fn_val.data.function.init orelse @panic("no init block");
        // const init_block_inst = builder.sema.instructions.items[init_block_inst_index];
        // const init_block_value = builder.getComplexValue(callee_value.value);
        // const instructions = builder.sema.lists.getSlice(init_block_inst.data.block.instructions_list);
        // _ = instructions; // autofix
        // const instructions = builder.sema.instructions.items[init_block_inst_index .. init_block_inst_index + init_block_inst.data.block.instructions_list.len];

        return ExecContext{
            .builder = builder,
            .args = args,
            .root_block = init_block_inst_index,
            // .values = try builder.sema.allocator.alloc(?Sema.TypedValue, instructions.len),
        };
    }
    pub fn deinit(self: *ExecContext) void {
        self.values.deinit(self.builder.allocator);
    }
    pub fn getInstructionFn(context: *anyopaque, index: Sema.Instruction.Index) Sema.Instruction {
        const self: *ExecContext = @alignCast(@ptrCast(context));
        var instruction = self.builder.sema.instructions.items[self.root_block + index];

        instruction.typed_value = self.values.get(index) orelse return instruction;
        return instruction;
    }

    pub fn setInstructionFn(context: *anyopaque, index: Sema.Instruction.Index, instruction: Sema.Instruction) void {
        const self: *ExecContext = @alignCast(@ptrCast(context));
        // self.values[index] = instruction.typed_value;
        self.values.put(self.builder.allocator, index, instruction.typed_value) catch @panic("failed to set instruction");
    }

    pub fn getParamValueFn(context: *anyopaque, index: u32) Sema.Value.Key {
        const self: *ExecContext = @alignCast(@ptrCast(context));
        return self.args[index].value;
    }

    pub fn setIdMapFn(context: *anyopaque, id: u32, index: Sema.Instruction.Index) void {
        const self: *ExecContext = @alignCast(@ptrCast(context));
        self.values.put(self.builder.allocator, index, Sema.TypedValue{
            .type = self.args[id].type,
            .value = self.args[id].value,
        }) catch @panic("failed to set id map");
        // self.builder.sema.instructions.items[self.root_block + index] = instruction;
    }

    pub fn goToFn(context: *anyopaque, index: Sema.Instruction.Index) void {
        _ = index; // autofix
        const self: *ExecContext = @alignCast(@ptrCast(context));
        _ = self; // autofix
        // self.exec(index);
    }

    pub fn pushInstructionFn(context: *anyopaque, id: ?u32, instruction: Sema.Instruction) Sema.Instruction.Index {
        _ = context; // autofix
        _ = id; // autofix
        _ = instruction; // autofix
        @panic("Should not push instruction in fn call");
    }
    pub fn getInstContext(self: *ExecContext) InstContext {
        return InstContext{
            .builder = self.builder,
            .context = self,
            .getInstructionFn = getInstructionFn,
            .setInstructionFn = setInstructionFn,
            .getParamValueFn = getParamValueFn,
            .setIdMapFn = setIdMapFn,
            .pushInstructionFn = pushInstructionFn,
            .goToFn = goToFn,
            .is_comptime = true,
            .indent = self.indent,
        };
    }
    pub fn exec(self: *ExecContext) Error!Sema.TypedValue {
        var inst_context = self.getInstContext();
        std.debug.print("exec\n", .{});
        try Index.exec(&inst_context, 0);
        std.debug.print("exec done\n", .{});
        return self.values.get(0) orelse Sema.TypedValue.VOID;
    }
};

pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);

    if (!ctx.is_comptime) @panic("not comptime known");

    // std.debug.panic("not implemented", .{});
    const arg_instructions = ctx.builder.sema.lists.getSlice(inst.data.fn_call.args_list);
    var args = try std.ArrayList(Sema.TypedValue).initCapacity(ctx.builder.allocator, arg_instructions.len);
    defer args.deinit();
    for (arg_instructions) |arg_inst_index| {
        const typed_value = ctx.getTypedValue(arg_inst_index);
        if (!typed_value.isComptimeKnown()) {
            std.debug.panic("cannot call function with non-comptime arguments", .{});
        }
        args.appendAssumeCapacity(typed_value);
    }

    const callee = ctx.getInstruction(inst.data.fn_call.callee);
    std.debug.print("callee: {}\n", .{callee});

    // const init_block = ctx.builder
    // const fn_value: Sema.Value = ctx.builder.getValue(inst.typed_value.value) orelse {
    //     std.debug.panic("error: function value not found", .{});
    // };
    // const init_block = ctx.builder.getComplexValue(fn_value).data.function.init orelse {
    //     std.debug.panic("error: function value is not a function", .{});
    // };

    var exec_context = ExecContext.init(ctx.builder, args.items, callee.typed_value);
    exec_context.indent = ctx.indent;
    defer exec_context.deinit();
    const val = try exec_context.exec();

    ctx.setValue(inst_index, val);

    // const exec_context = ExecContext{
    //     .builder = ctx.builder,
    //     .args = args.items,
    //     .callee_value = callee.typed_value,
    //     .values = ctx.values,
    // };
    // var exec_context = InstContext{
    //     .builder = ctx.builder,
    //     .depth = 0,
    //     .context = ctx.context,
    //     // .getInstructionFn = ctx.getInstructionFn,
    //     // .setInstructionFn = ctx.setInstructionFn,
    //     // .getParamValueFn = ctx.getParamValueFn,
    //     // .goToFn = ctx.goToFn,
    // };
    // @panic("not implemented");
}
