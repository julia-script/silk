const std = @import("std");
const Mir = @import("../../Mir.zig");
const WasmBuilder = @import("./WasmBuilder.zig");
const InternedSlice = @import("../../InternedStrings.zig").InternedSlice;
const Compilation = @import("../../Compilation.zig");

const Self = @This();
const dir = @import("../../dir.zig");

const DeclarationMapKey = union(enum) {
    global: Mir.Global.Index,
    local: Mir.Instruction.Index,
};
const DeclarationMap = std.AutoArrayHashMap(DeclarationMapKey, u32);

var STACK_POINTER_INDEX: u32 = 0;
compilation: *Compilation,
mir: *Mir,
builder: WasmBuilder.Module,

declaration_map: DeclarationMap,
pub fn compile(compilation: *Compilation) !void {
    var arena = std.heap.ArenaAllocator.init(compilation.allocator);
    defer arena.deinit();
    const root_source = try compilation.getRootSource();
    var self = Self{
        .compilation = compilation,
        .mir = &(root_source.mir orelse return error.RootSourceNotCompiled),
        .builder = try WasmBuilder.Module.init(arena.allocator()),
        .declaration_map = DeclarationMap.init(arena.allocator()),
    };

    try self.translateMir();
    var output_dir = try compilation.openOutputDir("bin", true);
    defer output_dir.close();
    var buf: [std.fs.max_name_bytes]u8 = undefined;
    const file_name = dir.concat(buf[0..], .{ compilation.name, ".wasm" });
    var file = try output_dir.createFile(file_name, .{});
    defer file.close();
    try self.builder.toBytes(file.writer().any());
}

pub fn translateMir(self: *Self) !void {
    var global = try self.builder.makeGlobal("__stack_pointer", .i32);
    STACK_POINTER_INDEX = @intCast(global.index);

    global.mutable = true;
    try global.pushInstruction(.{ .i32_const = @intCast(self.compilation.stack_memory_size) });
    try global.commit();
    // try self.translateModule(Mir.Type.RootIndex);
    for (0..self.mir.*.globals.items.len) |global_index| {
        try self.translateDeclaration(@intCast(global_index));
    }
}
pub fn translateDeclaration(self: *Self, global_index: Mir.Global.Index) !void {
    const global = self.mir.globals.items[global_index];
    if (self.getType(global.type)) |ty| {
        switch (ty) {
            .@"fn" => {
                try self.translateFunctionType(global_index);
            },
            else => {
                @panic("unimplemented");
            },
        }
    }

    // switch (global.type) {
    //     .@"fn" => {
    //         try self.translateFunctionType(global.type, global.name);
    //     },
    //     else => {
    //         @panic("unimplemented");
    //     },
    // }
    // const decl = self.mir.types.items[index];
    // try self.translateModule(decl.global.type);
}
pub fn translateFunctionType(self: *Self, global_index: Mir.Global.Index) !void {
    const global = self.mir.globals.items[global_index];
    const func_type = self.getType(global.type) orelse return error.TypeNotFound;
    const name_slice = self.mir.strings.getSlice(global.name);
    var func_wip = try self.builder.makeFunction();
    try self.declaration_map.put(.{ .global = global_index }, @intCast(func_wip.index));
    func_wip.name = name_slice;
    func_wip.@"export" = true; //export all for now
    if (func_type.@"fn".return_type != .void) {
        const return_type = self.convertType(func_type.@"fn".return_type);
        _ = try func_wip.pushResult(return_type);
    }
    // var iter_params = self.iterList(func_type.@"fn".params);
    // while (iter_params.next()) |param_index| {
    //     const param_type = self.convertType(Mir.Type.Index.asTypeIndex(param_index));
    //     _ = try func_wip.pushParam(param_type);
    // }

    if (global.init) |init_index| {
        try self.translateInstructions(&func_wip, init_index);
    }
    const func_index = try self.builder.putFunction(&func_wip);
    _ = func_index; // autofix

}
pub fn translateInstructions(self: *Self, wip: *WasmBuilder.Function, instructions_list: Mir.Lists.Index) !void {
    var iter = self.iterList(instructions_list);
    while (iter.next()) |inst_index| {
        const inst = self.mir.instructions.items[inst_index];
        switch (inst.op) {
            .param => {
                // const param_type = self.getType(inst.type) orelse return error.TypeNotFound;
                try self.putDeclaration(
                    .{ .local = inst_index },
                    wip.local_count,
                );
                _ = try wip.pushParam(self.convertType(inst.type));
            },
            // .local => {
            //     try wip.pushLocal(self.convertType(inst.type));
            //     try self.declaration_map.put(.{ .local = inst_index }, @intCast(wip.params.items.len + wip.locals.items.len - 1));
            // },
            .constant => {
                const value = self.getValue(inst.value) orelse return error.ValueNotFound;
                switch (value) {
                    .integer => |integer| {
                        switch (inst.type) {
                            .i8, .i16, .i32 => {
                                _ = try wip.pushInstruction(.{ .i32_const = @intCast(integer) });
                            },
                            .i64 => {
                                _ = try wip.pushInstruction(.{ .i64_const = integer });
                            },
                            .usize => {
                                _ = try wip.pushInstruction(.{ .i64_const = integer });
                            },
                            .u8, .u16, .u32, .u64 => {
                                @panic("unimplemented");
                            },
                            .f32 => {
                                _ = try wip.pushInstruction(.{ .f32_const = @floatFromInt(integer) });
                            },
                            .f64 => {
                                _ = try wip.pushInstruction(.{ .f64_const = @floatFromInt(integer) });
                            },
                            else => {
                                std.debug.panic("unimplemented type: {s}", .{@tagName(inst.type)});
                            },
                        }
                    },
                    .float => |float| {
                        switch (inst.type) {
                            .f32 => {
                                _ = try wip.pushInstruction(.{ .f32_const = @floatCast(float) });
                            },
                            .f64 => {
                                _ = try wip.pushInstruction(.{ .f64_const = float });
                            },
                            else => {
                                std.debug.panic("unimplemented type: {s}", .{@tagName(inst.type)});
                            },
                        }
                    },
                    else => {
                        std.debug.panic("unimplemented value: {s}", .{@tagName(value)});
                    },
                }
            },
            .local_set,
            .param_set,
            => {
                const local_inst = self.mir.instructions.items[inst.data.bin_op.lhs];
                _ = try wip.pushInstruction(.{ .local_set = local_inst.data.scoped.index });
            },
            .param_get,
            .local_get,
            => {
                const param_inst = self.mir.instructions.items[inst.data.instruction];
                _ = try wip.pushInstruction(.{ .local_get = param_inst.data.scoped.index });
            },
            inline .gt, .lt, .div => |tag| {
                const tag_str = @tagName(tag);

                const lhs_inst = self.mir.instructions.items[inst.data.bin_op.lhs];
                const wasm_inst = switch (lhs_inst.type) {
                    inline .i8, .i16, .i32 => @unionInit(WasmBuilder.Instruction, "i32_" ++ tag_str ++ "_s", {}),
                    inline .i64 => @unionInit(WasmBuilder.Instruction, "i64_" ++ tag_str ++ "_s", {}),
                    inline .u8, .u16, .u32 => @unionInit(WasmBuilder.Instruction, "i32_" ++ tag_str ++ "_u", {}),
                    inline .u64, .usize => @unionInit(WasmBuilder.Instruction, "i64_" ++ tag_str ++ "_u", {}),
                    inline .f32 => @unionInit(WasmBuilder.Instruction, "f32_" ++ tag_str, {}),
                    inline .f64 => @unionInit(WasmBuilder.Instruction, "f64_" ++ tag_str, {}),
                    else => unreachable,
                };

                _ = try wip.pushInstruction(wasm_inst);
            },
            inline .add,
            .sub,
            .mul,
            .eq,
            .ne,
            => |tag| {
                const tag_str = @tagName(tag);

                const wasm_inst = switch (inst.type) {
                    inline .i8, .i16, .i32, .u8, .u16, .u32 => @unionInit(WasmBuilder.Instruction, "i32_" ++ tag_str, {}),
                    inline .i64, .u64, .usize => @unionInit(WasmBuilder.Instruction, "i64_" ++ tag_str, {}),
                    inline .f32 => @unionInit(WasmBuilder.Instruction, "f32_" ++ tag_str, {}),
                    inline .f64 => @unionInit(WasmBuilder.Instruction, "f64_" ++ tag_str, {}),
                    else => unreachable,
                };
                _ = try wip.pushInstruction(wasm_inst);
            },
            .branch => {
                _ = try wip.pushInstruction(.{ .@"if" = .empty });
                try self.translateInstructions(wip, inst.data.branch.then_instructions_list);
                if (inst.data.branch.else_instructions_list) |else_instructions_list| {
                    _ = try wip.pushInstruction(.{ .@"else" = {} });
                    try self.translateInstructions(wip, else_instructions_list);
                }
                _ = try wip.pushInstruction(.{ .end = {} });
            },
            .ret => {
                _ = try wip.pushInstruction(.{ .@"return" = {} });
            },
            .loop => {
                _ = try wip.pushInstruction(.{ .loop = .empty });
                try self.translateInstructions(wip, inst.data.loop.instructions_list);
                _ = try wip.pushInstruction(.{ .end = {} });
            },
            .br => {
                _ = try wip.pushInstruction(.{ .br = 1 });
            },
            .call => {
                // const callee = self.declaration_map.get(inst.data.call.callee) orelse unreachable;
                _ = try wip.pushInstruction(.{
                    .call = inst.data.call.callee,
                });
            },
            .alloc => {
                // _ = try wip.pushInstruction(.{ .alloc = {} });
                if (self.getType(inst.data.alloc.type)) |ty| {
                    switch (ty) {
                        .array => {
                            // _ = try wip.pushInstruction(.{ .i32_alloc = {} });
                            const element_type = self.convertType(ty.array.type);
                            const element_size: i32 = switch (element_type) {
                                .i32 => 4,
                                .i64 => 8,
                                .f32 => 4,
                                .f64 => 8,
                                else => {
                                    std.debug.panic("unimplemented element type: {s}", .{@tagName(element_type)});
                                },
                            };
                            _ = try wip.pushInstruction(.{ .global_get = STACK_POINTER_INDEX });
                            _ = try wip.pushInstruction(.{ .i32_const = element_size * @as(i32, @intCast(ty.array.size)) });
                            _ = try wip.pushInstruction(.{ .i32_sub = {} });
                            _ = try wip.pushInstruction(.{ .global_set = 0 });
                            continue;
                        },
                        else => {
                            // std.debug.panic("unimplemented alloc type: {s}", .{@tagName(ty)});
                        },
                    }
                }
                var decl_iter = self.declaration_map.iterator();
                while (decl_iter.next()) |decl| {
                    std.debug.print("decl: {} {}\n", .{ decl.key_ptr.*, decl.value_ptr.* });
                }
                try self.putDeclaration(.{ .local = inst_index }, wip.local_count);
                try wip.pushLocal(self.convertType(inst.data.alloc.type));
            },
            .store => {
                // const local_inst = self.mir.instructions.items[inst.data.bin_op.lhs];
                const local_index = self.declaration_map.get(.{ .local = inst.data.store.pointer }) orelse unreachable;
                _ = try wip.pushInstruction(.{ .local_set = local_index });
                // _ = try wip.pushInstruction(.{ .store = {} });
            },
            .load => {
                // const local_inst = self.mir.instructions.items[inst.data.instruction];
                const local_inst = self.declaration_map.get(.{ .local = inst.data.instruction }) orelse unreachable;
                _ = try wip.pushInstruction(.{ .local_get = local_inst });
            },
            else => {
                std.debug.panic("unimplemented instruction: {s}", .{@tagName(inst.op)});
            },
        }
        // try self.translateInstruction(wip, inst);
    }
}

pub fn putDeclaration(self: *Self, key: DeclarationMapKey, value: usize) !void {
    std.debug.print("putDeclaration: {} {}\n", .{ key, value });
    try self.declaration_map.put(key, @intCast(value));
}
pub fn convertType(self: *Self, type_index: Mir.Type.Index) WasmBuilder.Type {
    if (self.getType(type_index)) |ty| {
        switch (ty) {
            .param => |param| {
                return self.convertType(param.type);
            },
            else => {
                std.debug.panic("unimplemented type: {s}", .{@tagName(ty)});
            },
        }
    }
    switch (type_index) {
        .i8,
        .i16,
        .i32,
        => return .i32,
        .i64 => return .i64,

        .u8, .u16, .u32 => return .i32,
        .u64, .usize => return .i64,

        .f32 => return .f32,
        .f64 => return .f64,

        else => {
            std.debug.panic("unimplemented type: {s}", .{@tagName(type_index)});
        },
    }
}
pub fn getType(self: *Self, index: Mir.Type.Index) ?Mir.Type {
    if (index.toInt()) |int| {
        return self.mir.types.items[int];
    }
    return null;
}
pub fn getValue(self: *Self, index: Mir.Value.Index) ?Mir.Value {
    if (index.toInt()) |int| {
        return self.mir.values.items[int];
    }
    return null;
}
pub fn iterList(self: *Self, index: usize) Mir.Lists.ListIter {
    return self.mir.lists.iterList(index);
}
pub fn translateModule(self: *Self, type_index: Mir.Type.Index) !void {
    const module = self.getType(type_index) orelse return error.TypeNotFound;
    var decl_iter = self.iterList(module.module.decls);
    while (decl_iter.next()) |decl| {
        try self.translateDecl(Mir.Type.Index.asTypeIndex(decl));
    }
}
pub fn translateDecl(self: *Self, decl_index: Mir.Type.Index) !void {
    const decl = self.getType(decl_index) orelse return error.DeclNotFound;
    const ty = self.getType(decl.global.type) orelse return error.TypeNotFound;
    switch (ty) {
        .@"fn" => |function| {
            _ = function; // autofix
            // try self.translateFunction(function);
            try self.translateFunction(decl.global.type);
        },
        else => {
            @panic("unimplemented");
        },
    }
}
pub fn translateFunction(self: *Self, function_index: Mir.Type.Index) !void {
    const ty: Mir.Type = self.getType(function_index) orelse return error.FunctionNotFound;
    try self.declaration_map.put(function_index, @intCast(self.builder.function_types.count()));

    // var iter_params = self.iterList(ty.@"fn".params);
    var func_wip = self.builder.makeFunction();
    const name = self.mir.strings.getSlice(ty.@"fn".name);
    func_wip.name = name;
    func_wip.@"export" = true; //export all for now
    if (ty.@"fn".return_type != .void) {
        switch (ty.@"fn".return_type) {
            .i32 => {
                _ = try func_wip.pushResult(.i32);
            },
            .f64 => {
                _ = try func_wip.pushResult(.f64);
            },
            else => {
                std.debug.panic("unimplemented return type: {s}", .{@tagName(ty.@"fn".return_type)});
            },
        }
    }
    if (ty.@"fn".init_block) |body_index| {
        try self.translateBlockInto(body_index, &func_wip);
    }
    // const inst_start = self.mir.instructions.items.len;
    _ = try self.builder.pushFunction(func_wip);
}

pub fn translateBlockInto(self: *Self, block_index: Mir.Type.Index, func: *WasmBuilder.Function) !void {
    const block_ty = self.getType(block_index) orelse return error.TypeNotFound;
    var iter = self.iterList(block_ty.block.instructions);

    while (iter.next()) |inst_index| {
        const inst = self.mir.instructions.items[inst_index];
        switch (inst.op) {
            .param => {
                // const param_type = self.getType(inst.data.param_declaration.type) orelse return error.TypeNotFound;
                const ty = self.getType(inst.type) orelse return error.TypeNotFound;
                //             _ = try func_wip.pushParam(.i32);
                // std.debug.print("param: {s}\n", .{@tagName(inst.type)});
                _ = try func.pushParam(switch (ty.param.type) {
                    .i32 => .i32,
                    .i64 => .i64,
                    .f32 => .f32,
                    .f64 => .f64,
                    else => |t| {
                        std.debug.panic("unimplemented param type: {}", .{t});
                    },
                });
            },
            .local => {
                try func.pushLocal(switch (inst.type) {
                    .i32 => .i32,
                    .i64 => .i64,
                    .f32 => .f32,
                    .f64 => .f64,
                    else => {
                        std.debug.panic("unimplemented local type: {s}", .{@tagName(inst.type)});
                    },
                });
            },
            .constant => {
                if (self.getValue(inst.value)) |value| {
                    switch (value) {
                        .float => |float| {
                            _ = try func.pushInstruction(switch (inst.type) {
                                .f32 => .{ .f32_const = @floatCast(float) },
                                .f64 => .{ .f64_const = float },
                                .i32 => .{ .i32_const = @intFromFloat(float) },
                                .i64 => .{ .i64_const = @intFromFloat(float) },
                                else => unreachable,
                            });
                        },
                        .integer => |integer| {
                            _ = try func.pushInstruction(switch (inst.type) {
                                .f64 => .{ .f64_const = @floatFromInt(integer) },
                                .f32 => .{ .f32_const = @floatFromInt(integer) },
                                .i32 => .{ .i32_const = @intCast(integer) },
                                .i64 => .{ .i64_const = integer },

                                else => {
                                    std.debug.panic("unimplemented integer type: {s}", .{@tagName(inst.type)});
                                },
                            });
                        },

                        else => {
                            @panic("unimplemented");
                        },
                    }
                } else {
                    @panic("Value not found");
                }
            },
            .param_get => {
                const param_inst = self.mir.instructions.items[inst.data.instruction];
                _ = try func.pushInstruction(.{
                    .local_get = param_inst.data.scoped.index,
                });
            },
            .local_get => {
                const local_inst = self.mir.instructions.items[inst.data.instruction];
                _ = try func.pushInstruction(.{ .local_get = local_inst.data.scoped.index });
            },
            // .global_get => {
            // _ = try func.pushInstruction(.{ .global_get = inst.data.scoped.index.? });
            // },
            inline .add,
            .sub,
            .mul,
            .div,
            .gt,
            .lt,
            .eq,
            .ne,
            => |tag| {
                const lhs_inst = self.mir.instructions.items[inst.data.bin_op.lhs];
                // const rhs_inst = self.mir.instructions.items[inst.data.bin_op.rhs];
                switch (lhs_inst.type) {
                    inline .i32, .f64, .i64, .f32 => |type_tag| {
                        const key = @tagName(type_tag) ++ "_" ++ @tagName(tag);
                        _ = try func.pushInstruction(@unionInit(
                            WasmBuilder.Instruction,
                            switch (tag) {
                                .lt, .gt => switch (type_tag) {
                                    .i32, .i64 => key ++ "_s",
                                    // .f32, .f64 => @tagName(tag),
                                    else => key,
                                },
                                else => key,
                            },
                            {},
                        ));
                    },
                    else => {
                        std.debug.panic("unimplemented type: {s}", .{@tagName(inst.type)});
                    },
                }
            },
            // .gt => {
            //     switch (inst.type) {
            //         .number => {
            //             _ = try func.pushInstruction(.{ .f64_gt = {} });
            //         },
            //         else => {
            //             std.debug.panic("unimplemented gt type: {s}", .{@tagName(inst.type)});
            //         },
            //     }
            // },
            .ret => {
                _ = try func.pushInstruction(.{ .@"return" = {} });
            },
            .if_expr => {
                _ = try func.pushInstruction(.{ .@"if" = .empty });
                try self.translateBlockInto(inst.data.if_expr.then_body, func);
                if (inst.data.if_expr.else_body) |else_body| {
                    _ = try func.pushInstruction(.{ .@"else" = {} });
                    try self.translateBlockInto(else_body, func);
                }
                _ = try func.pushInstruction(.{ .end = {} });
            },
            .local_set => {
                const local_inst = self.mir.instructions.items[inst.data.bin_op.lhs];

                // std.debug.panic("Unimplemented instruction: #{d} .{s} .{s}", .{
                //     inst.data.binOp.lhs,
                //     @tagName(local_inst.op),
                //     @tagName(local_inst.data),
                // });
                // switch (local_inst.op) {
                _ = try func.pushInstruction(.{ .local_set = local_inst.data.scoped.index });
            },

            .loop => {
                _ = try func.pushInstruction(.{ .loop = .empty });
                try self.translateBlockInto(inst.data.loop.body, func);
                _ = try func.pushInstruction(.{ .end = {} });
            },
            .br => {
                _ = try func.pushInstruction(.{ .br = 1 });
            },
            .call => {
                const callee = self.declaration_map.get(inst.data.call.callee) orelse unreachable;
                _ = try func.pushInstruction(.{
                    .call = callee,
                });
            },
            else => {
                std.debug.panic("Unimplemented instruction: #{d} .{s} .{s}", .{
                    inst_index,
                    @tagName(inst.op),
                    @tagName(inst.data),
                });
            },
        }
    }
}
