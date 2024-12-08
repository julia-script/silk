const std = @import("std");
const Mir = @import("../../Mir.zig");
const WasmBuilder = @import("./WasmBuilder.zig");

const Compilation = @import("../../Compilation.zig");
const Self = @This();
const dir = @import("../../dir.zig");

compilation: *Compilation,
mir: *Mir,
builder: WasmBuilder.Module,

declaration_map: std.AutoArrayHashMap(Mir.Type.Index, u32),
pub fn compile(compilation: *Compilation) !void {
    var arena = std.heap.ArenaAllocator.init(compilation.allocator);
    defer arena.deinit();
    const root_source = try compilation.getRootSource();
    var self = Self{
        .compilation = compilation,
        .mir = &(root_source.mir orelse return error.RootSourceNotCompiled),
        .builder = try WasmBuilder.Module.init(arena.allocator()),
        .declaration_map = std.AutoArrayHashMap(Mir.Type.Index, u32).init(arena.allocator()),
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
    try self.translateModule(Mir.Type.RootIndex);
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
