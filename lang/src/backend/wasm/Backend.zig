const std = @import("std");
const Mir = @import("../../Mir.zig");
const WasmBuilder = @import("./WasmBuilder.zig");

const Compilation = @import("../../Compilation.zig");
const Self = @This();

compilation: *Compilation,
mir: *Mir,
builder: WasmBuilder.Module,
pub fn compile(compilation: *Compilation) !void {
    var arena = std.heap.ArenaAllocator.init(compilation.allocator);
    defer arena.deinit();
    const root_source = try compilation.getRootSource();
    var self = Self{
        .compilation = compilation,
        .mir = &(root_source.mir orelse return error.RootSourceNotCompiled),
        .builder = try WasmBuilder.Module.init(arena.allocator()),
    };

    // var mir = root_source.mir orelse return error.RootSourceNotCompiled;
    try self.translateMir();
    const file = try compilation.createFile("out.wasm");
    defer file.close();
    try self.builder.dumpBytes();
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
    const ty = self.getType(decl.decl.type) orelse return error.TypeNotFound;
    switch (ty) {
        .@"fn" => |function| {
            _ = function; // autofix
            // try self.translateFunction(function);
            try self.translateFunction(decl.decl.type);
        },
        else => {
            @panic("unimplemented");
        },
    }
}
pub fn translateFunction(self: *Self, function_index: Mir.Type.Index) !void {
    const ty: Mir.Type = self.getType(function_index) orelse return error.FunctionNotFound;

    var iter_params = self.iterList(ty.@"fn".params);
    var func_wip = self.builder.makeFunction();

    while (iter_params.next()) |param| {
        const param_ty = self.getType(Mir.Type.Index.asTypeIndex(param)) orelse return error.TypeNotFound;
        switch (param_ty.param.type) {
            .number => {
                _ = try func_wip.pushParam(.f64);
            },
            else => {
                @panic("unimplemented");
            },
        }
    }
    if (ty.@"fn".return_type != .void) {
        switch (ty.@"fn".return_type) {
            .number => {
                _ = try func_wip.pushResult(.f64);
            },
            else => {
                @panic("unimplemented");
            },
        }
    }
    if (ty.@"fn".body) |body_index| {
        try self.translateBlockInto(body_index, &func_wip);
    }
    // const inst_start = self.mir.instructions.items.len;
    _ = try self.builder.pushFunction(func_wip);
}
pub fn translateBlockInto(self: *Self, block_index: Mir.Type.Index, func: *WasmBuilder.Function) !void {
    const block_ty = self.getType(block_index) orelse return error.TypeNotFound;
    var i = block_ty.block.instruction_start;
    const inst_end = i + block_ty.block.instruction_count;
    while (i < inst_end) : (i += 1) {
        const inst = self.mir.instructions.items[i];
        switch (inst.op) {
            .constant => {
                if (self.getValue(inst.data.value)) |value| {
                    switch (value) {
                        .float => |float| {
                            _ = try func.pushInstruction(.{ .f64_const = float });
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
                _ = try func.pushInstruction(.{ .local_get = inst.data.scoped.index.? });
            },
            // .global_get => {
            // _ = try func.pushInstruction(.{ .global_get = inst.data.scoped.index.? });
            // },
            .add => {
                switch (inst.type) {
                    .number => {
                        _ = try func.pushInstruction(.{ .f64_add = {} });
                    },
                    else => {
                        @panic("unimplemented");
                    },
                }
            },
            .gt => {
                switch (inst.type) {
                    .number => {
                        _ = try func.pushInstruction(.{ .f64_gt = {} });
                    },
                    else => {
                        std.debug.panic("unimplemented gt type: {s}", .{@tagName(inst.type)});
                    },
                }
            },
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
            else => {
                std.debug.panic("Unimplemented instruction: {s}", .{@tagName(inst.op)});
            },
        }
    }
}
