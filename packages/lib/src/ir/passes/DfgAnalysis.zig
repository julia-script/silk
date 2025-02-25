const std = @import("std");
const Error = std.mem.Allocator.Error;

const debug = @import("../../debug.zig");
const Module = @import("../Module.zig");
const TypedValue = Module.TypedValue;
const DfgInterpreter = @import("DfgInterpreter.zig");

allocator: std.mem.Allocator,
arena: std.heap.ArenaAllocator,
mod: *Module,
src: *Module.Dfg,
builder: Module.DefinitionBuilder,
dst: Module.Dfg,
// decl_ref: Module.Decl.Ref,

inst_map: std.AutoHashMapUnmanaged(Module.InstData.Ref, Module.InstData.Ref) = .{},
value_map: std.AutoHashMapUnmanaged(Module.InstData.Ref, Module.TypedValue) = .{},
block_map: std.AutoHashMapUnmanaged(Module.Block.Ref, Module.Block.Ref) = .{},
local_map: std.AutoHashMapUnmanaged(Module.Dfg.Local.Ref, Module.TypedValue) = .{},
mode: Mode = .analyze,

pub const Mode = enum {
    analyze,
    comp,
};

const Self = @This();

pub fn analyze(allocator: std.mem.Allocator, module: *Module, def_ref: Module.Definition.Ref) !Module.Dfg {
    std.debug.print("Dfg Analyzing decl {}\n", .{def_ref});
    const def = module.getDefinition(def_ref);
    std.debug.print("{}", .{def.dfg.display(module)});

    var self: Self = .{
        .mod = module,
        .src = &def.dfg,
        .builder = undefined,
        .allocator = allocator,
        .arena = std.heap.ArenaAllocator.init(allocator),
        .dst = try Module.Dfg.init(module.allocator, def.dfg.is_comptime, def.dfg.is_inline),
    };

    self.dst.signature = self.src.signature;
    defer self.arena.deinit();

    self.builder = try Module.DefinitionBuilder.init(module, def_ref, &self.dst);

    if (def.dfg.result) |result| {
        std.debug.print("Dfg result: {}\n", .{result});
        def.dfg.deinit();
        self.dst.result = try self.analyzeTypedValue(result);
        return self.dst;
    }

    try self.analyzeLocals();
    try self.mapBlock(Module.Dfg.EntryBlock, Module.Dfg.EntryBlock);
    try self.analyzeBlock(Module.Dfg.EntryBlock);

    if (self.dst.is_inline) {
        const block_call_inst: Module.InstData.Ref = .{ .idx = 0 };
        const value = self.getValue(block_call_inst);
        std.debug.print("setting value {}", .{value.display(self.mod)});
        self.dst.result = value;
    }

    def.dfg.deinit();

    return self.dst;
}
pub fn callBuiltin(self: *Self, callee: Module.TypedValue, args: []Module.TypedValue) !Module.TypedValue {
    const builtin = callee.value.builtin;
    switch (builtin) {
        .length => {
            const ty = args[0].ty;
            const tydata = ty.getTyData(self.mod) orelse std.debug.panic("{} is not a type\n", .{ty.display(self.mod)});
            const len_tyv = switch (tydata) {
                .array => |array| try self.analyzeTypedValue(array.len),
                else => std.debug.panic("{} is not an array\n", .{ty.display(self.mod)}),
            };
            const len = len_tyv.value.readAs(u64);
            return TypedValue.Imm(.u64, len);
        },
        .sizeof => {
            const ty = args[0].value.ty;
            const size = ty.getSize(self.mod);
            return TypedValue.Imm(.u64, size);
        },
        else => {
            std.debug.panic("TODO: builtin {}\n", .{builtin});
        },
    }
}
pub fn call(self: *Self, callee: Module.TypedValue, args: []Module.TypedValue) !Module.TypedValue {
    // const ty_data = callee.ty.getTyData(self.mod) orelse std.debug.panic("{} is not a function\n", .{callee.display(self.mod)});
    // const func = switch (ty_data) {
    //     .func => |func| func,
    //     else => std.debug.panic("{} is not a function\n", .{callee.display(self.mod)}),
    // };
    // const signature = self.mod.getSignature(func.signature);
    // if (args.len != signature.params.items.len) {
    //     std.debug.panic("{} has {} args, but {} were provided\n", .{ callee.display(self.mod), signature.params.items.len, args.len });
    // }
    // for (signature.params.items, 0..) |arg, i| {
    //     args[i] = try self.doCast(arg.ty, args[i]);
    //     // std.debug.print("arg: {}\n", .{arg.ty.display(self.mod)});
    // }

    if (callee.isBuiltin()) {
        return try self.callBuiltin(callee, args);
    }

    const func_def = switch (callee.value) {
        .global => |ref| self.mod.maybeGetDefinitionByDeclRef(ref),
        .def => |ref| self.mod.getDefinition(ref),
        else => std.debug.panic("{} is not a function\n", .{callee.display(self.mod)}),
    } orelse std.debug.panic("{} is not defined yet\n", .{callee.display(self.mod)});

    var func_dfg = func_def.dfg;
    var interpreter = try DfgInterpreter.init(self.mod, &func_dfg, self.allocator, args);
    defer interpreter.deinit();

    return try interpreter.run();
}
pub fn mapValue(self: *Self, inst_ref: Module.InstData.Ref, value: Module.TypedValue) !void {
    std.debug.print("mapping {} -> {}\n", .{ inst_ref, value.display(self.mod) });
    try self.value_map.put(self.arena.allocator(), inst_ref, value);
}
pub fn mapInst(self: *Self, src_inst_ref: Module.InstData.Ref, dst_inst_ref: Module.InstData.Ref) !void {
    try self.inst_map.put(self.arena.allocator(), src_inst_ref, dst_inst_ref);
}
pub fn getInst(self: *Self, inst_ref: Module.InstData.Ref) Module.InstData.Ref {
    return self.inst_map.get(inst_ref) orelse std.debug.panic("No inst for {}\n", .{inst_ref});
}
pub fn mapBlock(self: *Self, src_block_ref: Module.Block.Ref, dst_block_ref: Module.Block.Ref) !void {
    try self.block_map.put(self.arena.allocator(), src_block_ref, dst_block_ref);
}
pub fn getBlock(self: *Self, block_ref: Module.Block.Ref) Module.Block.Ref {
    return self.block_map.get(block_ref) orelse std.debug.panic("No block for {}\n", .{block_ref});
}
pub fn getValue(self: *Self, inst_ref: Module.InstData.Ref) Module.TypedValue {
    std.debug.print("getting value for {}\n", .{inst_ref});
    return self.value_map.get(inst_ref) orelse std.debug.panic("No value for inst {}\n", .{inst_ref});
}
fn analyzeLocals(self: *Self) !void {
    // var signature
    const maybe_signature = self.getSignature();
    const params = if (maybe_signature) |signature| signature.params.items else &.{};

    for (self.src.local_values.slice()) |src_local| {
        var local = src_local;

        if (local.is_param) {
            const param = params[local.index];
            local.value.ty = param.ty;
        }
        // const value = try self.analyzeTypedValue(local.value);
        // local.value = value;

        _ = try self.dst.pushLocal(local);
    }
    // if (self.src.result) |src_result| {
    //     var result = src_result;
    //     if (maybe_signature) |signature|
    //         result.ty = signature.ret;
    //     // result = try self.analyzeTypedValue(result);
    //     self.builder.setResult(result);
    // }
}
pub fn getSignature(self: *Self) ?*Module.Signature {
    const sig_ref = self.dst.signature orelse return null;
    return self.mod.getSignature(sig_ref);
}
pub fn getGlobalValue(self: *Self, ref: Module.Decl.Ref) !Module.TypedValue {
    const def = self.mod.getDefinitionByDeclRef(ref);
    const res = def.dfg.result orelse std.debug.panic("No result for global {}\n", .{ref});
    return res;
}
pub fn analyzeValue(self: *Self, value: Module.Value) !Module.Value {
    return switch (value) {
        .global => |ref| {
            if (self.mod.maybeGetDefinitionByDeclRef(ref)) |def| {
                if (def.dfg.result) |result| {
                    if (!result.isRuntime())
                        return result.value;
                }

                // const val = try self.getGlobalValue(ref);
                // return val.value;
            }
            const val = try self.builder.useGlobal(ref);
            return val.value;
        },

        .ty => |ty| {
            const analyzed_ty = try self.analyzeType(ty);
            return .{ .ty = analyzed_ty };
        },
        .tyty => |ty| {
            const analyzed_ty = try self.analyzeType(ty);
            return .{ .tyty = analyzed_ty };
        },
        .inst => |inst| {
            return self.getValue(inst).value;
        },
        else => value,
    };
}
pub fn analyzeType(self: *Self, ty: Module.Ty) Error!Module.Ty {
    return switch (ty) {
        .global => |ref| {
            const val = try self.getGlobalValue(ref);
            return val.value.ty;
        },
        .ref => |ref| {
            const ty_data = self.mod.getTy(ref);
            switch (ty_data.*) {
                .array => |array| {
                    var dst_len = try self.analyzeTypedValue(array.len);
                    dst_len = try doCast(self, .u64, dst_len);
                    const dst_child_ty = try self.analyzeType(array.type);
                    return try self.mod.declareTy(.{ .array = .{
                        .type = dst_child_ty,
                        .len = dst_len,
                        .size = array.size,
                    } });
                },
                .@"struct" => |_str| {
                    const str: Module.Ty.TyData.Struct = _str;
                    var fields = try std.ArrayList(Module.Ty.TyData.Struct.Field).initCapacity(
                        self.mod.arena.allocator(),
                        str.fields.len,
                    );
                    for (str.fields) |field| {
                        const field_ty = try self.analyzeType(field.ty);
                        fields.appendAssumeCapacity(.{
                            .name = field.name,
                            .ty = field_ty,
                            .source_order_index = field.source_order_index,
                            .size = field.size,
                            .offset = field.offset,
                        });
                    }

                    const dst_ty = try self.mod.declareTy(.{ .@"struct" = .{
                        .fields = fields.items,
                        .sealed = str.sealed,
                        .associated_ns = str.associated_ns,
                        .size = str.size,
                    } });

                    if (str.associated_ns) |ns_ref| {
                        const ns = self.mod.namespaces.getPtr(ns_ref);
                        ns.ty = dst_ty;
                    }

                    return dst_ty;
                },

                else => {
                    return ty;
                },
            }
        },
        // .def => |ref| {
        //     var def = self.mod.getDefinition(ref);
        // },
        // .local => |local| {
        //     const val = try self.analyzeTypedValue(Module.TypedValue.Type(.{ .local = local }));
        //     return val.value.ty;
        // },
        .inst => |inst| {
            const value = self.getValue(inst);
            return value.value.ty;
        },
        else => ty,
    };
}
pub fn analyzeTypedValue(self: *Self, tyv: Module.TypedValue) !Module.TypedValue {
    std.debug.print("analyze typedValue {}\n", .{tyv});
    // TODO: Analyze definition type and value
    switch (tyv.value) {
        .inst => |inst| {
            return self.getValue(inst);
        },
        .local => |local| {
            if (self.local_map.get(local)) |val| {
                return val;
            }

            const src_local = self.src.local_values.get(local);
            var val = try self.analyzeTypedValue(src_local.value);
            if (val.isRuntime()) {
                val.value = tyv.value;
            }
            try self.local_map.put(self.arena.allocator(), local, val);
            self.dst.local_values.getPtr(local).value = val;

            return val;
        },
        else => {},
    }
    const analyzed_ty = try self.analyzeType(tyv.ty);
    const analyzed_value = try self.analyzeValue(tyv.value);
    return .{
        .ty = analyzed_ty,
        .value = analyzed_value,
    };
}
fn analyzeBlock(self: *Self, block_ref: Module.Block.Ref) !void {
    const block = self.src.getBlock(block_ref);
    std.debug.print("block: {}\n", .{block});
    for (block.instructions.items) |inst_ref| {
        try self.analyzeInstruction(inst_ref);
    }
}

fn analyzeInstruction(self: *Self, inst_ref: Module.InstData.Ref) Error!void {
    const inst = self.src.getInstruction(inst_ref);
    std.debug.print("{} {}\n", .{ inst_ref, inst.display(self.mod) });
    switch (inst.*) {
        .cast => try self.mapValue(inst_ref, try self.analyzeInstCast(inst_ref)),
        .store => try self.analyzeStore(inst_ref),
        .binary => |bin| switch (bin.op) {
            .eq,
            .ne,
            .lt,
            .ge,
            .gt,
            .le,
            // .lt_u,
            // .ge_u,
            // .gt_u,
            // .le_u,
            // .lt_s,
            // .ge_s,
            // .gt_s,
            // .le_s,
            => try self.mapValue(inst_ref, try self.analyzeComparison(inst_ref)),
            .add, .sub, .mul, .div => {
                try self.mapValue(inst_ref, try self.analyzeArithmetic(inst_ref));
            },
            else => {},
        },
        .branch => try self.mapValue(inst_ref, try self.analyzeBranch(inst_ref)),
        .loop => try self.mapValue(inst_ref, try self.analyzeLoop(inst_ref)),
        .@"break" => try self.analyzeBreak(inst_ref),
        .@"return" => try self.analyzeReturn(inst_ref),
        .init_array => try self.mapValue(inst_ref, try self.analyzeInitArray(inst_ref)),
        .init_struct => try self.mapValue(inst_ref, try self.analyzeInitStruct(inst_ref)),
        .property_by_name => try self.mapValue(inst_ref, try self.analyzePropertyByName(inst_ref)),
        .property_by_index => try self.mapValue(inst_ref, try self.analyzePropertyByIndex(inst_ref)),
        .block_call => try self.mapValue(inst_ref, try self.analyzeBlockCall(inst_ref)),
        .call => try self.mapValue(inst_ref, try self.analyzeCall(inst_ref)),
        else => {
            const inst_data = self.src.getInstruction(inst_ref);
            std.debug.panic("can't analyze instruction {}\n", .{inst_data.display(self.mod)});
        },
    }
}
fn doCast(self: *Self, ty: Module.Ty, tyv: Module.TypedValue) !Module.TypedValue {
    if (ty.eql(.any)) {
        return tyv;
    }
    if (ty.isNumber()) return try doNumberCast(self, ty, tyv);
    if (tyv.ty.eql(ty)) {
        return tyv;
    }
    std.debug.panic("can't cast type '{}' to '{}'", .{ tyv.ty.display(self.mod), ty.display(self.mod) });
}
fn doNumberCast(self: *Self, ty: Module.Ty, tyv: Module.TypedValue) !Module.TypedValue {
    if (tyv.ty.eql(ty)) {
        return tyv;
    }
    if (!ty.isUntypedNumber() and !tyv.ty.isUntypedNumber()) {
        if (tyv.ty.isSigned() != ty.isSigned() or tyv.ty.getSize(self.mod) > ty.getSize(self.mod) or tyv.ty.isFloat() != ty.isFloat()) {
            std.debug.panic("type '{}' can't be implicity cast to '{}'", .{ tyv.ty.display(self.mod), ty.display(self.mod) });
        }
    }
    if (!tyv.isResolved()) {
        return try self.builder.cast(ty, tyv);
    }
    if (tyv.value.acceptLocal()) |local| {
        _ = local; // autofix
        // const local_value = self.src.local_values.getPtr(local);
        // local_value.value = ty;
        return try self.builder.cast(ty, tyv);
    }
    switch (tyv.ty) {
        .int => {
            const int_value = tyv.value.readAs(i64);
            return switch (ty) {
                .i8 => TypedValue.Imm(.i8, @as(i8, @intCast(int_value))),
                .i16 => TypedValue.Imm(.i16, @as(i16, @intCast(int_value))),
                .i32 => TypedValue.Imm(.i32, @as(i32, @intCast(int_value))),
                .i64 => TypedValue.Imm(.i64, @as(i64, @intCast(int_value))),

                .u8 => TypedValue.Imm(.u8, @as(u8, @intCast(int_value))),
                .u16 => TypedValue.Imm(.u16, @as(u16, @intCast(int_value))),
                .u32 => TypedValue.Imm(.u32, @as(u32, @intCast(int_value))),
                .u64 => TypedValue.Imm(.u64, @as(u64, @intCast(int_value))),

                .f32 => TypedValue.Imm(.f32, @as(f32, @floatFromInt(int_value))),
                .f64 => TypedValue.Imm(.f64, @as(f64, @floatFromInt(int_value))),
                .float => TypedValue.Imm(.float, @as(f64, @floatFromInt(int_value))),

                else => std.debug.panic("type '{}' can't be stored as '{}'", .{ tyv.ty.display(self.mod), ty.display(self.mod) }),
            };
        },
        .float => {
            const float_value = tyv.value.readAs(f64);
            return switch (ty) {
                .f32 => TypedValue.Imm(.f32, @as(f32, @floatCast(float_value))),
                .f64 => TypedValue.Imm(.f64, @as(f64, @floatCast(float_value))),

                .i8 => TypedValue.Imm(.i8, @as(i8, @intFromFloat(float_value))),
                .i16 => TypedValue.Imm(.i16, @as(i16, @intFromFloat(float_value))),
                .i32 => TypedValue.Imm(.i32, @as(i32, @intFromFloat(float_value))),
                .i64 => TypedValue.Imm(.i64, @as(i64, @intFromFloat(float_value))),

                .u8 => TypedValue.Imm(.u8, @as(u8, @intFromFloat(float_value))),
                .u16 => TypedValue.Imm(.u16, @as(u16, @intFromFloat(float_value))),
                .u32 => TypedValue.Imm(.u32, @as(u32, @intFromFloat(float_value))),
                .u64 => TypedValue.Imm(.u64, @as(u64, @intFromFloat(float_value))),

                .int => TypedValue.Imm(.int, @as(i64, @intFromFloat(float_value))),
                else => std.debug.panic("type '{}' can't be stored as '{}'", .{ tyv.ty.display(self.mod), ty.display(self.mod) }),
            };
        },
        .i8, .i16, .i32, .i64 => {
            if (tyv.ty.isSigned() != ty.isSigned() or tyv.ty.getSize(self.mod) > ty.getSize(self.mod)) {
                std.debug.panic("type '{}' can't be implicity casted to '{}'", .{ tyv.ty.display(self.mod), ty.display(self.mod) });
            }
            const value = tyv.value.readAs(i64);
            return TypedValue.Imm(ty, value);
        },
        .u8, .u16, .u32, .u64 => {
            if (tyv.ty.isSigned() != ty.isSigned() or tyv.ty.getSize(self.mod) > ty.getSize(self.mod)) {
                std.debug.panic("type '{}' can't be implicity casted to '{}'", .{ tyv.ty.display(self.mod), ty.display(self.mod) });
            }
            const value = tyv.value.readAs(u64);
            return TypedValue.Imm(ty, value);
        },
        .f32, .f64 => {
            if (tyv.ty.getSize(self.mod) > ty.getSize(self.mod)) {
                std.debug.panic("type '{}' can't be implicity casted to '{}'", .{ tyv.ty.display(self.mod), ty.display(self.mod) });
            }
            const value = tyv.value.readAs(f64);
            return TypedValue.Imm(ty, value);
        },
        else => {
            std.debug.panic("type '{}' can't be cast to '{}'", .{ tyv.ty.display(self.mod), ty.display(self.mod) });
        },
    }
}
fn analyzeInstCast(self: *Self, inst_ref: Module.InstData.Ref) !Module.TypedValue {
    const inst = self.src.getInstruction(inst_ref).cast;
    const ty = try self.analyzeType(inst.ty);
    const tyv = try self.analyzeTypedValue(inst.value);
    return try doCast(self, ty, tyv);
}
fn analyzeComparison(self: *Self, inst_ref: Module.InstData.Ref) !Module.TypedValue {
    const inst = self.src.getInstruction(inst_ref).binary;
    const lhs, const rhs = try normalizeTypes(self, inst.args[0], inst.args[1]);
    if (!lhs.isResolved() or !rhs.isResolved()) {
        return switch (inst.op) {
            .eq => try self.builder.eq(lhs.ty, lhs, rhs),
            .ne => try self.builder.ne(lhs.ty, lhs, rhs),
            .lt => try self.builder.lt(lhs.ty, lhs, rhs),
            .le => try self.builder.le(lhs.ty, lhs, rhs),
            .gt => try self.builder.gt(lhs.ty, lhs, rhs),
            .ge => try self.builder.ge(lhs.ty, lhs, rhs),
            else => std.debug.panic("can't do {s} on runtime values", .{@tagName(inst.op)}),
        };
    }
    if (lhs.ty.isSigned()) {
        std.debug.print("Signed comparison {} {}\n", .{
            lhs.display(self.mod),
            rhs.display(self.mod),
        });
        const lhs_value = lhs.value.readAs(i64);
        const rhs_value = rhs.value.readAs(i64);
        switch (inst.op) {
            .eq => return TypedValue.Imm(.bool, lhs_value == rhs_value),
            .ne => return TypedValue.Imm(.bool, lhs_value != rhs_value),
            .lt => return TypedValue.Imm(.bool, lhs_value < rhs_value),
            .le => return TypedValue.Imm(.bool, lhs_value <= rhs_value),
            .gt => return TypedValue.Imm(.bool, lhs_value > rhs_value),
            .ge => return TypedValue.Imm(.bool, lhs_value >= rhs_value),
            else => std.debug.panic("can't do {s} on type {}", .{ @tagName(inst.op), lhs.ty.display(self.mod) }),
        }
    } else {
        const lhs_value = lhs.value.readAs(u64);
        const rhs_value = rhs.value.readAs(u64);
        switch (inst.op) {
            .eq => return TypedValue.Imm(.bool, lhs_value == rhs_value),
            .ne => return TypedValue.Imm(.bool, lhs_value != rhs_value),
            .lt => return TypedValue.Imm(.bool, lhs_value < rhs_value),
            .le => return TypedValue.Imm(.bool, lhs_value <= rhs_value),
            .gt => return TypedValue.Imm(.bool, lhs_value > rhs_value),
            .ge => return TypedValue.Imm(.bool, lhs_value >= rhs_value),
            else => std.debug.panic("can't do {s} on type {}", .{ @tagName(inst.op), lhs.ty.display(self.mod) }),
        }
    }
    if (lhs.ty.isFloat()) {
        const lhs_value = lhs.value.readAs(f64);
        const rhs_value = rhs.value.readAs(f64);
        switch (inst.op) {
            .eq => return TypedValue.Imm(.bool, lhs_value == rhs_value),
            .ne => return TypedValue.Imm(.bool, lhs_value != rhs_value),
            .lt => return TypedValue.Imm(.bool, lhs_value < rhs_value),
            .le => return TypedValue.Imm(.bool, lhs_value <= rhs_value),
            .gt => return TypedValue.Imm(.bool, lhs_value > rhs_value),
            .ge => return TypedValue.Imm(.bool, lhs_value >= rhs_value),
            else => std.debug.panic("can't do {s} on type {}", .{ @tagName(inst.op), lhs.ty.display(self.mod) }),
        }
    }
}
pub fn normalizeTypes(self: *Self, lhs: Module.TypedValue, rhs: Module.TypedValue) !struct { TypedValue, TypedValue } {
    const lhs_copy = try self.analyzeTypedValue(lhs);
    const rhs_copy = try self.analyzeTypedValue(rhs);
    if (lhs_copy.ty.eql(rhs_copy.ty)) {
        return .{ lhs_copy, rhs_copy };
    }
    const lhs_size = lhs_copy.ty.maybeGetSize(self.mod) orelse 0;
    const rhs_size = rhs_copy.ty.maybeGetSize(self.mod) orelse 0;
    if (lhs_size > rhs_size) {
        return .{ lhs_copy, try doCast(self, lhs_copy.ty, rhs_copy) };
    } else {
        return .{ try doCast(self, rhs_copy.ty, lhs_copy), rhs_copy };
    }
}
fn analyzeArithmetic(self: *Self, inst_ref: Module.InstData.Ref) !Module.TypedValue {
    const inst = self.src.getInstruction(inst_ref).binary;
    const lhs, const rhs = try normalizeTypes(self, inst.args[0], inst.args[1]);
    if (!lhs.isResolved() or !rhs.isResolved()) {
        return switch (inst.op) {
            .add => try self.builder.add(lhs.ty, lhs, rhs),
            .sub => try self.builder.sub(lhs.ty, lhs, rhs),
            .mul => try self.builder.mul(lhs.ty, lhs, rhs),
            .div => try self.builder.div(lhs.ty, lhs, rhs),
            else => std.debug.panic("can't do {s} on type {}", .{ @tagName(inst.op), lhs.ty.display(self.mod) }),
        };
    }
    if (lhs.ty.isFloat()) {
        const lhs_value = lhs.value.readAs(f64);
        const rhs_value = rhs.value.readAs(f64);
        return switch (inst.op) {
            .add => TypedValue.Imm(lhs.ty, lhs_value + rhs_value),
            .sub => TypedValue.Imm(lhs.ty, lhs_value - rhs_value),
            .mul => TypedValue.Imm(lhs.ty, lhs_value * rhs_value),
            .div => TypedValue.Imm(lhs.ty, @divTrunc(lhs_value, rhs_value)),

            else => std.debug.panic("can't do {s} on type {}", .{ @tagName(inst.op), lhs.ty.display(self.mod) }),
        };
    }

    if (lhs.ty.isSigned()) {
        const lhs_value = lhs.value.readAs(i64);
        const rhs_value = rhs.value.readAs(i64);
        return switch (inst.op) {
            .add => TypedValue.Imm(lhs.ty, lhs_value + rhs_value),
            .sub => TypedValue.Imm(lhs.ty, lhs_value - rhs_value),
            .mul => TypedValue.Imm(lhs.ty, lhs_value * rhs_value),
            .div => TypedValue.Imm(lhs.ty, @divTrunc(lhs_value, rhs_value)),
            else => std.debug.panic("can't do {s} on type {}", .{ @tagName(inst.op), lhs.ty.display(self.mod) }),
        };
    }
    const lhs_value = lhs.value.readAs(u64);
    const rhs_value = rhs.value.readAs(u64);
    return switch (inst.op) {
        .add => TypedValue.Imm(lhs.ty, lhs_value + rhs_value),
        .sub => TypedValue.Imm(lhs.ty, lhs_value - rhs_value),
        .mul => TypedValue.Imm(lhs.ty, lhs_value * rhs_value),
        .div => TypedValue.Imm(lhs.ty, @divTrunc(lhs_value, rhs_value)),
        else => std.debug.panic("can't do {s} on type {}", .{ @tagName(inst.op), lhs.ty.display(self.mod) }),
    };
}

fn assertTypeMatch(self: *Self, a: Module.Ty, b: Module.Ty) void {
    debug.assertPrint(a.eql(b), "type {} can't be stored in {}", .{ a.display(self.mod), b.display(self.mod) });
}

fn analyzeStore(self: *Self, inst_ref: Module.InstData.Ref) !void {
    const store = self.src.getInstruction(inst_ref).store;
    var local = self.builder.dfg.local_values.getPtr(store.local);

    var value = try self.analyzeTypedValue(store.value);
    const local_ty = try self.analyzeTypedValue(
        Module.TypedValue.Type(local.value.ty),
    );
    value = try doCast(self, local_ty.value.ty, value);

    try self.builder.store(store.local, value);
    if (local.is_comptime) {
        std.debug.print("setting local {} to {}\n", .{ local.index, value.display(self.mod) });
        local.value = value;
    }
}

fn analyzeBranch(self: *Self, inst_ref: Module.InstData.Ref) !Module.TypedValue {
    const inst = self.src.getInstruction(inst_ref).branch;
    const cond = try self.analyzeTypedValue(inst.cond);
    if (cond.isResolved()) {
        const cond_value = cond.value.readAs(bool);
        const src_then_block = if (cond_value) inst.args[0] else inst.args[1];
        const src_finally_block = inst.args[2];
        if (src_then_block == null and src_finally_block == null) {
            return TypedValue.Void;
        }

        const dst_block_call = try self.builder.makeBlockCall();
        var dst_then_block: ?Module.Block.Ref = null;
        var dst_finally_block: ?Module.Block.Ref = null;

        if (src_then_block) |src_block_ref| {
            try self.mapInst(inst_ref, dst_block_call);
            const dst_block_ref = try self.builder.makeBlock(dst_block_call, false);
            try self.analyzeBlock(src_block_ref);
            dst_then_block = dst_block_ref;
        }
        if (src_finally_block) |src_block_ref| if (!self.src.isBlockEmpty(src_block_ref)) {
            const dst_block_ref = try self.builder.makeBlock(dst_block_call, false);
            try self.analyzeBlock(src_block_ref);
            dst_finally_block = dst_block_ref;
        };

        try self.builder.setBlockCallTarget(dst_block_call, dst_then_block.?, dst_finally_block);
        return TypedValue.Void;
    }

    self.assertTypeMatch(cond.ty, .bool);

    const dst_branch = try self.builder.makeBranch(cond);
    try self.mapInst(inst_ref, dst_branch);

    var blocks: [3]?Module.Block.Ref = undefined;

    // if (inst.args[0]) |then_block| {
    for (inst.args, 0..) |maybe_block_ref, i| {
        if (maybe_block_ref) |src_block_ref| {
            const dst_block_ref = try self.builder.makeBlock(dst_branch, false);
            try self.mapBlock(src_block_ref, dst_block_ref);

            blocks[i] = dst_block_ref;
            try self.analyzeBlock(src_block_ref);
        } else {
            blocks[i] = null;
        }
    }
    try self.builder.setBranchTarget(dst_branch, blocks[0], blocks[1], blocks[2]);

    return TypedValue.Void;
}

fn analyzeLoop(self: *Self, inst_ref: Module.InstData.Ref) !Module.TypedValue {
    const inst = self.src.getInstruction(inst_ref).loop;
    const dst_loop = try self.builder.makeLoop();
    try self.mapInst(inst_ref, dst_loop);
    var then_block: ?Module.Block.Ref = null;
    var finally_block: ?Module.Block.Ref = null;

    if (inst.args[0]) |src_block_ref| {
        const dst_block = try self.builder.makeBlock(dst_loop, false);
        try self.analyzeBlock(src_block_ref);
        then_block = dst_block;
    }
    if (inst.args[1]) |src_block_ref| if (!self.src.isBlockEmpty(src_block_ref)) {
        const dst_finally = try self.builder.makeBlock(dst_loop, false);
        try self.analyzeBlock(src_block_ref);
        finally_block = dst_finally;
    };
    try self.builder.setLoopTarget(dst_loop, then_block, finally_block);

    // const cond = try self.analyzeTypedValue(inst.cond);
    // const body = try self.analyzeBlock(inst.body);
    return TypedValue.Void;
}

fn analyzeBreak(self: *Self, inst_ref: Module.InstData.Ref) !void {
    const inst = self.src.getInstruction(inst_ref).@"break";
    const value = try self.analyzeTypedValue(inst.value);
    const dst_target = self.getInst(inst.target);

    const dst_break = try self.builder.breakTo(dst_target, value);
    try self.mapInst(inst_ref, dst_break);
}
fn analyzeReturn(self: *Self, inst_ref: Module.InstData.Ref) !void {
    const inst = self.src.getInstruction(inst_ref).@"return";
    var value = try self.analyzeTypedValue(inst.value);

    if (self.getSignature()) |sig| {
        value = try self.doCast(sig.ret, value);
    }

    try self.builder.ret(value);
}
fn analyzeInitArray(self: *Self, inst_ref: Module.InstData.Ref) !Module.TypedValue {
    const init_array = self.src.getInstruction(inst_ref).init_array;
    const items = init_array.items;

    const ty = try self.analyzeType(init_array.ty);
    // const src_ty_data = init_array.ty.getTyData(self.mod) orelse std.debug.panic("can't get child type of {}", .{init_array.ty.display(self.mod)});
    // var dst_size = try self.analyzeTypedValue(src_ty_data.array.size);
    // dst_size = try doCast(self, .u64, dst_size);
    // const dst_child_ty = try self.analyzeType(src_ty_data.array.type);

    var dst_items = try std.ArrayList(Module.TypedValue).initCapacity(
        self.arena.allocator(),
        items.len,
    );
    const item_ty = ty.getChildTy(self.mod) orelse std.debug.panic("can't get child type of {}", .{ty.display(self.mod)});

    defer dst_items.deinit();

    for (items) |item| {
        var value = try self.analyzeTypedValue(item);
        value = try doCast(self, item_ty, value);
        dst_items.appendAssumeCapacity(value);
    }
    // const array_ty = try self.mod.declareTy(.{ .array = .{
    //     .type = dst_child_ty,
    //     .size = dst_size,
    // } });
    // std.debug.print("array_ty: {}\n", .{array_ty.display(self.mod)});
    return try self.builder.initArray(ty, dst_items.items);
}

fn analyzeInitStruct(self: *Self, inst_ref: Module.InstData.Ref) !Module.TypedValue {
    const init_struct = self.src.getInstruction(inst_ref).init_struct;
    const ty = try self.analyzeType(init_struct.ty);
    const ty_data = ty.getTyData(self.mod) orelse std.debug.panic("can't get ty data of {}", .{ty.display(self.mod)});
    const fields = ty_data.@"struct".fields;

    var field_ty_by_name = std.StringHashMap(Module.Ty).init(self.arena.allocator());
    for (fields) |field| {
        try field_ty_by_name.put(field.name, field.ty);
    }

    var values = try std.ArrayList(Module.TypedValue).initCapacity(
        self.arena.allocator(),
        init_struct.values.len,
    );
    defer values.deinit();
    for (init_struct.values, init_struct.keys) |item, key| {
        var value = try self.analyzeTypedValue(item);
        const field_ty = field_ty_by_name.get(key) orelse std.debug.panic("can't get field type of {s}", .{key});
        value = try doCast(self, field_ty, value);
        values.appendAssumeCapacity(value);
    }

    return try self.builder.initStruct(ty, init_struct.keys, values.items);
}
fn analyzePropertyByName(self: *Self, inst_ref: Module.InstData.Ref) !Module.TypedValue {
    const inst = self.src.getInstruction(inst_ref).property_by_name;
    const tyv = try self.analyzeTypedValue(inst.tyv);
    const prop_ty: Module.Ty = blk: {
        const field = tyv.ty.getFieldByName(self.mod, inst.name) orelse std.debug.panic("can't get field {s} of {}", .{ inst.name, tyv.ty.display(self.mod) });
        break :blk field.ty;
    };
    return try self.builder.propertyByName(tyv, inst.name, prop_ty);
}
fn analyzePropertyByIndex(self: *Self, inst_ref: Module.InstData.Ref) !Module.TypedValue {
    const inst = self.src.getInstruction(inst_ref).property_by_index;

    std.debug.print("PROPERTY_BY_INDEX {}\n", .{inst.tyv.display(self.mod)});
    const tyv = try self.analyzeTypedValue(inst.tyv);
    var index = try self.analyzeTypedValue(inst.index);
    index = try self.doNumberCast(.u64, index);

    const prop_ty = tyv.ty.getChildTy(self.mod) orelse std.debug.panic("can't get child type of {}", .{tyv.ty.display(self.mod)});
    return try self.builder.propertyByIndex(tyv, index, prop_ty);
}

fn analyzeBlockCall(self: *Self, inst_ref: Module.InstData.Ref) !Module.TypedValue {
    const inst = self.src.getInstruction(inst_ref).block_call;

    const src_then, const src_finally = inst.args;
    const dst_block_call = try self.builder.makeBlockCall();
    try self.mapInst(inst_ref, dst_block_call);

    var dst_then_block: ?Module.Block.Ref = null;
    var dst_finally_block: ?Module.Block.Ref = null;

    if (src_then) |src_block_ref| {
        const src_block = self.src.getBlock(src_block_ref);
        const dst_block_ref = try self.builder.makeBlock(dst_block_call, src_block.is_comptime);

        try self.analyzeBlock(src_block_ref);
        dst_then_block = dst_block_ref;
    }
    if (src_finally) |src_block_ref| if (!self.src.isBlockEmpty(src_block_ref)) {
        const src_block = self.src.getBlock(src_block_ref);
        const dst_block_ref = try self.builder.makeBlock(dst_block_call, src_block.is_comptime);
        try self.analyzeBlock(src_block_ref);
        dst_finally_block = dst_block_ref;
    };
    try self.builder.setBlockCallTarget(
        dst_block_call,
        dst_then_block.?,
        dst_finally_block,
    );
    return self.builder.dfg.inst_values.get(dst_block_call) orelse Module.TypedValue.Void;
}

fn analyzeCall(self: *Self, inst_ref: Module.InstData.Ref) !Module.TypedValue {
    const inst = self.src.getInstruction(inst_ref).call;
    std.debug.print("analyze callee {}", .{inst.callee});
    const callee = try self.analyzeTypedValue(inst.callee);

    const ty_data = callee.ty.getTyData(self.mod) orelse std.debug.panic("{} is not a function\n", .{callee.display(self.mod)});

    const func = switch (ty_data) {
        .func => |func| func,
        else => std.debug.panic("{} is not a function\n", .{callee.display(self.mod)}),
    };

    const signature = self.mod.getSignature(func.signature);

    if (inst.args.len != signature.params.items.len) {
        std.debug.panic("{} has {} args, but {} were provided\n", .{ callee.display(self.mod), signature.params.items.len, inst.args.len });
    }

    var dst_args = try std.ArrayList(Module.TypedValue).initCapacity(
        self.arena.allocator(),
        inst.args.len,
    );
    defer dst_args.deinit();

    for (signature.params.items, inst.args) |param, unanalized_value| {
        var value = try self.analyzeTypedValue(unanalized_value);
        if (!value.ty.satisfies(self.mod, param.ty)) {
            std.debug.panic("Expected type '{}', got '{}'\n", .{ param.ty.display(self.mod), value.ty.display(self.mod) });
        }
        value = try doCast(self, param.ty, value);
        dst_args.appendAssumeCapacity(value);
    }

    if (self.builder.isComptime() or callee.isBuiltin() or signature.ret == .type) {
        std.debug.print("calling \n", .{});
        const result = try self.call(callee, dst_args.items);
        std.debug.print("result: {}\n", .{result.display(self.mod)});
        return result;
    }

    return try self.builder.call(callee, dst_args.items);
}
