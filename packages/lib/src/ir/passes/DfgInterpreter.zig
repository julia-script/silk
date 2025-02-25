const std = @import("std");
const Module = @import("../Module.zig");
const debug = @import("../../debug.zig");

mod: *Module,
dfg: *Module.Dfg,
memory: std.ArrayListUnmanaged(u8) = .{},
args: []Module.TypedValue,
local: std.ArrayListUnmanaged(Module.TypedValue) = .{},
values: std.ArrayListUnmanaged(?Module.TypedValue) = .{},
ret_value: ?Module.TypedValue = null,
allocator: std.mem.Allocator,
const Self = @This();

const Error = std.mem.Allocator.Error;
pub fn init(mod: *Module, dfg: *Module.Dfg, allocator: std.mem.Allocator, args: []Module.TypedValue) !@This() {
    return .{
        .mod = mod,
        .dfg = dfg,
        .args = args,
        .allocator = allocator,
    };
}

pub fn deinit(self: *@This()) void {
    self.memory.deinit(self.allocator);
    self.values.deinit(self.allocator);
    self.local.deinit(self.allocator);
}
pub fn getValue(self: *@This(), inst_ref: Module.InstData.Ref) Module.TypedValue {
    return self.values.items[inst_ref.idx] orelse std.debug.panic("Value not set for instruction {}\n", .{inst_ref});
}
pub fn run(self: *@This()) Error!Module.TypedValue {
    var ctx = Context{
        .parent = null,
        .block_ref = Module.Dfg.EntryBlock,
        .mod = self.mod,
        .dfg = self.dfg,
    };
    std.debug.print("running\n\n{}", .{self.dfg.display(self.mod)});
    try self.values.appendNTimes(self.allocator, null, self.dfg.instructions.count());
    const local_count = self.dfg.local_values.count();
    try self.local.ensureTotalCapacity(self.allocator, local_count);
    self.local.appendSliceAssumeCapacity(self.args);
    self.local.items.len = local_count;
    try self.execBlock(&ctx);
    return self.ret_value orelse Module.TypedValue.Void;
}
const Context = struct {
    parent: ?*Context,
    cursor: usize = 0,
    block_ref: Module.Block.Ref,
    mod: *Module,
    dfg: *Module.Dfg,
    is_done: bool = false,
    is_loop_body: bool = false,
    should_break: bool = true,

    pub fn makeChild(self: *@This(), ref: Module.Block.Ref) @This() {
        return .{
            .parent = self,
            .block_ref = ref,
            .mod = self.mod,
            .dfg = self.dfg,
        };
    }
    pub fn getBlock(self: *@This()) *Module.Block {
        return self.dfg.getBlock(self.block_ref);
    }
    pub fn cleanup(self: *@This()) void {
        self.is_done = true;
    }
};

pub fn resolveValue(self: *@This(), tyv: Module.TypedValue) !Module.TypedValue {
    switch (tyv.value) {
        .inst => |inst| {
            return self.getValue(inst);
        },
        .local => |local_ref| {
            const local = self.dfg.local_values.getPtr(local_ref);
            return self.local.items[local.index];
        },
        // .ref => |ref| {
        //     return self.values.items[ref];
        // },
        else => return tyv,
    }
}
pub fn setValue(self: *@This(), inst_ref: Module.InstData.Ref, tyv: Module.TypedValue) !void {
    self.values.items[inst_ref.idx] = tyv;
}
fn computeBinary(T: type, op: Module.Op, lhs: Module.TypedValue, rhs: Module.TypedValue) Module.TypedValue {
    const lhs_value = lhs.value.readAs(T);
    const rhs_value = rhs.value.readAs(T);
    switch (op) {
        .add => return Module.TypedValue.Imm(lhs.ty, lhs_value + rhs_value),
        .sub => return Module.TypedValue.Imm(lhs.ty, lhs_value - rhs_value),
        .mul => return Module.TypedValue.Imm(lhs.ty, lhs_value * rhs_value),
        .div => {
            if (rhs_value == 0) {
                std.debug.panic("division by zero", .{});
            }
            switch (T) {
                f32, f64 => {
                    return Module.TypedValue.Imm(lhs.ty, lhs_value / rhs_value);
                },
                else => {
                    return Module.TypedValue.Imm(lhs.ty, @divTrunc(lhs_value, rhs_value));
                },
            }
        },
        .eq => return Module.TypedValue.Imm(.bool, lhs_value == rhs_value),
        .ne => return Module.TypedValue.Imm(.bool, lhs_value != rhs_value),
        .lt => return Module.TypedValue.Imm(.bool, lhs_value < rhs_value),
        .le => return Module.TypedValue.Imm(.bool, lhs_value <= rhs_value),
        .gt => return Module.TypedValue.Imm(.bool, lhs_value > rhs_value),
        .ge => return Module.TypedValue.Imm(.bool, lhs_value >= rhs_value),
        else => std.debug.panic("can't do {s} on type {}", .{ @tagName(op), lhs.ty }),
    }
}

pub fn execBlock(self: *@This(), ctx: *Context) !void {
    const block = ctx.getBlock();
    // defer ctx.cleanup();

    while (ctx.cursor < block.instructions.items.len and !ctx.is_done) : (ctx.cursor += 1) {
        const inst_ref = block.instructions.items[ctx.cursor];
        const inst = self.dfg.instructions.get(inst_ref);
        std.debug.print("exec {}: {s}\n", .{ ctx.cursor, @tagName(inst) });

        switch (inst) {
            .binary => |binary| {
                const lhs_tyv = try self.resolveValue(inst.binary.args[0]);
                const rhs_tyv = try self.resolveValue(inst.binary.args[1]);
                debug.assertPrint(lhs_tyv.ty.eql(rhs_tyv.ty), "type mismatch: {} != {}", .{ lhs_tyv.ty.display(ctx.mod), rhs_tyv.ty.display(ctx.mod) });

                std.debug.print("lhs: {}\n", .{lhs_tyv});
                std.debug.print("rhs: {}\n", .{rhs_tyv});

                const result = switch (lhs_tyv.ty) {
                    .i8, .i16, .i32, .i64 => computeBinary(i64, binary.op, lhs_tyv, rhs_tyv),
                    .u8, .u16, .u32, .u64 => computeBinary(u64, binary.op, lhs_tyv, rhs_tyv),
                    .f32, .f64 => computeBinary(f64, binary.op, lhs_tyv, rhs_tyv),
                    else => std.debug.panic("can't do {s} on type {}", .{ @tagName(binary.op), lhs_tyv.ty.display(ctx.mod) }),
                };
                try self.setValue(inst_ref, result);
            },

            .block_call => {
                const then_ref, const finally_ref = inst.block_call.args;
                var then_ctx = ctx.makeChild(then_ref.?);

                try self.execBlock(&then_ctx);

                if (ctx.is_done) {
                    return;
                }

                if (finally_ref) |finally_block| {
                    var finally_ctx = ctx.makeChild(finally_block);
                    try self.execBlock(&finally_ctx);
                }
            },
            .@"return" => |ret| {
                std.debug.print("returning {}\n", .{ret.value});
                self.ret_value = try self.resolveValue(ret.value);
                var current_ctx = ctx;
                current_ctx.cleanup();
                while (current_ctx.parent) |parent| {
                    current_ctx = parent;
                    current_ctx.cleanup();
                }

                return;
            },
            .branch => |branch| {
                const cond = try self.resolveValue(branch.cond);
                const maybe_then_block, const maybe_else_block, const maybe_finally_block = branch.args;
                std.debug.print("branch cond: {}\n", .{cond});
                if (cond.value.readAs(bool)) {
                    if (maybe_then_block) |then_block| {
                        var then_ctx = ctx.makeChild(then_block);
                        try self.execBlock(&then_ctx);
                    }
                } else {
                    if (maybe_else_block) |else_block| {
                        var else_ctx = ctx.makeChild(else_block);
                        try self.execBlock(&else_ctx);
                    }
                }

                if (ctx.is_done) {
                    return;
                }
                if (maybe_finally_block) |finally_block| {
                    var finally_ctx = ctx.makeChild(finally_block);
                    try self.execBlock(&finally_ctx);
                }
            },
            .store => |store| {
                const value = try self.resolveValue(store.value);
                const local_ref = store.local;
                const local = self.dfg.local_values.get(local_ref);
                self.local.items[local.index] = value;
            },
            .loop => |loop| {
                // const loop_ctx = ctx.makeChild();
                const body_ref, const finally_ref = loop.args;
                var max_iter: usize = 100;
                if (body_ref) |body_block| {
                    var body_ctx = ctx.makeChild(body_block);
                    body_ctx.is_loop_body = true;
                    try self.execBlock(&body_ctx);
                    while (body_ctx.should_break == false) {
                        max_iter -= 1;
                        if (max_iter == 0) {
                            std.debug.panic("loop max iter reached", .{});
                        }
                        body_ctx.cleanup();

                        body_ctx = ctx.makeChild(body_block);
                        body_ctx.is_loop_body = true;
                        try self.execBlock(&body_ctx);
                    }
                }
                if (ctx.is_done) {
                    return;
                }
                if (finally_ref) |finally_block| {
                    var finally_ctx = ctx.makeChild(finally_block);
                    try self.execBlock(&finally_ctx);
                }
            },
            .@"break" => |brk| {
                const target: Module.InstData.Ref = brk.target;
                const value = try self.resolveValue(brk.value);
                try self.setValue(inst_ref, value);
                var current_ctx = ctx;
                var current_ctx_block = current_ctx.getBlock();

                // while (current_ctx_block.initiator != null and current_ctx_block.initiator.?.idx != target.idx) {
                while (true) {
                    current_ctx.cleanup();
                    if (current_ctx_block.initiator != null and current_ctx_block.initiator.?.idx == target.idx) {
                        break;
                    }
                    if (current_ctx.parent) |parent| {
                        current_ctx = parent;
                        current_ctx_block = current_ctx.getBlock();
                    } else {
                        std.debug.panic("break target not found", .{});
                    }
                }
                if (current_ctx.is_loop_body) {
                    current_ctx.should_break = false;
                }
                std.debug.print("break target found {} {}\n", .{ current_ctx_block.initiator.?, current_ctx.block_ref });
                // const target_block = ctx.dfg.getBlock(target.value.readAs(Module.Block.Ref));
                // var target_ctx = ctx.makeChild(target_block);
                // try self.execBlock(&target_ctx);
            },
            .call => |call| {
                const callee = try self.resolveValue(call.callee);
                var args = try self.allocator.alloc(Module.TypedValue, call.args.len);
                defer self.allocator.free(args);
                for (call.args, 0..) |arg, i| {
                    args[i] = try self.resolveValue(arg);
                }
                const func_def = switch (callee.value) {
                    .global => |ref| self.mod.maybeGetDefinitionByDeclRef(ref),
                    else => std.debug.panic("{} is not a function\n", .{callee.display(self.mod)}),
                } orelse std.debug.panic("{} is not defined yet\n", .{callee.display(self.mod)});
                var func_dfg = func_def.dfg;
                var interpreter = try Self.init(self.mod, &func_dfg, self.allocator, args);
                defer interpreter.deinit();
                const result = try interpreter.run();
                try self.setValue(inst_ref, result);
            },
            .init_struct => |init_struct| {
                const size = init_struct.ty.getSize(self.mod);
                const ptr = self.memory.items.len;
                try self.memory.appendNTimes(self.allocator, 0, size);
                const tydata = init_struct.ty.getTyData(self.mod) orelse std.debug.panic("{} is not a struct\n", .{init_struct.ty.display(self.mod)});
                // var slice = self.memory.items[self.memory.items.len - size ..];
                std.debug.print("size: {}\n", .{size});

                for (init_struct.keys, init_struct.values) |key, value| {
                    for (tydata.@"struct".fields) |field| {
                        if (!std.mem.eql(u8, field.name, key)) continue;
                        const field_value = try self.resolveValue(value);
                        const offset = (field.offset orelse std.debug.panic("field '{s}' of type '{}' size is not known\n", .{ field.name, field.ty.display(self.mod) }));
                        const field_size = field.ty.getSize(self.mod);
                        const value_bytes = field_value.value.bytes;
                        const field_ptr = ptr + offset;
                        std.debug.print("copying offset: {}\n", .{offset});
                        std.mem.copyForwards(u8, self.memory.items[field_ptr .. field_ptr + field_size], value_bytes[0..field_size]);
                        break;
                    }
                    // std.debug.panic("field '{s}' not found in {}\n", .{ key, init_struct.ty.display(self.mod) });
                }
                std.debug.print("stored slice: {any}\n", .{self.memory.items[ptr .. ptr + size]});
                try self.setValue(inst_ref, .{ .ty = init_struct.ty, .value = .{ .pointer = @intCast(ptr) } });
            },
            .property_by_name => |prop| {
                const value = try self.resolveValue(prop.tyv);
                const tydata = value.ty.getTyData(self.mod) orelse std.debug.panic("{} is not a struct\n", .{value.ty.display(self.mod)});
                const size = value.ty.getSize(self.mod);
                const ptr = value.value.pointer;
                const slice = self.memory.items[ptr .. ptr + size];
                std.debug.print("fetched slice: {any}\n", .{slice});
                for (tydata.@"struct".fields) |field| {
                    if (!std.mem.eql(u8, field.name, prop.name)) continue;
                    const offset = (field.offset orelse std.debug.panic("field '{s}' of type '{}' size is not known\n", .{ field.name, field.ty.display(self.mod) }));
                    const field_size = field.ty.getSize(self.mod);

                    if (field_size > 8) {
                        std.debug.panic("Unimplemented: property of size > 8", .{});
                    }
                    var final_value = Module.TypedValue.Imm(field.ty, 0);
                    const bytes_to_copy = slice[offset .. offset + field_size];
                    std.mem.copyForwards(
                        u8,
                        final_value.value.bytes[0..field_size],
                        bytes_to_copy,
                    );

                    try self.setValue(inst_ref, final_value);
                    std.debug.print("final value: {} bytes: {any} offset {d} size {d} slice: {any}\n", .{ final_value.display(self.mod), bytes_to_copy, offset, field_size, slice });
                    break;
                }
            },
            .init_array => |init_array| {
                const tydata = init_array.ty.getTyData(self.mod) orelse std.debug.panic("{} is not an array\n", .{init_array.ty.display(self.mod)});
                const size_tyv = try self.resolveValue(tydata.array.len);
                const len = size_tyv.value.readAs(u64);
                const size = (tydata.array.size orelse std.debug.panic("array size is not known\n", .{}));
                const element_size = size / len;
                const ptr = self.memory.items.len;
                try self.memory.appendNTimes(self.allocator, 0, size);

                std.debug.print("size: {}\n", .{size});
                var slice = self.memory.items[ptr .. ptr + size];
                for (init_array.items, 0..) |value, i| {
                    const value_tyv = try self.resolveValue(value);
                    const offset = i * element_size;
                    // TODO: handle values with more than 8 bytes;
                    const value_bytes = self.getBytes(value_tyv);
                    std.debug.print("copying {} at offset: {} bytes: {any}\n", .{ value_tyv.display(self.mod), offset, value_bytes });
                    std.mem.copyForwards(u8, slice[offset .. offset + element_size], value_bytes);
                }
                std.debug.print("array bytes: {any}\n", .{slice});
                try self.setValue(inst_ref, .{ .ty = init_array.ty, .value = .{ .pointer = @intCast(ptr) } });
            },
            .property_by_index => |prop| {
                const tyv = try self.resolveValue(prop.tyv);
                const index_tyv = try self.resolveValue(prop.index);
                const array_bytes = self.getBytes(tyv);
                std.debug.print("array bytes: {any}\n", .{array_bytes});

                const index = index_tyv.value.readAs(u64);
                const item_ty = tyv.ty.getChildTy(self.mod) orelse std.debug.panic("can't get child type of {}\n", .{tyv.ty.display(self.mod)});
                const item_size = item_ty.getSize(self.mod);
                const item_ptr = index * item_size;
                const item_bytes = array_bytes[item_ptr .. item_ptr + item_size];
                var final_value = Module.TypedValue.Imm(item_ty, 0);
                std.mem.copyForwards(u8, final_value.value.bytes[0..item_size], item_bytes);
                try self.setValue(inst_ref, final_value);
            },
            else => {
                std.debug.panic("unhandled instruction: {s}\n", .{@tagName(inst)});
            },
        }
    }
}
inline fn getBytes(self: *@This(), tyv: Module.TypedValue) []const u8 {
    switch (tyv.value) {
        .bytes => |bytes| {
            const size = tyv.ty.getSize(self.mod);
            return bytes[0..size];
        },
        .pointer => |ptr| {
            const size = tyv.ty.getSize(self.mod);
            return self.memory.items[ptr .. ptr + size];
        },
        else => {
            std.debug.panic("can't get bytes from {}\n", .{tyv.display(self.mod)});
        },
    }
}
