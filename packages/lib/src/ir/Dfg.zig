const std = @import("std");
const Array = std.ArrayList;
const ArrayMap = std.AutoArrayHashMap;
const HashMap = std.AutoHashMap;
const InstData = @import("./inst.zig").InstData;
const Block = @import("./Block.zig");
const TyVal = @import("./tyval.zig").TyVal;
const Ty = @import("./tyval.zig").Ty;
const Val = @import("./tyval.zig").Val;
const Op = @import("./opcodes.zig").Op;
const utils = @import("./utils.zig");
const debug = @import("../debug.zig");
const Value = @import("./val.zig").Value;
const FunctionDeclaration = @import("./FunctionDeclaration.zig");
const Module = @import("./Module.zig");
const Set = @import("../data_structures.zig").AutoSet;

bytes: Array(u8),
instructions: InstData.Ref.List(InstData),
result: ?Value = null,
values: InstData.Ref.Map(Value),
types: InstData.Ref.Map(Ty),
local_values: Local.Ref.List(Local),
blocks: Block.Ref.List(Block),
allocator: std.mem.Allocator,
arena: std.heap.ArenaAllocator,
dependencies: Set(Module.Decl.Ref),

pub const Local = struct {
    is_param: bool = false,
    index: u32,
    value: Value,
    is_comptime: bool,
    pub const Ref = utils.MakeRef(.local, Local);
};
// stack_slots: Array(StackSlot),
// pub const StackSlot = struct {
//     ty: Ty,
//     is_mutable: bool,
//     is_comptime: bool,
//     pub const Ref = utils.MakeRef(.ss, StackSlot);
// };

pub const EntryBlock = Block.Ref.from(0);

const Self = @This();
pub fn init(allocator: std.mem.Allocator) !Self {
    var self = Self{
        .arena = std.heap.ArenaAllocator.init(allocator),
        .allocator = allocator,
        .bytes = Array(u8).init(allocator),
        .instructions = InstData.Ref.List(InstData).init(allocator),
        .blocks = Block.Ref.List(Block).init(allocator),
        .types = InstData.Ref.Map(Ty).init(allocator),
        .values = InstData.Ref.Map(Value).init(allocator),
        .local_values = Local.Ref.List(Local).init(allocator),
        .dependencies = Set(Module.Decl.Ref).init(allocator),
    };

    // Fixes weird zig memory leak bug
    try self.local_values.items.ensureUnusedCapacity(1);

    _ = try self.makeBlock(null);

    return self;
}
pub inline fn makeBlock(self: *Self, initiator: ?InstData.Ref) !Block.Ref {
    return try self.blocks.append(Block.init(
        self.allocator,
        .body,
        initiator,
    ));
}

pub fn deinit(self: *Self) void {
    for (self.blocks.slice()) |*block| {
        block.deinit();
    }
    self.arena.deinit();
    std.debug.print("local_values: {}\n", .{self.local_values.count()});
    self.local_values.deinit();
    self.bytes.deinit();
    self.instructions.deinit();
    self.blocks.deinit();
    self.types.deinit();
    self.values.deinit();
}

pub fn pushLocal(self: *Self, ty: Ty, is_param: bool, is_comptime: bool) !Local.Ref {
    const index: u32 = @intCast(self.local_values.count());
    const ref = try self.local_values.append(Local{
        .index = index,
        .value = Value.Local(index, ty, .undefined, is_comptime),
        .is_comptime = is_comptime,
        .is_param = is_param,
    });
    return ref;
}
pub fn pushInst(self: *Self, block_ref: Block.Ref, inst: InstData) !InstData.Ref {
    const block = self.blocks.getPtr(block_ref);
    if (block.sealed) {
        return error.BlockAlreadySealed;
    }
    const ref = try self.instructions.append(inst);
    try block.instructions.append(ref);
    return ref;
}

pub fn pushBinary(self: *Self, block_ref: Block.Ref, ty: Ty, op: Op, a: Value, b: Value, is_comptime: bool) !Value {
    const ref = try self.pushInst(block_ref, .{
        .binary = .{
            .op = op,
            .args = .{ a, b },
        },
    });
    const val = Value.Inst(
        ref.ref,
        ty,
        .runtime,
        is_comptime,
    );
    try self.values.put(ref, val);
    return val;
}
pub fn pushOperand(self: *Self, block_ref: Block.Ref, op: Op, ty: Ty, value: Value, is_comptime: bool) !Value {
    const ref = try self.pushInst(block_ref, .{
        .operand = .{
            .op = op,
            .value = value,
        },
    });
    const val = Value.Inst(
        ref.ref,
        ty,
        .runtime,
        is_comptime,
    );
    try self.values.put(ref, val);

    return val;
}
pub fn pushReturn(self: *Self, block_ref: Block.Ref, value: Value, is_comptime: bool) !void {
    _ = is_comptime; // autofix
    _ = try self.pushInst(block_ref, .{
        .@"return" = .{
            .op = .@"return",
            .value = value,
        },
    });
}
pub fn pushStoreGlobal(self: *Self, block_ref: Block.Ref, global: Module.Decl.Ref, value: Value) !void {
    _ = try self.pushInst(block_ref, .{
        .storeGlobal = .{
            .global = global,
            .value = value,
        },
    });
}
pub fn pushStore(self: *Self, block_ref: Block.Ref, local: Local.Ref, value: Value) !void {
    _ = try self.pushInst(block_ref, .{
        .store = .{
            .local = local,
            .value = value,
        },
    });
}
pub fn pushBranch(self: *Self, block_ref: Block.Ref, cond: Value, args: [3]?Block.Ref) !InstData.Ref {
    return try self.pushInst(block_ref, .{
        .branch = .{
            .cond = cond,
            .args = args,
        },
    });
}
pub fn pushLoop(self: *Self, block_ref: Block.Ref, args: [2]?Block.Ref) !InstData.Ref {
    return try self.pushInst(block_ref, .{
        .loop = .{
            .args = args,
        },
    });
}
pub fn pushBreak(self: *Self, block_ref: Block.Ref, target: InstData.Ref, value: ?Value, is_comptime: bool) !InstData.Ref {
    _ = is_comptime; // autofix
    return try self.pushInst(block_ref, .{
        .@"break" = .{
            .target = target,
            .value = value orelse Value.Imm(.void, .void),
        },
    });
}

pub fn pushCall(self: *Self, module: *Module, block_ref: Block.Ref, callee: Value, args: []Value, is_comptime: bool) !Value {
    const ref = try self.pushInst(block_ref, .{
        .call = .{
            .callee = callee,
            .args = try self.arena.allocator().dupe(Value, args),
        },
    });

    const callee_ty = callee.getTy();

    const ty_data = module.acceptTyData(callee_ty, .func) orelse
        std.debug.panic("{} is not callable", .{callee_ty});

    const signature = module.getSignature(ty_data.func.signature);

    const val = Value.Inst(
        ref.ref,
        signature.ret,
        .runtime,
        is_comptime,
    );
    try self.values.put(ref, val);
    return val;
}
pub fn pushInitArray(self: *Self, block_ref: Block.Ref, ty: Module.Ty, items: []Value, is_comptime: bool) !Value {
    const ref = try self.pushInst(block_ref, .{
        .init_array = .{
            .ty = ty,
            .items = try self.arena.allocator().dupe(Value, items),
        },
    });

    const val = Value.Inst(
        ref.ref,
        ty,
        .runtime,
        is_comptime,
    );
    try self.values.put(ref, val);
    return val;
}
// pub fn getParam(self: *Self, index: u32) Value {
//     return self.local_values.items[index].value;
// }
pub fn formatInst(self: *Self, writer: std.io.AnyWriter, inst_ref: InstData.Ref) !void {
    const inst = self.instructions.get(inst_ref);
    if (self.values.get(inst_ref)) |val| {
        try writer.print("{}: {} = ", .{ val, val.getTy() });
    }

    switch (inst) {
        .binary => |binary| {
            try writer.print("{} {} {}", .{ binary.op, binary.args[0], binary.args[1] });
        },
        .store => |store| {
            try writer.print("store l{} {}", .{ store.local.ref, store.value });
        },
        .storeGlobal => |store| {
            try writer.print("store_global {} {}", .{ store.global, store.value });
        },
        .branch => |branch| {
            try writer.print("({})branch {} {?}, {?}, {?}", .{ inst_ref, branch.cond, branch.args[0], branch.args[1], branch.args[2] });
        },
        .loop => |loop| {
            try writer.print("({})loop {?}, {?}", .{ inst_ref, loop.args[0], loop.args[1] });
        },
        .operand => |operand| {
            try writer.print("{} {}", .{ operand.op, operand.value });
        },
        .@"return" => |@"return"| {
            try writer.print("return {}", .{@"return".value});
        },
        .@"break" => |@"break"| {
            try writer.print("break {} {}", .{ @"break".target, @"break".value });
        },
        .call => |call| {
            try writer.print("call {}", .{call.callee});
            if (call.args.len > 0) {
                try writer.print(" with ", .{});
            }
            for (call.args, 0..) |arg, i| {
                if (i > 0) {
                    try writer.print(", ", .{});
                }
                try writer.print("{}", .{arg});
            }
        },
        .init_array => |init_array| {
            try writer.print("init_array {}", .{init_array.ty});
            if (init_array.items.len > 0) {
                try writer.print(" with ", .{});
            }
            for (init_array.items, 0..) |item, i| {
                if (i > 0) {
                    try writer.print(", ", .{});
                }
                try writer.print("{}", .{item});
            }
        },

        // else => {
        //     // try writer.print("{} = {}", .{ inst_ref, inst });
        // },
    }
}

test "Dfg" {
    var dfg = try Self.init(std.testing.allocator);
    defer dfg.deinit();
    const a = Value.Const(.u8, 1);
    const b = Value.Const(.u8, 2);

    const inst = try dfg.pushBinary(
        EntryBlock,
        .u8,
        .add,
        a,
        b,
        true,
    );
    const c = Value.Const(.u8, 3);
    const y = try dfg.pushBinary(
        EntryBlock,
        .u8,
        .sub,
        inst,
        c,
        false,
    );
    _ = y; // autofix
    // _ = y; // autofix

    const stderr = std.io.getStdErr().writer().any();
    _ = stderr; // autofix
    // try dfg.formatInst(stderr, ref);
    // try stderr.print("\n", .{});
    // try dfg.formatInst(stderr, y);
    // try stderr.print("\n", .{});

    // std.debug.print("Dfg\n", .{});
}
