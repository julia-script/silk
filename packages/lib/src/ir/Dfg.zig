const std = @import("std");
const Array = std.ArrayListUnmanaged;
const ArrayMap = std.AutoArrayHashMap;
const HashMap = std.AutoHashMap;
const InstData = @import("./inst.zig").InstData;
const Block = @import("./Block.zig");
const Ty = @import("./ty.zig").Ty;
const Op = @import("./opcodes.zig").Op;
const utils = @import("./utils.zig");
const debug = @import("../debug.zig");
// const Value = @import("./val.zig").Value;
const FunctionDeclaration = @import("./FunctionDeclaration.zig");
const Module = @import("./Module.zig");
const Set = @import("../data_structures.zig").AutoSetUnmanaged;
const TypedValue = Module.TypedValue;
const Map = std.AutoHashMapUnmanaged;

bytes: Array(u8) = .{},
instructions: InstData.Ref.ListUnmanaged(InstData) = .{},
result: ?TypedValue = null,
inst_values: InstData.Ref.MapUnmanaged(TypedValue) = .{},
types: InstData.Ref.MapUnmanaged(Ty) = .{},
local_values: Local.Ref.ListUnmanaged(Local) = .{},
blocks: Block.Ref.ListUnmanaged(Block) = .{},
allocator: std.mem.Allocator,
arena: std.heap.ArenaAllocator,
dependencies: Map(Module.Decl.Ref, DependsOn) = .{},

pub const DependsOn = enum {
    declaration,
    definition,
};

pub const Local = struct {
    is_param: bool = false,
    index: u32,
    value: TypedValue,
    is_comptime: bool,
    pub const Ref = utils.MakeRef(.local, Local, "\x1b[34mloc{d}\x1b[0m");
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
    };

    // Fixes weird zig memory leak bug
    // try self.local_values.items.ensureUnusedCapacity(1);

    _ = try self.makeBlock(null);

    return self;
}
pub inline fn makeBlock(self: *Self, initiator: ?InstData.Ref) !Block.Ref {
    return try self.blocks.append(self.arena.allocator(), Block.init(
        .body,
        initiator,
    ));
}

pub fn deinit(self: *Self) void {
    // for (self.blocks.slice()) |*block| {
    //     block.deinit();
    // }
    self.arena.deinit();
    // std.debug.print("local_values: {}\n", .{self.local_values.count()});
    // self.local_values.deinit(self.arena.allocator());
    // self.inst_values.deinit(self.arena.allocator());
    // self.bytes.deinit(self.arena.allocator());
    // self.instructions.deinit(self.arena.allocator());
    // self.blocks.deinit(self.arena.allocator());
    // self.types.deinit(self.arena.allocator());
}

pub fn pushLocal(self: *Self, ty: Ty, is_param: bool, is_comptime: bool) !Local.Ref {
    const index: u32 = @intCast(self.local_values.count());
    const ref = try self.local_values.append(self.arena.allocator(), Local{
        .index = index,
        .value = TypedValue.Runtime(ty, is_comptime),
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
    const ref = try self.instructions.append(self.arena.allocator(), inst);
    try block.instructions.append(self.arena.allocator(), ref);
    return ref;
}
pub fn setDependency(self: *Self, dep: Module.Decl.Ref, depends_on: DependsOn) !void {
    const gop = try self.dependencies.getOrPut(self.arena.allocator(), dep);
    if (gop.found_existing) {
        if (gop.value_ptr.* == .definition) {
            return;
        }
        gop.value_ptr.* = depends_on;
        return;
    }
    gop.value_ptr.* = depends_on;
}

pub fn pushBinary(self: *Self, block_ref: Block.Ref, ty: Ty, op: Op, a: TypedValue, b: TypedValue, is_comptime: bool) !TypedValue {
    const ref = try self.pushInst(block_ref, .{
        .binary = .{
            .op = op,
            .args = .{ a, b },
        },
    });
    const val = TypedValue.Inst(
        ty,
        ref,
        is_comptime,
    );
    try self.inst_values.put(self.arena.allocator(), ref, val);
    return val;
}
pub fn pushOperand(self: *Self, block_ref: Block.Ref, op: Op, ty: Ty, value: TypedValue, is_comptime: bool) !TypedValue {
    const ref = try self.pushInst(block_ref, .{
        .operand = .{
            .op = op,
            .value = value,
        },
    });
    const val = TypedValue.Inst(
        ty,
        ref,
        is_comptime,
    );
    try self.inst_values.put(self.arena.allocator(), ref, val);

    return val;
}
pub fn pushReturn(self: *Self, block_ref: Block.Ref, value: TypedValue, is_comptime: bool) !void {
    _ = is_comptime; // autofix
    _ = try self.pushInst(block_ref, .{
        .@"return" = .{
            .op = .@"return",
            .value = value,
        },
    });
}
pub fn pushStoreGlobal(self: *Self, block_ref: Block.Ref, global: Module.Decl.Ref, value: TypedValue) !void {
    _ = try self.pushInst(block_ref, .{
        .storeGlobal = .{
            .global = global,
            .value = value,
        },
    });
}
pub fn pushStore(self: *Self, block_ref: Block.Ref, local: Local.Ref, value: TypedValue) !void {
    _ = try self.pushInst(block_ref, .{
        .store = .{
            .local = local,
            .value = value,
        },
    });
}
pub fn pushBranch(self: *Self, block_ref: Block.Ref, cond: TypedValue, args: [3]?Block.Ref) !InstData.Ref {
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
pub fn pushBreak(self: *Self, block_ref: Block.Ref, target: InstData.Ref, value: ?TypedValue, is_comptime: bool) !InstData.Ref {
    _ = is_comptime; // autofix
    return try self.pushInst(block_ref, .{
        .@"break" = .{
            .target = target,
            .value = value orelse TypedValue.Void,
        },
    });
}

pub fn pushCall(self: *Self, module: *Module, block_ref: Block.Ref, callee: TypedValue, args: []TypedValue, is_comptime: bool) !TypedValue {
    const ref = try self.pushInst(block_ref, .{
        .call = .{
            .callee = callee,
            .args = try self.arena.allocator().dupe(TypedValue, args),
        },
    });

    const callee_ty = callee.ty;

    const ty_data = module.acceptTyData(callee_ty, .func) orelse
        std.debug.panic("{} is not callable", .{callee_ty});

    const signature = module.getSignature(ty_data.func.signature);

    const val = TypedValue.Inst(
        signature.ret,
        ref,
        is_comptime,
    );
    try self.inst_values.put(self.arena.allocator(), ref, val);
    return val;
}
pub fn pushInitArray(self: *Self, block_ref: Block.Ref, ty: Module.Ty, items: []TypedValue, is_comptime: bool) !TypedValue {
    const ref = try self.pushInst(block_ref, .{
        .init_array = .{
            .ty = ty,
            .items = try self.arena.allocator().dupe(TypedValue, items),
        },
    });

    const val = TypedValue.Inst(
        ty,
        ref,
        is_comptime,
    );
    try self.inst_values.put(self.arena.allocator(), ref, val);
    return val;
}
pub fn pushInitStruct(self: *Self, block_ref: Block.Ref, ty: Module.Ty, keys: []const []const u8, values: []const TypedValue, is_comptime: bool) !TypedValue {
    var keys_dupe = try self.arena.allocator().dupe([]const u8, keys);
    for (keys_dupe, 0..) |key, i| {
        keys_dupe[i] = try self.arena.allocator().dupe(u8, key);
    }
    const ref = try self.pushInst(block_ref, .{
        .init_struct = .{
            .ty = ty,
            .keys = keys_dupe,
            .values = try self.arena.allocator().dupe(TypedValue, values),
        },
    });

    const val = TypedValue.Inst(
        ty,
        ref,
        is_comptime,
    );
    try self.inst_values.put(self.arena.allocator(), ref, val);
    return val;
}

// pub fn getParam(self: *Self, index: u32) Value {
//     return self.local_values.items[index].value;
// }
pub fn formatInst(self: *Self, writer: std.io.AnyWriter, module: *const Module, inst_ref: InstData.Ref) !void {
    const inst = self.instructions.get(inst_ref);
    if (self.inst_values.get(inst_ref)) |val| {
        _ = val; // autofix
        try writer.print("{} = {}", .{ inst_ref, inst.display(module) });
    } else {
        switch (inst) {
            .loop, .branch => {
                try writer.print("({}) {}", .{ inst_ref, inst.display(module) });
            },
            else => {
                try writer.print("{}", .{inst.display(module)});
            },
        }
    }
}

fn displayFn(self: Self, writer: std.io.AnyWriter, module: *const Module) anyerror!void {
    const instruction_indent = "    ";
    var block_iter = self.blocks.iter();
    for (self.local_values.slice()) |local| {
        if (local.is_param) {
            continue;
        }
        try writer.print(instruction_indent ++ "{}: {}\n", .{ Local.Ref.from(local.index), local.value.ty.display(module) });
    }
    while (block_iter.next()) |entry| {
        const block = entry.item;
        const ref = entry.ref;

        if (block.initiator == null) {
            try writer.print("Entry:\n", .{});
        } else {
            try writer.print("{}:\n", .{ref});
        }
        for (block.instructions.items) |inst_ref| {
            try writer.print("{s}", .{instruction_indent});
            // try self.formatInst(writer, self.module, inst_ref);
            const inst = self.instructions.get(inst_ref);
            if (self.inst_values.get(inst_ref)) |val| {
                _ = val; // autofix
                try writer.print("{} = {}", .{ inst_ref, inst.display(module) });
            } else {
                switch (inst) {
                    .loop, .branch => {
                        try writer.print("({}) {}", .{ inst_ref, inst.display(module) });
                    },
                    else => {
                        try writer.print("{}", .{inst.display(module)});
                    },
                }
            }
            try writer.print("\n", .{});
        }
        try writer.print("\n", .{});
    }
    // try writer.print("{}", .{self});
}
pub fn display(self: Self, module: *const Module) Module.utils.MakeDisplay(Self, displayFn) {
    return .{ .value = self, .module = module };
}
test "Dfg" {
    var dfg = try Self.init(std.testing.allocator);
    defer dfg.deinit();
    const a = TypedValue.Imm(.u8, 1, false);
    const b = TypedValue.Imm(.u8, 2, false);

    const inst = try dfg.pushBinary(
        EntryBlock,
        .u8,
        .add,
        a,
        b,
        true,
    );
    const c = TypedValue.Imm(.u8, 3, false);
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
    try dfg.formatInst(stderr, undefined, (.{ .ref = 0 }));
    // try stderr.print("\n", .{});
    // try dfg.formatInst(stderr, y);
    // try stderr.print("\n", .{});

    // std.debug.print("Dfg\n", .{});
}
