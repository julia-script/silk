const std = @import("std");
const Array = std.ArrayList;
const ArrayMap = std.AutoArrayHashMap;
const HashMap = std.AutoHashMap;
const Inst = @import("./inst.zig").Inst;
const Block = @import("./Block.zig");
const TyVal = @import("./tyval.zig").TyVal;
const Ty = @import("./tyval.zig").Ty;
const Val = @import("./tyval.zig").Val;
const Op = @import("./opcodes.zig").Op;
const utils = @import("./utils.zig");
const debug = @import("../debug.zig");
const InstVal = Inst.InstVal;

bytes: Array(u8),
instructions: Inst.Ref.List(Inst),

values: Inst.Ref.Map(Val),
types: Inst.Ref.Map(Ty),
blocks: Block.Ref.List(Block),
allocator: std.mem.Allocator,

pub const EntryBlock = Block.Ref.from(0);

const Self = @This();
pub fn init(allocator: std.mem.Allocator) !Self {
    var self = create(allocator);
    _ = try self.makeBlock(null);
    return self;
}
pub fn makeBlock(self: *Self, initiator: ?Inst.Ref) !Block.Ref {
    return try self.blocks.append(Block.init(
        self.allocator,
        .body,
        initiator,
    ));
}
fn create(allocator: std.mem.Allocator) Self {
    return Self{
        .bytes = Array(u8).init(allocator),
        .instructions = Inst.Ref.List(Inst).init(allocator),
        .blocks = Block.Ref.List(Block).init(allocator),
        .types = Inst.Ref.Map(Ty).init(allocator),
        .values = Inst.Ref.Map(Val).init(allocator),
        // .immediates = Array(TyVal).init(allocator),
        .allocator = allocator,
    };
}

pub fn deinit(self: *Self) void {
    for (self.blocks.slice()) |*block| {
        block.deinit();
    }
    self.bytes.deinit();
    self.instructions.deinit();
    self.blocks.deinit();
    self.types.deinit();
    self.values.deinit();
    // self.immediates.deinit();
}

pub fn pushInst(self: *Self, block_ref: Block.Ref, inst: Inst) !Inst.Ref {
    const block = self.blocks.getPtr(block_ref);
    if (block.sealed) {
        return error.BlockAlreadySealed;
    }
    const ref = try self.instructions.append(inst);
    try block.instructions.append(ref);
    return ref;
}

pub fn resolveType(self: *Self, inst_val: InstVal) !Ty {
    return switch (inst_val) {
        .imm => |imm| imm.ty,
        .ref => |ref| {
            const ty_ref = self.types.get(ref) orelse @panic("unknown instruction");
            return ty_ref;
        },
    };
}

pub fn pushBinary(self: *Self, block_ref: Block.Ref, op: Op, a: InstVal, b: InstVal) !Inst.Ref {
    const a_ty = try resolveType(self, a);
    const b_ty = try resolveType(self, b);
    debug.assertPrint(a_ty.eql(b_ty), "type mismatch: {s} != {s}", .{ a_ty, b_ty });
    const ref = try self.pushInst(block_ref, Inst{
        .binary = .{
            .op = op,
            .args = .{ a, b },
            .is_comptime = false,
        },
    });
    try self.types.put(ref, a_ty);
    return ref;
}

pub fn formatInst(self: *Self, writer: std.io.AnyWriter, inst_ref: Inst.Ref) !void {
    const inst = self.instructions.get(inst_ref);
    // try writer.print("v{} = ", .{inst_ref.ref});

    switch (inst) {
        .binary => |binary| {
            try writer.print("{} = {} {} {}", .{ inst_ref, binary.op, binary.args[0], binary.args[1] });
        },
        else => {
            // try writer.print("{} = {}", .{ inst_ref, inst });
        },
    }
}

test "Dfg" {
    var dfg = try Self.init(std.testing.allocator);
    defer dfg.deinit();
    const a = try InstVal.fromImm(.u8, 1);
    const b = try InstVal.fromImm(.u8, 2);

    const ref = try dfg.pushBinary(EntryBlock, .add, a, b);
    const c = try InstVal.fromImm(.u8, 3);
    const y = try dfg.pushBinary(
        EntryBlock,
        .sub,
        InstVal.fromRef(ref),
        c,
    );

    const stderr = std.io.getStdErr().writer().any();
    try dfg.formatInst(stderr, ref);
    try stderr.print("\n", .{});
    try dfg.formatInst(stderr, y);
    try stderr.print("\n", .{});

    std.debug.print("Dfg\n", .{});
}
