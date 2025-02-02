const std = @import("std");
const Array = std.ArrayList;
const utils = @import("./utils.zig");
const Inst = @import("./inst.zig").Inst;

// Instruction that initiated the block. Only root block does not have an initiator.
initiator: ?Inst.Ref,
instructions: Array(Inst.Ref),

sealed: bool = false,
kind: Kind = .value,

const Self = @This();
pub const Kind = enum {
    value,
    body,
};
pub const Ref = utils.MakeRef(.block, Self);

pub fn init(allocator: std.mem.Allocator, kind: Kind, initiator: ?Inst.Ref) Self {
    return Self{
        .kind = kind,
        .initiator = initiator,
        .instructions = Array(Inst.Ref).init(allocator),
    };
}
pub fn deinit(self: *Self) void {
    self.instructions.deinit();
}

pub fn seal(self: *Self) void {
    self.sealed = true;
}
