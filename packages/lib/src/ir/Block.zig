const std = @import("std");
const Array = std.ArrayList;
const utils = @import("./utils.zig");
const InstData = @import("./inst.zig").InstData;

// Instruction that initiated the block. Only root block does not have an initiator.
initiator: ?InstData.Ref,
instructions: Array(InstData.Ref),

sealed: bool = false,
kind: Kind = .value,

const Self = @This();
pub const Kind = enum {
    value,
    body,
};
pub const Ref = utils.MakeRef(.block, Self);

pub fn init(allocator: std.mem.Allocator, kind: Kind, initiator: ?InstData.Ref) Self {
    return Self{
        .kind = kind,
        .initiator = initiator,
        .instructions = Array(InstData.Ref).init(allocator),
    };
}
pub fn deinit(self: *Self) void {
    self.instructions.deinit();
}

pub fn seal(self: *Self) void {
    self.sealed = true;
}
