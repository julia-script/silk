const std = @import("std");
const Array = std.ArrayListUnmanaged;
const utils = @import("./utils.zig");
const InstData = @import("./inst.zig").InstData;
const Module = @import("./Module.zig");

// Instruction that initiated the block. Only root block does not have an initiator.
initiator: ?InstData.Ref,
instructions: Array(InstData.Ref) = .{},
is_comptime: bool = false,
dependencies: Array(Module.Dfg.Local.Ref) = .{},

sealed: bool = false,
kind: Kind = .value,

const Self = @This();
pub const Kind = enum {
    value,
    body,
};
pub const Ref = utils.MakeRef(.block, Self, "\x1b[35mblk{d}\x1b[0m");

pub fn init(kind: Kind, initiator: ?InstData.Ref, is_comptime: bool) Self {
    return Self{
        .kind = kind,
        .initiator = initiator,
        .is_comptime = is_comptime,
        // .instructions = Array(InstData.Ref).init(allocator),
    };
}
pub fn deinit(self: *Self) void {
    self.instructions.deinit();
}

pub fn seal(self: *Self) void {
    self.sealed = true;
}

pub fn isEmpty(self: *Self) bool {
    return self.instructions.items.len == 0;
}
