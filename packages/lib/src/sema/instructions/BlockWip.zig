const std = @import("std");
const Sema = @import("../Sema.zig");
const Entity = @import("../gen.zig").Entity;
const Self = @This();
const Hir = @import("../../hir.zig");

parent: ?*Self,
scope: *Scope,
instructions_list: std.ArrayListUnmanaged(Sema.Instruction.Index) = .{},
is_comptime: bool = false,
hir_inst_index: Hir.Inst.Index,

pub const Scope = struct {
    instructions: std.ArrayListUnmanaged(Sema.Instruction) = .{},
    entity: Entity.Key,
    builder: *Sema.Builder,
    allocator: std.mem.Allocator,
    pub fn makeBlock(self: *Scope, parent: ?*Self, hir_inst_index: Hir.Inst.Index, is_comptime: bool) Self {
        return Self{
            .parent = parent,
            .scope = self,
            .instructions_list = .{},
            .is_comptime = self.is_comptime orelse is_comptime,
            .hir_inst_index = hir_inst_index,
        };
    }

    // pub fn emit(self: *Self, ) !void {
    //     try self.instructions.append(instruction);
    // }
};
