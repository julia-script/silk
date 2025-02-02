const utils = @import("./utils.zig");
const Op = @import("./opcodes.zig").Op;
const tyval = @import("./tyval.zig");
const std = @import("std");
const Block = @import("./Block.zig");
pub const Inst = union(enum) {
    binary: struct {
        op: Op,
        args: [2]InstVal,
        is_comptime: bool,
    },
    br_if: struct {
        cond: InstVal,
        args: [3]?Block.Ref,
        is_comptime: bool,
    },

    pub const Ref = utils.MakeRef(.inst, Inst);

    pub const InstVal = union(enum) {
        imm: tyval.TyVal,
        ref: Inst.Ref,
        pub fn fromRef(ref: Inst.Ref) InstVal {
            return .{ .ref = ref };
        }
        pub fn fromImm(ty: tyval.Ty, value: anytype) !InstVal {
            return .{ .imm = try tyval.TyVal.fromConst(ty, value) };
        }

        pub fn format(
            self: InstVal,
            comptime _: []const u8,
            _: std.fmt.FormatOptions,
            writer: std.io.AnyWriter,
        ) !void {
            switch (self) {
                .imm => |imm| try writer.print("{}", .{imm}),
                .ref => |ref| try writer.print("{}", .{ref}),
            }
        }
    };
};
