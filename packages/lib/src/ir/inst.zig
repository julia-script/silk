const utils = @import("./utils.zig");
const Op = @import("./opcodes.zig").Op;
const tyval = @import("./tyval.zig");
const std = @import("std");
const Block = @import("./Block.zig");
const Value = @import("./val.zig").Value;
const Dfg = @import("./Dfg.zig");
const Module = @import("./Module.zig");
const FunctionDeclaration = @import("./FunctionDeclaration.zig");
pub const InstData = union(enum) {
    binary: struct {
        op: Op,
        args: [2]Value,
    },
    operand: struct {
        op: Op,
        value: Value,
    },
    @"return": struct {
        op: Op,
        value: Value,
    },
    branch: struct {
        cond: Value,
        // then, else, finally
        args: [3]?Block.Ref,
    },
    loop: struct {
        // body, finally
        args: [2]?Block.Ref,
    },
    @"break": struct {
        target: InstData.Ref,
        value: Value,
    },
    store: struct {
        local: Dfg.Local.Ref,
        value: Value,
    },
    storeGlobal: struct {
        global: Module.Decl.Ref,
        value: Value,
    },
    call: struct {
        callee: Module.Decl.Ref,
        args: []Value,
    },

    pub const Ref = utils.MakeRef(.inst, InstData);

    // pub const InstVal = union(enum) {
    //     imm: tyval.TyVal,
    //     ref: Inst.Ref,
    //     pub fn fromRef(ref: Inst.Ref) InstVal {
    //         return .{ .ref = ref };
    //     }
    //     pub fn fromImm(ty: tyval.Ty, value: anytype) !InstVal {
    //         return .{ .imm = try tyval.TyVal.fromConst(ty, value) };
    //     }

    //     pub fn format(
    //         self: InstVal,
    //         comptime _: []const u8,
    //         _: std.fmt.FormatOptions,
    //         writer: std.io.AnyWriter,
    //     ) !void {
    //         switch (self) {
    //             .imm => |imm| try writer.print("{}", .{imm}),
    //             .ref => |ref| try writer.print("{}", .{ref}),
    //         }
    //     }
    // };
};
