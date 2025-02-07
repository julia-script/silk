const utils = @import("./utils.zig");
const std = @import("std");
const Self = @This();
const Module = @import("./Module.zig");
pub const Ref = utils.MakeRef(.def, Self);
const Dfg = @import("./Dfg.zig");
const Array = std.ArrayList;
const Value = @import("./val.zig").Value;

dfg: Dfg,
kind: Kind,

pub const Kind = enum {
    function_body,
    inline_expression,
};
pub fn init(kind: Kind, dfg: Dfg) Self {
    return .{ .kind = kind, .dfg = dfg };
}

pub fn deinit(self: *Self) void {
    self.dfg.deinit();
}

pub const Formatable = struct {
    def: *Self,
    module: *const Module,
    pub fn format(
        self: Formatable,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: std.io.AnyWriter,
    ) !void {
        const instruction_indent = "    ";
        var block_iter = self.def.dfg.blocks.iter();
        for (self.def.dfg.local_values.slice(), 0..) |local, i| {
            if (local.is_param) {
                continue;
            }
            try writer.print(instruction_indent ++ "l{}: {}\n", .{ i, local.value.getTy() });
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
                try self.def.dfg.formatInst(writer, inst_ref);

                try writer.print("\n", .{});
            }
            try writer.print("\n", .{});
        }
    }
};
pub fn formatable(self: *Self, module: *const Module) Formatable {
    return .{ .def = self, .module = module };
}
