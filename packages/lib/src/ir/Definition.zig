const utils = @import("./utils.zig");
const std = @import("std");
const Self = @This();
const Module = @import("./Module.zig");
pub const Ref = utils.MakeRef(.def, Self, "def{d}");
const Dfg = @import("./Dfg.zig");
const Array = std.ArrayList;
const Value = @import("./val.zig").Value;

dfg: Dfg,

pub fn deinit(self: *Self) void {
    self.dfg.deinit();
}

fn displayFn(self: Self, writer: std.io.AnyWriter, module: *const Module) anyerror!void {
    try writer.print("{}", .{self.dfg.display(module)});
}
pub fn display(self: Self, module: *const Module) Module.utils.MakeDisplay(Self, displayFn) {
    return .{ .value = self, .module = module };
}
