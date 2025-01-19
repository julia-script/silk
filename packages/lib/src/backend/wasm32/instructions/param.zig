const std = @import("std");
const Scope = @import("./Scope.zig");
const Sema = @import("../../../sema/Sema.zig");

pub fn emit(scope: *Scope, index: Sema.Instruction.Index) !void {
    const instruction = scope.getInstruction(index);
    const param_index = instruction.data.param.index;
    switch (instruction.typed_value.type) {
        .simple => |simple| switch (simple) {
            .i8, .i16, .i32 => {
                const param_name = scope.program.sema.getSlice(instruction.data.param.name)[scope.function.name.len + 2 ..];

                _ = try scope.function.pushParam(param_name, .i32);
                try scope.setLocal(index, .{ .index = @intCast(param_index), .name = param_name });
            },
            else => {
                @panic("TODO");
            },
        },
        .complex => @panic("TODO"),
    }
}
