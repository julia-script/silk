const std = @import("std");
const Scope = @import("./Scope.zig");
const Sema = @import("../../../sema/Sema.zig");

pub fn emit(scope: *Scope, index: Sema.Instruction.Index) !void {
    const instruction = scope.getInstruction(index);

    switch (instruction.typed_value.type) {
        .simple => |simple| switch (simple) {
            .i8, .i16, .i32 => {
                try scope.function.pushInstruction(.{
                    .@"i32.const" = scope.program.sema.readNumberAsType(i32, instruction.typed_value),
                });
            },

            else => {
                std.debug.panic("unimplemented alloc type: {s}", .{@tagName(simple)});
            },
        },
        .complex => |complex| {
            const complex_type = scope.program.sema.getComplexType(complex);
            // const size = complex_type.size;
            switch (complex_type.data) {
                else => {
                    std.debug.panic("unimplemented alloc type: {s}", .{@tagName(complex_type.data)});
                },
            }
        },
    }
}
