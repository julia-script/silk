const std = @import("std");
const Scope = @import("./Scope.zig");
const Sema = @import("../../../sema/Sema.zig");

pub fn emit(scope: *Scope, index: Sema.Instruction.Index) !void {
    const instruction = scope.getInstruction(index);
    _ = type;
    // instruction.data.alloc.type
    const name = scope.program.sema.getSlice(instruction.data.alloc.name);
    switch (instruction.data.alloc.type) {
        .simple => |simple| switch (simple) {
            .i8, .i16, .i32 => {
                _ = try scope.pushLocal(index, name, .i32);
            },

            else => {
                std.debug.panic("unimplemented alloc type: {s}", .{@tagName(simple)});
            },
        },
        .complex => |complex| {
            const complex_type = scope.program.sema.getComplexType(complex);
            const offset = scope.stack_alloc_size;
            const size = complex_type.size;
            if (scope.stack_alloc_size == 0) {
                scope.stack_pointer_local = try scope.pushLocal(null, "__base_pointer", .i32);
            }

            scope.stack_alloc_size += @intCast(size);

            const pointer = try scope.pushLocal(index, name, scope.backend.pointer_type);

            try scope.function.pushInstruction(.{ .@"local.get" = scope.stack_pointer_local.? });

            switch (scope.backend.pointer_type) {
                .i32 => try scope.function.pushInstruction(.{ .@"i32.const" = @intCast(offset) }),
                .i64 => try scope.function.pushInstruction(.{ .@"i64.const" = @intCast(offset) }),
                else => unreachable,
            }

            try scope.function.pushInstruction(.{ .@"i32.add" = {} });
            try scope.function.pushInstruction(.{ .@"local.set" = pointer });
        },
    }
}
