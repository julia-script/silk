const std = @import("std");
const Scope = @import("./Scope.zig");
const Sema = @import("../../../sema/Sema.zig");

pub fn emit(scope: *Scope, index: Sema.Instruction.Index) !void {
    const instruction = scope.getInstruction(index);
    const lhs_instruction = scope.getInstruction(instruction.data.bin_op.lhs);
    try scope.function.pushInstruction(
        switch (lhs_instruction.typed_value.type) {
            .simple => |simple| switch (simple) {
                .i8, .i16, .i32 => switch (instruction.op) {
                    .gt => .@"i32.gt_s",
                    .ge => .@"i32.ge_s",
                    .lt => .@"i32.lt_s",
                    .le => .@"i32.le_s",
                    .eq => .@"i32.eq",
                    .ne => .@"i32.ne",
                    else => @panic("TODO"),
                },
                .i64 => switch (instruction.op) {
                    .gt => .@"i64.gt_s",
                    .ge => .@"i64.ge_s",
                    .lt => .@"i64.lt_s",
                    .le => .@"i64.le_s",
                    .eq => .@"i64.eq",
                    .ne => .@"i64.ne",
                    else => @panic("TODO"),
                },
                .u8, .u16, .u32 => switch (instruction.op) {
                    .gt => .@"i32.gt_u",
                    .ge => .@"i32.ge_u",
                    .lt => .@"i32.lt_u",
                    .le => .@"i32.le_u",
                    .eq => .@"i32.eq",
                    .ne => .@"i32.ne",
                    else => @panic("TODO"),
                },
                .u64 => switch (instruction.op) {
                    .gt => .@"i64.gt_u",
                    .ge => .@"i64.ge_u",
                    .lt => .@"i64.lt_u",
                    .le => .@"i64.le_u",
                    .eq => .@"i64.eq",
                    .ne => .@"i64.ne",
                    else => @panic("TODO"),
                },

                .f32 => switch (instruction.op) {
                    .eq => .@"f32.eq",
                    .ne => .@"f32.ne",
                    .lt => .@"f32.lt",
                    .le => .@"f32.le",
                    .gt => .@"f32.gt",
                    .ge => .@"f32.ge",
                    else => @panic("TODO"),
                },
                .f64 => switch (instruction.op) {
                    .eq => .@"f64.eq",
                    .ne => .@"f64.ne",
                    .lt => .@"f64.lt",
                    .le => .@"f64.le",
                    .gt => .@"f64.gt",
                    .ge => .@"f64.ge",
                    else => @panic("TODO"),
                },
                else => @panic("TODO"),
            },
            .complex => @panic("TODO"),
        },
    );
}
