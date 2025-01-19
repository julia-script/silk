const std = @import("std");
const Scope = @import("./Scope.zig");
const Sema = @import("../../../sema/Sema.zig");

pub fn emit(scope: *Scope, index: Sema.Instruction.Index) !void {
    const instruction = scope.getInstruction(index);
    try scope.function.pushInstruction(
        switch (instruction.typed_value.type) {
            .simple => |simple| switch (simple) {
                .i8, .i16, .i32 => switch (instruction.op) {
                    .add => .@"i32.add",
                    .sub => .@"i32.sub",
                    .mul => .@"i32.mul",
                    .div => .@"i32.div_s",
                    else => @panic("TODO"),
                },
                .i64 => switch (instruction.op) {
                    .add => .@"i64.add",
                    .sub => .@"i64.sub",
                    .mul => .@"i64.mul",
                    .div => .@"i64.div_s",
                    else => @panic("TODO"),
                },
                .u8, .u16, .u32 => switch (instruction.op) {
                    .add => .@"i32.add",
                    .sub => .@"i32.sub",
                    .mul => .@"i32.mul",
                    .div => .@"i32.div_u",
                    else => @panic("TODO"),
                },
                .u64 => switch (instruction.op) {
                    .add => .@"i64.add",
                    .sub => .@"i64.sub",
                    .mul => .@"i64.mul",
                    .div => .@"i64.div_u",
                    else => @panic("TODO"),
                },
                .usize => switch (scope.program.target.getPointerSize()) {
                    4 => switch (instruction.op) {
                        .add => .@"i32.add",
                        .sub => .@"i32.sub",
                        .mul => .@"i32.mul",
                        .div => .@"i32.div_u",
                        else => @panic("TODO"),
                    },
                    8 => switch (instruction.op) {
                        .add => .@"i64.add",
                        .sub => .@"i64.sub",
                        .mul => .@"i64.mul",
                        .div => .@"i64.div_u",
                        else => @panic("TODO"),
                    },
                    else => @panic("TODO"),
                },
                .f32 => switch (instruction.op) {
                    .add => .@"f32.add",
                    .sub => .@"f32.sub",
                    .mul => .@"f32.mul",
                    .div => .@"f32.div",
                    else => @panic("TODO"),
                },
                .f64 => switch (instruction.op) {
                    .add => .@"f64.add",
                    .sub => .@"f64.sub",
                    .mul => .@"f64.mul",
                    .div => .@"f64.div",
                    else => @panic("TODO"),
                },
                else => @panic("TODO"),
            },
            .complex => @panic("TODO"),
        },
    );
}
