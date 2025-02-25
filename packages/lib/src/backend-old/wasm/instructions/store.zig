const std = @import("std");
const Scope = @import("./Scope.zig");
const Sema = @import("../../../sema/Sema.zig");

pub fn emit(scope: *Scope, index: Sema.Instruction.Index) !void {
    const instruction = scope.getInstruction(index);
    const payload_instruction = scope.getInstruction(instruction.data.operand_payload.payload);
    _ = payload_instruction; // autofix
    const base_instruction = scope.getInstruction(instruction.data.operand_payload.operand);
    if (scope.locals.get(instruction.data.operand_payload.operand)) |local| {
        try scope.function.pushInstruction(.{
            .@"local.set" = local.index,
        });
        return;
    }

    switch (base_instruction.typed_value.type) {
        .simple => |simple| switch (simple) {
            // .i8, .i16, .i32 => {
            //     const local = scope.locals.get(instruction.data.operand_payload.operand) orelse @panic("local not set");
            //     try scope.function.pushInstruction(.{
            //         .local_set = local.index,
            //     });
            // },
            else => {
                std.debug.panic("unimplemented alloc type: {s}", .{@tagName(simple)});
            },
        },
        .complex => |complex| {
            const complex_type = scope.program.sema.getComplexType(complex);
            const main_memory = try scope.backend.getMainMemory();
            switch (complex_type.data) {
                .pointer => |pointer| {
                    switch (pointer.child) {
                        .simple => |simple| switch (simple) {
                            .i8 => try scope.function.pushInstruction(.{ .@"i32.store8" = .{ .memory = main_memory } }),
                            .i16 => try scope.function.pushInstruction(.{ .@"i32.store16" = .{ .memory = main_memory } }),
                            .i32 => try scope.function.pushInstruction(.{ .@"i32.store" = .{ .memory = main_memory } }),
                            .i64 => try scope.function.pushInstruction(.{ .@"i64.store" = .{ .memory = main_memory } }),
                            else => {
                                std.debug.panic("unimplemented alloc type: {s}", .{@tagName(simple)});
                            },
                        },
                        .complex => |c| switch (scope.program.sema.getComplexType(c).data) {
                            .pointer => try scope.function.pushInstruction(.{ .@"i32.store" = .{ .memory = main_memory } }),
                            else => {
                                std.debug.panic("unimplemented alloc type: {s}", .{@tagName(complex_type.data)});
                            },
                        },
                    }

                    // try scope.function.pushInstruction();
                },
                else => {
                    std.debug.panic("unimplemented alloc type: {s}", .{@tagName(complex_type.data)});
                },
            }
        },
    }
}
// fn pushStoreInstruction(scope: *Scope, ty: Sema.Type.Key) !void {
