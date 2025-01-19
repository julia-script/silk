const std = @import("std");
const Scope = @import("./Scope.zig");
const Sema = @import("../../../sema/Sema.zig");

pub fn emit(scope: *Scope, index: Sema.Instruction.Index) !void {
    const instruction = scope.getInstruction(index);
    const base_instruction = scope.getInstruction(instruction.data.get_element_pointer.base);
    const base_local = scope.locals.get(instruction.data.get_element_pointer.base) orelse unreachable;
    // const base
    const unwrapped_base_type = scope.program.sema.builder.unwrapPointerType(base_instruction.typed_value.type) orelse unreachable;

    try scope.function.pushInstruction(.{
        .@"local.get" = base_local.index,
    });
    const complex_type = scope.program.sema.getComplexType(unwrapped_base_type);
    switch (complex_type.data) {
        .array => |array_type| {
            const element_size = scope.program.sema.getTypeSize(array_type.child);
            switch (instruction.data.get_element_pointer.index) {
                .constant => |constant| {
                    const element_index = scope.program.sema.readNumberAsType(usize, constant);
                    if (element_index == 0) return;
                    try scope.function.pushInstruction(.{
                        .@"i32.const" = @intCast(element_index * element_size),
                    });
                    try scope.function.pushInstruction(.{ .@"i32.add" = {} });
                },
                else => {
                    std.debug.panic("unimplemented get element pointer index: {s}", .{@tagName(instruction.data.get_element_pointer.index)});
                },
            }
        },
        .@"struct" => |struct_type| {
            const fields = scope.program.sema.getList(struct_type.fields);
            switch (instruction.data.get_element_pointer.index) {
                .constant => |constant| {
                    const element_index = scope.program.sema.readNumberAsType(usize, constant);
                    if (element_index == 0) return;
                    const field_type = scope.program.sema.getComplexType(Sema.Type.Key.decode(fields[element_index]));

                    try scope.function.pushInstruction(.{
                        .@"i32.const" = @intCast(field_type.data.struct_field.offset),
                    });
                    try scope.function.pushInstruction(.{ .@"i32.add" = {} });
                },
                else => {
                    std.debug.panic("unimplemented get element pointer index: {s}", .{@tagName(instruction.data.get_element_pointer.index)});
                },
            }
        },
        else => {
            std.debug.panic("unimplemented get element pointer type: {s}", .{@tagName(complex_type.data)});
        },
        // .@"struct" => |struct_type| {
        //     const entity = scope.builder.getEntity(struct_type.entity);
        //     const field = entity.data.module_declaration.fields.get(property_name_range) orelse {
        //         std.debug.panic("error: property '{s}' not found in struct '{s}'", .{
        //             scope.builder.getSlice(property_name_range),
        //             scope.builder.getSlice(entity.name),
        //         });
        //     };
        // },
    }

    // ├ %13: *[3]i32 = .alloc type=([3]i32) mutable=(false) name=("foo")      ; *[3]i32@fff20
    // ├ %14: *i32 = .get_element_pointer base=(%13) index=(usize{ 0 })        ; *i32@fff20
    // ├ %15: i32 = .cast operand=(%5)                                         ; i32{ 1 }
    // ├ %16: void = .store operand=(%14) payload=(%15)                        ; void
    // ├ %17: *i32 = .get_element_pointer base=(%13) index=(usize{ 1 })        ; *i32@fff24
    // ├ %18: i32 = .cast operand=(%6)                                         ; i32{ 2 }
    // ├ %19: void = .store operand=(%17) payload=(%18)                        ; void
    // ├ %20: *i32 = .get_element_pointer base=(%13) index=(usize{ 2 })        ; *i32@fff28
    // ├ %21: i32 = .cast operand=(%7)                                         ; i32{ 3 }
    // └ %22: void = .store operand=(%20) payload=(%21)                        ; void
    // local.get $foo
    //

    // try scope.function.pushInstruction(.{
    //     .i32_const = 0,
    // });

    // instruction.data.get_element_pointer.

    // switch (instruction.typed_value.type) {
    //     .simple => |simple| switch (simple) {
    //         .i8, .i16, .i32 => {
    //             try scope.function.pushInstruction(.{
    //                 .i32_const = scope.program.sema.readNumberAsType(i32, instruction.typed_value),
    //             });
    //         },
    //         .usize => {
    //             switch (scope.backend.pointer_type) {
    //                 .i32 => try scope.function.pushInstruction(.{
    //                     .i32_const = scope.program.sema.readNumberAsType(i32, instruction.typed_value),
    //                 }),
    //                 .i64 => try scope.function.pushInstruction(.{
    //                     .i64_const = scope.program.sema.readNumberAsType(i64, instruction.typed_value),
    //                 }),
    //                 else => unreachable,
    //             }
    //         },
    //         else => {
    //             std.debug.panic("unimplemented alloc type: {s}", .{@tagName(simple)});
    //         },
    //     },
    //     .complex => |complex| {
    //         const complex_type = scope.program.sema.getComplexType(complex);
    //         // const size = complex_type.size;
    //         switch (complex_type.data) {
    //             else => {
    //                 std.debug.panic("unimplemented alloc type: {s}", .{@tagName(complex_type.data)});
    //             },
    //         }
    //     },
    // }
}
