const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    switch (hir_inst) {
        .number_literal => |ast_node| {
            const slice = scope.entity.getHir().ast.getNodeSlice(ast_node.node);

            const is_float = std.mem.indexOf(u8, slice, ".") != null;

            var bytes: [8]u8 = undefined;
            if (is_float) {
                const f = try std.fmt.parseFloat(f64, slice);
                std.mem.copyForwards(u8, &bytes, std.mem.asBytes(&f));
            } else {
                const i = try std.fmt.parseInt(i64, slice, 10);
                std.mem.copyForwards(u8, &bytes, std.mem.asBytes(&i));
            }

            const value_index = try ctx.builder.internValueData(.{ .bytes = bytes });
            return ctx.pushInstruction(hir_inst_index, .{
                .op = .constant,
                .typed_value = .{
                    .type = if (is_float) Sema.Type.simple(.float) else Sema.Type.simple(.int),
                    .value = value_index,
                },
                .data = .void,
            });
        },
        .boolean_literal => |ast_node| {
            const slice = scope.entity.getHir().ast.getNodeSlice(ast_node.node);
            const is_true = std.mem.eql(u8, slice, "true");
            return ctx.pushInstruction(hir_inst_index, .{
                .op = .constant,
                .typed_value = .{
                    .type = Sema.Type.simple(.bool),
                    .value = try ctx.builder.numberAsBytesValueKey(is_true),
                },
                .data = .void,
            });
        },
        .string_literal => |ast_node| {
            var slice = scope.entity.getHir().ast.getNodeSlice(ast_node.node);
            if (slice[0] == '"') {
                slice = slice[1 .. slice.len - 1];
            }
            const ty = try ctx.builder.internTypeData(.{ .array = .{
                .child = Sema.Type.simple(.bchar),
                .len = @intCast(slice.len),
            } });

            // ctx.builder.pushDeclaration(.{
            //     .name = "str_literal",
            //     .type = ty,
            //     // .value = try ctx.builder.internVa

            // });

            const pointer = try ctx.builder.sema.memory.create(ty);
            // try ctx.builder.sema.memory.store(ty, pointer, .{ .bytes = slice });
            ctx.builder.sema.memory.storeAt([]const u8, pointer, slice);

            return ctx.pushInstruction(hir_inst_index, .{
                .op = .constant,
                .typed_value = .{
                    .type = ty,
                    .value = try ctx.builder.numberAsBytesValueKey(pointer),
                },
                // .typed_value = .{
                //     .type = try ctx.builder.internTypeData(.{ .pointer = .{
                //         .child = ty,
                //     } }),
                //     .value = try ctx.builder.numberAsBytesValueKey(pointer),
                // },
                .data = .void,
            });
        },
        .char_literal => |ast_node| {
            var slice = scope.entity.getHir().ast.getNodeSlice(ast_node.node);
            slice = slice[1 .. slice.len - 1];
            const char: u32 = std.unicode.utf8Decode(slice) catch {
                std.debug.panic("invalid char literal: {s}", .{slice});
            };
            const char_float: f64 = @floatFromInt(char);

            return ctx.pushInstruction(hir_inst_index, .{
                .op = .constant,
                .typed_value = .{
                    .type = Sema.Type.simple(.float),
                    .value = try ctx.builder.numberAsBytesValueKey(char_float),
                },
                .data = .void,
            });
        },
        else => unreachable,
    }
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    // @panic("not implemented");
    const inst = ctx.getInstruction(inst_index);
    ctx.setValue(inst_index, inst.typed_value);
}
