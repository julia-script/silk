const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

// pub fn pushGlobalGetInstruction(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index, entity_key: Entity.Key) Error!Sema.Instruction.Index {
//     const entity = self.builder.getEntity(entity_key);
//     // try self.scope.pushDependency(entity_key);
//     const global_type = try entity.resolveType();
//     switch (entity.data) {
//         .function_declaration => |fn_decl| {
//             // global_entity.data.function_declaration.declaration_index,
//             return self.pushInstruction(hir_inst_index, .{
//                 .op = .global_get,
//                 .typed_value = .{
//                     .type = global_type,
//                     .value = try self.maybeResolveDependency(entity_key),
//                 },
//                 .data = .{ .declaration = fn_decl.declaration_index },
//             });
//         },
//         .global_type_declaration => |type_decl| {
//             return self.pushInstruction(hir_inst_index, .{
//                 .op = .global_get,
//                 .typed_value = .{
//                     .type = global_type,
//                     .value = try self.maybeResolveDependency(entity_key),
//                 },
//                 .data = .{ .declaration = type_decl.declaration_index },
//             });
//         },
//         else => std.debug.panic("unhandled global_entity: {s}", .{@tagName(entity.data)}),
//     }
// }
const Entity = @import("../gen.zig").Entity;
pub fn genGlobalGetInstruction(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index, entity_key: Entity.Key) !Sema.Instruction.Index {
    const entity = scope.builder.getEntity(entity_key);
    // try self.scope.pushDependency(entity_key);
    const global_type = try entity.resolveType();
    switch (entity.data) {
        .function_declaration => |fn_decl| {
            // global_entity.data.function_declaration.declaration_index,
            const val = try scope.maybeResolveDependency(entity_key);
            std.debug.print("genGlobalGetInstruction: {d} {}\n", .{ entity_key, val });
            return ctx.pushInstruction(hir_inst_index, .{
                .op = .global_get,
                .typed_value = .{
                    .type = global_type,
                    .value = val,
                },
                .data = .{ .global_get = .{ .entity = entity_key, .declaration = fn_decl.declaration_index } },
            });
        },
        .global_type_declaration => |type_decl| {
            return ctx.pushInstruction(hir_inst_index, .{
                .op = .global_get,
                .typed_value = .{
                    .type = global_type,
                    .value = try scope.maybeResolveDependency(entity_key),
                },
                .data = .{ .global_get = .{ .entity = entity_key, .declaration = type_decl.declaration_index } },
            });
        },
        else => std.debug.panic("unhandled global_entity: {s}", .{@tagName(entity.data)}),
    }
}
pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const global_entity = scope.builder.getEntityByHirInst(hir_inst.global_get.operand);

    return try genGlobalGetInstruction(ctx, scope, hir_inst_index, global_entity.key);
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const entity = ctx.builder.getEntity(inst.data.global_get.entity);
    ctx.setValue(inst_index, .{
        .type = try entity.resolveType(),
        .value = try entity.resolveValue(),
    });
}
