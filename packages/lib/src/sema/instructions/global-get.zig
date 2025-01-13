const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

const Entity = @import("../gen.zig").Entity;

pub fn genGlobalGetInstruction(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index, entity_key: Entity.Key) !Sema.Instruction.Index {
    const entity = scope.builder.getEntity(entity_key);
    // try self.scope.pushDependency(entity_key);
    const global_type_key = try entity.resolveType();
    switch (entity.data) {
        .function_declaration => |fn_decl| {
            // global_entity.data.function_declaration.declaration_index,
            const val = try scope.maybeResolveDependency(entity_key);

            return ctx.pushInstruction(hir_inst_index, .{
                .op = .global_get,
                .typed_value = .{
                    .type = global_type_key,
                    .value = val,
                },
                .data = .{ .global_get = .{ .declaration = fn_decl.declaration_index } },
            });
        },

        .global_type_declaration => |type_decl| {
            const global_type = ctx.builder.getComplexType(global_type_key);
            const global_typeof_type = ctx.builder.getComplexType(global_type.data.global.type);
            const global_value_key = try scope.maybeResolveDependency(entity_key);

            const typed_value = Sema.TypedValue{
                .type = global_type.data.global.type,
                .value = if (global_value_key.isComptimeKnown()) try ctx.builder.internValueData(.{ .type = global_typeof_type.data.typeof.child }) else global_value_key,
            };
            return ctx.pushInstruction(hir_inst_index, .{
                .op = .global_get,
                .typed_value = typed_value,
                .data = .{ .global_get = .{ .declaration = type_decl.declaration_index } },
            });
        },
        .global_declaration => |global_decl| {
            const global_type = ctx.builder.getComplexType(global_type_key);
            const global_value_key = try scope.maybeResolveDependency(entity_key);

            // TODO: handle mutable declarations
            return ctx.pushInstruction(hir_inst_index, .{
                .op = .global_get,
                .typed_value = .{
                    .type = global_type.data.global.type,
                    .value = if (global_value_key.isComptimeKnown()) ctx.builder.getComplexValue(global_value_key).data.global.value.value else global_value_key,
                },
                .data = .{ .global_get = .{ .declaration = global_decl.declaration_index } },
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
    const declaration = ctx.builder.sema.declarations.items[inst.data.global_get.declaration];
    const entity = ctx.builder.getEntity(declaration.entity);
    ctx.setValue(inst_index, .{
        .type = try entity.resolveType(),
        .value = try entity.resolveValue(),
    });
}
