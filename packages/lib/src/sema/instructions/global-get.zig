const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;

const Entity = @import("../gen.zig").Entity;

pub fn genGlobalGetInstruction(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index, entity_key: Entity.Key) !Sema.Instruction.Index {
    const entity = block.ctx.builder.getEntity(entity_key);
    // try self.scope.pushDependency(entity_key);
    const global_type_key = try entity.resolveType();
    switch (entity.data) {
        .function_declaration => |fn_decl| {
            // global_entity.data.function_declaration.declaration_index,
            const val = try entity.maybeResolveDependency(entity_key);

            return try block.appendInstruction(hir_inst_index, .{
                .op = .global_get,
                .typed_value = .{
                    .type = global_type_key,
                    .value = val,
                },
                .data = .{ .global_get = .{ .declaration = fn_decl.declaration_index } },
            });
        },

        .global_type_declaration => |type_decl| {
            const global_type = block.ctx.builder.getComplexType(global_type_key);
            const global_typeof_type = block.ctx.builder.getComplexType(global_type.data.global.type);
            const global_value_key = try entity.maybeResolveDependency(entity_key);

            const typed_value = Sema.TypedValue{
                .type = global_type.data.global.type,
                .value = if (global_value_key.isComptimeKnown()) try block.ctx.builder.internValueData(.{ .type = global_typeof_type.data.typeof.child }) else global_value_key,
            };

            return try block.appendInstruction(hir_inst_index, .{
                .op = .global_get,
                .typed_value = typed_value,
                .data = .{ .global_get = .{ .declaration = type_decl.declaration_index } },
            });
        },
        .global_declaration => |global_decl| {
            const global_type = block.ctx.builder.getComplexType(global_type_key);
            const global_value_key = try entity.maybeResolveDependency(entity_key);

            // TODO: handle mutable declarations
            return try block.appendInstruction(hir_inst_index, .{
                .op = .global_get,
                .typed_value = .{
                    .type = global_type.data.global.type,
                    .value = if (global_value_key.isComptimeKnown()) block.ctx.builder.getComplexValue(global_value_key).data.global.value.value else global_value_key,
                },
                .data = .{ .global_get = .{ .declaration = global_decl.declaration_index } },
            });
        },
        else => std.debug.panic("unhandled global_entity: {s}", .{@tagName(entity.data)}),
    }
}
pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    const global_entity = block.ctx.builder.getEntityKeyByHirInst(hir_inst.global_get.operand) orelse {
        std.debug.panic("global_entity not found: {d}", .{hir_inst.global_get.operand});
    };

    return try genGlobalGetInstruction(block, hir_inst_index, global_entity);
}
const ExecContext = @import("./ExecContext.zig");
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);
    const declaration = ctx.builder.sema.declarations.items[inst.data.global_get.declaration];
    const entity = ctx.builder.getEntity(declaration.entity);
    try ctx.setValue(inst_index, .{
        .type = try entity.resolveType(),
        .value = try entity.resolveValue(),
    });
}
