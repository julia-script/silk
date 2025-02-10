const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);

    _ = hir_inst; // autofix

    const entity_key = try block.ctx.builder.makeEntity(.{
        .parent = block.ctx.entity_key,
        .hir_inst_index = hir_inst_index,
        .name = try block.ctx.builder.internSlice("mod"),
        .data = .{
            .module_declaration = .{},
        },
    });

    const entity = block.ctx.builder.getEntity(entity_key);
    const ty = try entity.resolveType();
    const value = try entity.resolveValue();

    return try block.appendInstruction(hir_inst_index, .{
        .op = .type,
        .typed_value = .{
            .type = ty,
            .value = value,
        },
        .data = .void,
    });
}

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    _ = hir_inst; // autofix

    // hir_inst.struct_decl.fields_list;
    const entity_key = try scope.builder.makeEntity(.{
        .parent = scope.entity.key,
        .hir_inst_index = hir_inst_index,
        .name = try scope.builder.internSlice("mod"),
        .data = .{
            .module_declaration = .{},
        },
    });
    var entity = scope.builder.getEntity(entity_key);

    const ty = try entity.resolveType();
    const value = try entity.resolveValue();
    return ctx.pushInstruction(hir_inst_index, .{
        .op = .type,
        .typed_value = .{
            .type = ty,
            .value = value,
        },
        .data = .void,
    });
}
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) !void {
    _ = ctx; // autofix
    _ = inst_index; // autofix
}
