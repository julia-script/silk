const Hir = @import("../../hir.zig");
const Sema = @import("../Sema.zig");
const std = @import("std");
const InstContext = @import("./InstContext.zig");
const GenScope = @import("../gen.zig").Scope;
pub fn emit(block: *InstContext.Block, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = block.ctx.getHirInstruction(hir_inst_index);
    const param_entity = block.ctx.builder.getEntityByHirInst(hir_inst.param.operand);

    const param_index = param_entity.data.parameter_declaration.index;
    return block.appendInstruction(hir_inst_index, .{
        .op = .param,
        .typed_value = .{
            .type = try param_entity.resolveType(),
            .value = Sema.Value.simple(.exec_time),
        },
        .data = .{ .param = .{ .index = param_index, .name = param_entity.name } },
    });
}

pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) !Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const param_entity = scope.builder.getEntityByHirInst(hir_inst.param.operand);

    const param_index = param_entity.data.parameter_declaration.index;
    return ctx.pushInstruction(hir_inst_index, .{
        .op = .param,
        .typed_value = .{
            .type = try param_entity.resolveType(),
            .value = ctx.getParamValue(@intCast(param_index)),
        },
        .data = .{ .param = .{ .index = param_index, .name = param_entity.name } },
    });
}
const ExecContext = @import("./ExecContext.zig");
pub fn exec(ctx: *ExecContext, inst_index: Sema.Instruction.Index) !void {
    const inst = ctx.getInstruction(inst_index);

    try ctx.setValue(inst_index, .{
        .type = inst.typed_value.type,
        .value = ctx.getParamValue(@intCast(inst.data.param.index)),
    });
}
