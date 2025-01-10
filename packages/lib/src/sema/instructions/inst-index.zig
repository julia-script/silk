pub const Block = @import("./control-block.zig");
pub const Loop = @import("./control-loop.zig");
pub const Break = @import("./control-break.zig");
pub const Return = @import("./control-return.zig");
pub const If = @import("./control-if.zig");

pub const FnCall = @import("./fncall.zig");

pub const Literal = @import("./literal.zig");
pub const TypeLiteral = @import("./type-literal.zig");
pub const TypeOf = @import("./typeof.zig");
pub const Alloc = @import("./alloc.zig");
pub const Store = @import("./store.zig");
pub const Load = @import("./load.zig");
pub const Arithmetic = @import("./arithmetic.zig");
pub const InstIndex = @import("./inst-index.zig");
pub const As = @import("./as.zig");
pub const Comparison = @import("./comparison.zig");
pub const GlobalGet = @import("./global-get.zig");
pub const ParamGet = @import("./param-get.zig");
pub const ParamSet = @import("./param-set.zig");
pub const Param = @import("./param.zig");
pub const TypeInit = @import("./type-init.zig");
pub const FieldInit = @import("./field-init.zig");
pub const GetElementPtr = @import("./get-element-ptr.zig");
pub const StructDecl = @import("./struct-decl.zig");
pub const ArrayInit = @import("./array-init.zig");
pub const Constant = @import("./constant.zig");
pub const Reinterpret = @import("./reinterpret.zig");
const InstContext = @import("./InstContext.zig");
const Sema = @import("../sema.zig");
const Error = @import("../gen.zig").Error;
const std = @import("std");
pub fn exec(ctx: *InstContext, inst_index: Sema.Instruction.Index) Error!void {
    const instruction = ctx.getInstruction(inst_index);

    const stderr = std.io.getStdErr().writer();
    const indent = ctx.indent;
    if (indent > 1000) {
        std.debug.panic("indent too deep: {d}", .{indent});
    }
    ctx.indent += 1;
    try stderr.writeBytesNTimes("  ", indent);
    if (instruction.liveness == 0) {
        try stderr.print("<exec.{s}(%{d}){s} dead />\n", .{ @tagName(instruction.op), inst_index, if (ctx.is_comptime) " comptime" else "" });
        ctx.indent = indent;
        return;
    }
    try stderr.print("<exec.{s}(%{d}){s}>\n", .{ @tagName(instruction.op), inst_index, if (ctx.is_comptime) " comptime" else "" });
    switch (instruction.op) {
        .constant => try Constant.exec(ctx, inst_index),
        .block => try Block.exec(ctx, inst_index),
        .add, .sub, .mul, .div => try Arithmetic.exec(ctx, inst_index),
        .store => try Store.exec(ctx, inst_index),
        .load => try Load.exec(ctx, inst_index),
        .alloc => try Alloc.exec(ctx, inst_index),
        .typeof => try TypeOf.exec(ctx, inst_index),
        .type => try TypeLiteral.exec(ctx, inst_index),
        .cast => try As.exec(ctx, inst_index),
        .loop => try Loop.exec(ctx, inst_index),
        .eq, .ne, .lt, .le, .gt, .ge => try Comparison.exec(ctx, inst_index),
        .br => try Break.exec(ctx, inst_index),
        .ret => try Return.exec(ctx, inst_index),
        .global_get => try GlobalGet.exec(ctx, inst_index),
        .fn_call => try FnCall.exec(ctx, inst_index),
        .param => try Param.exec(ctx, inst_index),
        .param_set => try ParamSet.exec(ctx, inst_index),
        .param_get => try ParamGet.exec(ctx, inst_index),
        .@"if" => try If.exec(ctx, inst_index),
        .type_init => try TypeInit.exec(ctx, inst_index),
        .field_init => try FieldInit.exec(ctx, inst_index),
        .get_element_pointer => try GetElementPtr.exec(ctx, inst_index),
        .reinterpret => try Reinterpret.exec(ctx, inst_index),

        else => |op| {
            std.debug.panic("unhandled op: {s}", .{@tagName(op)});
        },
    }
    try stderr.writeBytesNTimes("  ", indent);
    try stderr.print("</exec.{s}(%{d})>\n", .{ @tagName(instruction.op), inst_index });
    ctx.indent = indent;
}
const GenScope = @import("../gen.zig").Scope;
const Hir = @import("../../hir.zig");
pub fn gen(ctx: *InstContext, scope: *GenScope, hir_inst_index: Hir.Inst.Index) Error!Sema.Instruction.Index {
    const hir_inst = scope.entity.getHirInstruction(hir_inst_index);
    const stderr = std.io.getStdErr().writer();
    const indent = ctx.indent;
    ctx.indent += 1;
    try stderr.writeBytesNTimes("  ", indent);
    try stderr.print("<gen.{s}{s}(%{d})>\n", .{ @tagName(hir_inst), if (ctx.is_comptime) " comptime" else "", hir_inst_index });
    const index = switch (hir_inst) {
        .block, .inline_block => try Block.gen(ctx, scope, hir_inst_index),
        .number_literal,
        .char_literal,
        .boolean_literal,
        .string_literal,
        => try Literal.gen(ctx, scope, hir_inst_index),
        .typeof => try TypeOf.gen(ctx, scope, hir_inst_index),
        .alloc => try Alloc.gen(ctx, scope, hir_inst_index),
        .store => try Store.gen(ctx, scope, hir_inst_index),
        .load => try Load.gen(ctx, scope, hir_inst_index),
        .add, .sub, .mul, .div => try Arithmetic.gen(ctx, scope, hir_inst_index),
        .as => try As.gen(ctx, scope, hir_inst_index),
        .loop => try Loop.gen(ctx, scope, hir_inst_index),
        .eq, .ne, .lt, .le, .gt, .ge => try Comparison.gen(ctx, scope, hir_inst_index),
        .br => try Break.gen(ctx, scope, hir_inst_index),
        .ret => try Return.gen(ctx, scope, hir_inst_index),
        .global_get => try GlobalGet.gen(ctx, scope, hir_inst_index),
        .fn_call => try FnCall.gen(ctx, scope, hir_inst_index),
        .if_expr => try If.gen(ctx, scope, hir_inst_index),
        .param => try Param.gen(ctx, scope, hir_inst_index),
        .param_get => try ParamGet.gen(ctx, scope, hir_inst_index),
        .param_set => try ParamSet.gen(ctx, scope, hir_inst_index),
        .type_init => try TypeInit.gen(ctx, scope, hir_inst_index),
        .get_element_pointer => try GetElementPtr.gen(ctx, scope, hir_inst_index),
        .get_property_pointer => try GetElementPtr.gen(ctx, scope, hir_inst_index),
        .struct_decl => try StructDecl.gen(ctx, scope, hir_inst_index),
        .array_init => try ArrayInit.gen(ctx, scope, hir_inst_index),
        .field_init => try FieldInit.gen(ctx, scope, hir_inst_index),
        .ty_number,
        .ty_boolean,
        .ty_i8,
        .ty_i16,
        .ty_i32,
        .ty_i64,
        .ty_u8,
        .ty_u16,
        .ty_u32,
        .ty_u64,
        .ty_usize,
        .ty_array,
        .ty_f32,
        .ty_f64,
        .ty_void,
        .ty_pointer,
        .ty_global,
        => try TypeLiteral.gen(ctx, scope, hir_inst_index),

        else => {
            std.debug.panic("unhandled hir inst: {}\n", .{hir_inst});
        },
    };

    // const inst = ctx.getInstruction(index);

    ctx.indent = indent;
    try stderr.writeBytesNTimes("  ", indent);
    try stderr.print("</gen.{s}>\n", .{@tagName(hir_inst)});
    return index;
}

test {
    @import("std").testing.refAllDecls(@This());
}
