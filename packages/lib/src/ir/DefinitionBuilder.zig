const Function = @import("./Function.zig");
const FunctionDeclaration = @import("./FunctionDeclaration.zig");
const Module = @import("./Module.zig");
const Dfg = @import("./Dfg.zig");
const Signature = @import("./Signature.zig");
const std = @import("std");
const Definition = @import("./Definition.zig");
const InstData = @import("./inst.zig").InstData;
const TypedValue = Module.TypedValue;
const Op = @import("./opcodes.zig").Op;
const Block = @import("./Block.zig");
const Ty = @import("./ty.zig").Ty;
const Set = @import("../data_structures.zig").AutoSetUnmanaged;
const debug = @import("../debug.zig");
const utils = @import("./utils.zig");

module: *Module,
decl_ref: Module.Decl.Ref,
ref: Definition.Ref,

// kind: Kind,
active_block: Block.Ref = Dfg.EntryBlock,
unused_inst_vals: Set(Module.InstData.Ref) = .{},

// pub const Kind = union(enum) {
//     function_body,
//     inline_expression,
//     global_body,
// };

const Self = @This();
pub fn init(module: *Module, decl_ref: Module.Decl.Ref) !Self {
    return .{
        .module = module,
        .ref = try module.makeDefinition(),
        .decl_ref = decl_ref,
        // .unused_inst_vals = Set(Module.InstData.Ref).init(allocator),
    };
}
pub fn getDefinition(self: *Self) *Definition {
    return self.module.definitions.getPtr(self.ref);
}
pub fn getDfg(self: *Self) *Dfg {
    return &self.getDefinition().dfg;
}
pub fn declareLocal(self: *Self, ty: Ty, is_comptime: bool) !Dfg.Local.Ref {
    return try self.getDfg().pushLocal(ty, false, is_comptime);
}
pub fn useParam(self: *Self, index: u32) TypedValue {
    const local = self.getDfg().local_values.get(.{ .ref = index });
    debug.assertPrint(local.is_param, "Local {d} is not a param", .{index});
    return TypedValue.Local(local.value.ty, .{ .ref = index }, local.is_comptime);
}

pub fn useLocal(self: *Self, ref: Dfg.Local.Ref) TypedValue {
    const local = self.getDfg().local_values.get(ref);

    return TypedValue.Local(local.value.ty, ref, local.is_comptime);
    // return .{
    //     .local = .{
    //         .index = local.index,
    //         .ty = local.value.getTy(),
    //         .is_comptime = local.is_comptime,
    //         .data = local.value.getData(),
    //     },
    // };
}
pub fn useGlobal(self: *Self, module: *Module, ref: Module.Decl.Ref, is_comptime: bool) !TypedValue {
    const decl = module.getDeclaration(ref);
    if (is_comptime) {
        try self.getDfg().setDependency(ref, .definition);
    } else {
        try self.getDfg().setDependency(ref, .declaration);
    }

    switch (decl.*) {
        .func => {
            const ty = try module.declareTy(.{
                .func = .{
                    .signature = decl.func.signature,
                    .declaration = ref,
                },
            });

            return TypedValue.Global(ty, ref, is_comptime);
        },
        .global => {
            return decl.global.value;
        },
    }
}

pub fn consumeValue(self: *Self, val: TypedValue) void {
    switch (val.value) {
        .inst => |ref| {
            debug.assertPrint(
                !self.unused_inst_vals.contains(ref),
                "You're trying to consume '{}', but it either hasn't been defined or has already been consumed",
                .{val},
            );
            _ = self.unused_inst_vals.remove(ref);
        },
        else => {},
    }
}

pub fn makeBlock(self: *Self, initiator: InstData.Ref) !Block.Ref {
    const ref = try self.getDfg().makeBlock(initiator);
    self.active_block = ref;
    return ref;
}
pub fn switchToBlock(self: *Self, block: Block.Ref) void {
    self.active_block = block;
}
pub fn commit(self: *Self, decl_ref: Module.Decl.Ref) !void {
    // switch (self.decl_ref) {
    // .function_body => |decl_ref| {
    // self.getDefinition().kind = .function_body;

    try self.module.definition_map.put(self.module.arena.allocator(), decl_ref, self.ref);
    // return self.ref;
    //     // },
    //     .global_body => {
    //         self.getDefinition().kind = .global_body;
    //         return self.ref;
    //     },
    //     .inline_expression => {
    //         self.getDefinition().kind = .inline_expression;
    //         return self.ref;
    //     },
    // }
}

pub fn storeGlobal(self: *Self, global: Module.Decl.Ref, value: TypedValue) !void {
    return self.getDfg().pushStoreGlobal(self.active_block, global, value);
}
pub fn store(self: *Self, local: Dfg.Local.Ref, value: TypedValue) !void {
    self.consumeValue(value);
    return self.getDfg().pushStore(self.active_block, local, value);
}

inline fn arithmetic(
    self: *Self,
    ty: Ty,
    comptime op: Op,
    comptime iop: Op,
    comptime fop: Op,
    a: TypedValue,
    b: TypedValue,
    is_comptime: bool,
) !TypedValue {
    self.consumeValue(a);
    self.consumeValue(b);
    if (ty.isInt()) {
        return try self.getDfg().pushBinary(self.active_block, ty, iop, a, b, is_comptime);
    }
    if (ty.isFloat()) {
        return try self.getDfg().pushBinary(self.active_block, ty, fop, a, b, is_comptime);
    }
    return try self.getDfg().pushBinary(self.active_block, ty, op, a, b, is_comptime);
}
pub fn add(self: *Self, ty: Ty, a: TypedValue, b: TypedValue, is_comptime: bool) !TypedValue {
    return try arithmetic(self, ty, .add, .iadd, .fadd, a, b, is_comptime);
}
pub fn sub(self: *Self, ty: Ty, a: TypedValue, b: TypedValue, is_comptime: bool) !TypedValue {
    return try arithmetic(self, ty, .sub, .isub, .fsub, a, b, is_comptime);
}
pub fn mul(self: *Self, ty: Ty, a: TypedValue, b: TypedValue, is_comptime: bool) !TypedValue {
    return try arithmetic(self, ty, .mul, .imul, .fmul, a, b, is_comptime);
}

pub fn getBlock(self: *Self, ref: Block.Ref) Block {
    return self.getDfg().blocks.get(ref);
}
/// Comparison instructions
///
/// eq and ne are simpler
pub fn eq(self: *Self, ty: Ty, a: TypedValue, b: TypedValue, is_comptime: bool) !TypedValue {
    self.consumeValue(a);
    self.consumeValue(b);
    return try self.getDfg().pushBinary(self.active_block, ty, .eq, a, b, is_comptime);
}

pub fn ne(self: *Self, ty: Ty, a: TypedValue, b: TypedValue, is_comptime: bool) !TypedValue {
    self.consumeValue(a);
    self.consumeValue(b);
    return try self.getDfg().pushBinary(self.active_block, ty, .ne, a, b, is_comptime);
}
// pub fn typeof(self: *Self, value: Value, is_comptime: bool) !Ty {
//     self.consumeValue(value);
//     const val = try self.dfg.pushOperand(self.active_block, .typeof, value, is_comptime);
//     _ = val; // autofix
//     // return Ty {.ref = }
// }
pub fn cast(self: *Self, ty: Ty, value: TypedValue, is_comptime: bool) !TypedValue {
    self.consumeValue(value);
    return try self.getDfg().pushOperand(self.active_block, .cast, ty, value, is_comptime);
}
inline fn comparison(
    self: *Self,
    ty: Ty,
    comptime op: Op,
    comptime op_u: Op,
    comptime op_s: Op,
    a: TypedValue,
    b: TypedValue,
    is_comptime: bool,
) !TypedValue {
    self.consumeValue(a);
    self.consumeValue(b);
    if (ty.isUntypedNumber() or !ty.isResolvedNumeric() or ty.isFloat()) {
        return try self.getDfg().pushBinary(self.active_block, .bool, op, a, b, is_comptime);
    }
    if (ty.isSigned()) {
        return try self.getDfg().pushBinary(self.active_block, .bool, op_s, a, b, is_comptime);
    }
    return try self.getDfg().pushBinary(self.active_block, .bool, op_u, a, b, is_comptime);
}

pub fn lt(self: *Self, ty: Ty, a: TypedValue, b: TypedValue, is_comptime: bool) !TypedValue {
    return try comparison(self, ty, .lt, .lt_u, .lt_s, a, b, is_comptime);
}

pub fn le(self: *Self, ty: Ty, a: TypedValue, b: TypedValue, is_comptime: bool) !TypedValue {
    return try comparison(self, ty, .le, .le_u, .le_s, a, b, is_comptime);
}

pub fn gt(self: *Self, ty: Ty, a: TypedValue, b: TypedValue, is_comptime: bool) !TypedValue {
    return try comparison(self, ty, .gt, .gt_u, .gt_s, a, b, is_comptime);
}

pub fn ge(self: *Self, ty: Ty, a: TypedValue, b: TypedValue, is_comptime: bool) !TypedValue {
    return try comparison(self, ty, .ge, .ge_u, .ge_s, a, b, is_comptime);
}

pub fn makeBranch(self: *Self, cond: TypedValue) !InstData.Ref {
    return try self.getDfg().pushBranch(self.active_block, cond, [3]?Block.Ref{ null, null, null });
}
pub fn call(self: *Self, callee: TypedValue, args: []TypedValue, is_comptime: bool) !TypedValue {
    return try self.getDfg().pushCall(self.module, self.active_block, callee, args, is_comptime);
}
pub fn initArray(self: *Self, ty: Module.Ty, items: []TypedValue, is_comptime: bool) !TypedValue {
    return try self.getDfg().pushInitArray(self.active_block, ty, items, is_comptime);
}
pub fn initStruct(self: *Self, ty: Module.Ty, keys: []const []const u8, values: []const TypedValue, is_comptime: bool) !TypedValue {
    return try self.getDfg().pushInitStruct(self.active_block, ty, keys, values, is_comptime);
}

pub fn setBranchTarget(
    self: *Self,
    branch: InstData.Ref,
    then_block: ?Block.Ref,
    else_block: ?Block.Ref,
    finally_block: ?Block.Ref,
) !void {
    const branch_inst = self.getDfg().instructions.getPtr(branch);
    debug.assertPrint(then_block == null or self.getBlock(then_block.?).initiator.?.ref == branch.ref, "Then target {?} wasn't created from branch", .{then_block});
    debug.assertPrint(else_block == null or self.getBlock(else_block.?).initiator.?.ref == branch.ref, "Else target {?} wasn't created from branch", .{else_block});
    debug.assertPrint(finally_block == null or self.getBlock(finally_block.?).initiator.?.ref == branch.ref, "Finally target {?} wasn't created from branch", .{finally_block});
    branch_inst.branch.args[0] = then_block;
    branch_inst.branch.args[1] = else_block;
    branch_inst.branch.args[2] = finally_block;
}

pub fn makeLoop(self: *Self) !InstData.Ref {
    return try self.getDfg().pushLoop(self.active_block, [2]?Block.Ref{ null, null });
}
pub fn breakTo(self: *Self, target: InstData.Ref, value: ?TypedValue, is_comptime: bool) !InstData.Ref {
    return try self.getDfg().pushBreak(self.active_block, target, value, is_comptime);
}

pub fn setLoopTarget(self: *Self, loop: InstData.Ref, body: Block.Ref, finally: ?Block.Ref) !void {
    const loop_inst = self.getDfg().instructions.getPtr(loop);
    debug.assertPrint(self.getBlock(body).initiator.?.ref == loop.ref, "Body {?} wasn't created from loop", .{body});
    debug.assertPrint(finally == null or self.getBlock(finally.?).initiator.?.ref == loop.ref, "Finally {?} wasn't created from loop", .{finally});

    loop_inst.loop.args[0] = body;
    loop_inst.loop.args[1] = finally;
}

pub fn ret(self: *Self, value: ?TypedValue, is_comptime: bool) !void {
    const val = value orelse TypedValue.Void;
    self.consumeValue(val);

    try self.getDfg().pushReturn(self.active_block, val, is_comptime);
}
