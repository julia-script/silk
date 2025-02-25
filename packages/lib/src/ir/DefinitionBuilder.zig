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
def_ref: Definition.Ref,
dfg: *Dfg,

// kind: Kind,
active_block: Block.Ref = Dfg.EntryBlock,
unused_inst_vals: Set(Module.InstData.Ref) = .{},
initiators_block_map: std.AutoHashMapUnmanaged(Module.InstData.Ref, Block.Ref) = .{},

// pub const Kind = union(enum) {
//     function_body,
//     inline_expression,
//     global_body,
// };

const Self = @This();
pub fn init(module: *Module, def_ref: Definition.Ref, dfg: *Dfg) !Self {
    var self: Self = .{
        .module = module,
        .def_ref = def_ref,
        .dfg = dfg,
    };
    if (dfg.is_inline) {
        const block_call = try self.makeBlockCall();
        const block_arg = try self.makeBlock(block_call, null);
        try self.setBlockCallTarget(block_call, block_arg, null);
    }
    return self;
}

pub inline fn getDfg(self: *Self) *Dfg {
    return self.dfg;
    // return &self.getDefinition().dfg;
}

pub fn setResult(self: *Self, tyv: TypedValue) void {
    var dfg = self.getDfg();
    dfg.result = tyv;
}

pub fn declareLocal(self: *Self, ty: Ty, is_param: bool, is_comptime: bool) !Dfg.Local.Ref {
    const index = self.getDfg().local_values.count();

    return try self.getDfg().pushLocal(.{
        .value = TypedValue.Runtime(ty),
        .is_param = is_param,
        .index = @intCast(index),
        .is_comptime = is_comptime,
    });
}
pub fn useParam(self: *Self, index: u32) TypedValue {
    const local = self.getDfg().local_values.get(.{ .ref = index });
    debug.assertPrint(local.is_param, "Local {d} is not a param", .{index});
    return TypedValue.Local(local.value.ty, .{ .ref = index });
}

pub fn useLocal(self: *Self, ref: Dfg.Local.Ref) TypedValue {
    const local = self.getDfg().local_values.get(ref);

    return TypedValue.Local(local.value.ty, ref);
    // return .{
    //     .local = .{
    //         .index = local.index,
    //         .ty = local.value.getTy(),
    //         .is_comptime = local.is_comptime,
    //         .data = local.value.getData(),
    //     },
    // };
}
pub fn useGlobal(self: *Self, ref: Module.Decl.Ref) !TypedValue {
    const decl = self.module.getDeclaration(ref);

    switch (decl.*) {
        .func => {
            const ty = try self.module.declareTy(.{
                .func = .{
                    .signature = decl.func.signature,
                    .declaration = ref,
                },
            });
            return TypedValue.Global(ty, ref);
        },
        .global => {
            return TypedValue.Global(decl.global.ty, ref);
        },
    }
}

pub fn consumeValue(self: *Self, val: TypedValue) !void {
    switch (val.value) {
        .inst => |ref| {
            debug.assertPrint(
                !self.unused_inst_vals.contains(ref),
                "You're trying to consume '{}', but it either hasn't been defined or has already been consumed",
                .{val},
            );

            _ = self.unused_inst_vals.remove(ref);
        },
        .local => |ref| {
            var dfg = self.getDfg();
            var block = dfg.getBlock(self.active_block);
            try block.dependencies.append(dfg.arena.allocator(), ref);
        },
        .global => |ref| {
            // const decl = self.mod.getDeclaration(ref);
            const block = self.getBlock(self.active_block);

            if (block.is_comptime or val.isType()) {
                std.debug.print("push def dep {}", .{val});
                const def_ref = self.module.definition_map.get(ref) orelse std.debug.panic("Could not find definition for {}", .{ref});
                try self.getDfg().setDependency(.{ .definition = def_ref });
            } else {
                std.debug.print("push decl dep {}", .{val});
                try self.getDfg().setDependency(.{ .declaration = ref });
            }
            // if (self.module.getDeclaration(ref)) |decl| {
            //     if (decl.* == .func) {
            //         _ = self.unused_inst_vals.remove(ref);
            //     }
            // }
        },
        else => {},
    }
}

pub fn makeBlock(self: *Self, initiator: InstData.Ref, maybe_is_comptime: ?bool) !Block.Ref {
    var is_comptime = maybe_is_comptime orelse false;
    const initiator_block = self.initiators_block_map.get(initiator);
    if (initiator_block) |block_ref| {
        const block = self.getBlock(block_ref);
        is_comptime = is_comptime or block.is_comptime;
    }
    const ref = try self.getDfg().makeBlock(initiator, is_comptime);
    self.active_block = ref;
    return ref;
}
pub fn switchToBlock(self: *Self, block: Block.Ref) void {
    self.active_block = block;
}
pub fn linkToDecl(self: *Self, decl_ref: Module.Decl.Ref) !void {
    //     _ = self; // autofix
    //     _ = decl_ref; // autofix
    //     // switch (self.decl_ref) {
    //     // .function_body => |decl_ref| {
    //     // self.getDefinition().kind = .function_body;

    try self.module.definition_map.put(self.module.arena.allocator(), decl_ref, self.def_ref);
    //     // return self.ref;
    //     //     // },
    //     //     .global_body => {
    //     //         self.getDefinition().kind = .global_body;
    //     //         return self.ref;
    //     //     },
    //     //     .inline_expression => {
    //     //         self.getDefinition().kind = .inline_expression;
    //     //         return self.ref;
    //     //     },
    //     // }
}
// pub fn commit(self: *Self) !void {
//     const dfg = self.getDfg();
//     if (self.dfg.is_inline) {
//         const curr_block = self.getBlock(self.active_block);
//         const last_inst = curr_block.instructions.getLastOrNull() orelse std.debug.panic("Inline blocks must contain at least one instruction", .{});
//         const last_inst_value = self.
//         const block_call_ref:InstData.Ref = .{ .idx = 0};
//         try self.breakTo(block_call_ref, );

//     }
// }
pub fn breakInline(self: *Self, tyv: TypedValue) !InstData.Ref {
    debug.assert(self.dfg.is_inline, "Not an inline block");

    // const curr_block = self.getBlock(self.active_block);
    // const last_inst = curr_block.instructions.getLastOrNull() orelse std.debug.panic("Inline blocks must contain at least one instruction", .{});
    // const last_inst_value = self.
    const block_call_ref: InstData.Ref = .{ .idx = 0 };
    return try self.breakTo(block_call_ref, tyv);
}

pub fn storeGlobal(self: *Self, global: Module.Decl.Ref, value: TypedValue) !void {
    try self.consumeValue(value);
    return self.getDfg().pushStoreGlobal(self.active_block, global, value);
}
pub fn store(self: *Self, local: Dfg.Local.Ref, value: TypedValue) !void {
    try self.consumeValue(value);
    return self.getDfg().pushStore(self.active_block, local, value);
}

inline fn arithmetic(
    self: *Self,
    ty: Ty,
    comptime op: Op,
    // comptime iop: Op,
    // comptime fop: Op,
    a: TypedValue,
    b: TypedValue,
) !TypedValue {
    try self.consumeValue(a);
    try self.consumeValue(b);
    // if (a.ty.isUntypedNumber() or b.ty.isUntypedNumber()) {
    //     return try self.getDfg().pushBinary(self.active_block, ty, op, a, b);
    // }
    // if (ty.isInt()) {
    //     return try self.getDfg().pushBinary(self.active_block, ty, iop, a, b);
    // }
    // if (ty.isFloat()) {
    //     return try self.getDfg().pushBinary(self.active_block, ty, fop, a, b);
    // }
    return try self.getDfg().pushBinary(self.active_block, ty, op, a, b);
}
pub fn add(self: *Self, ty: Ty, a: TypedValue, b: TypedValue) !TypedValue {
    return try arithmetic(self, ty, .add, a, b);
}
pub fn sub(self: *Self, ty: Ty, a: TypedValue, b: TypedValue) !TypedValue {
    return try arithmetic(self, ty, .sub, a, b);
}
pub fn mul(self: *Self, ty: Ty, a: TypedValue, b: TypedValue) !TypedValue {
    return try arithmetic(self, ty, .mul, a, b);
}
pub fn div(self: *Self, ty: Ty, a: TypedValue, b: TypedValue) !TypedValue {
    return try arithmetic(self, ty, .div, a, b);
}

pub fn getBlock(self: *Self, ref: Block.Ref) Block {
    return self.getDfg().blocks.get(ref);
}
/// Comparison instructions
///
/// eq and ne are simpler

// pub fn typeof(self: *Self, value: Value, is_comptime: bool) !Ty {
//     self.consumeValue(value);
//     const val = try self.dfg.pushOperand(self.active_block, .typeof, value, is_comptime);
//     _ = val; // autofix
//     // return Ty {.ref = }
// }
pub fn cast(self: *Self, ty: Ty, value: TypedValue) !TypedValue {
    try self.consumeValue(value);
    return try self.getDfg().pushCast(self.active_block, ty, value);
}
pub fn propertyByName(self: *Self, tyv: TypedValue, name: []const u8, property_ty: Module.Ty) !TypedValue {
    try self.consumeValue(tyv);
    return try self.getDfg().pushPropertyByName(self.active_block, tyv, name, property_ty);
}
pub fn builtinPropertyAccess(self: *Self, tyv: TypedValue, name: []const u8) !TypedValue {
    try self.consumeValue(tyv);
    return try self.getDfg().pushBuiltinPropertyAccess(self.active_block, tyv, name, .unresolved);
}

pub fn propertyByIndex(self: *Self, tyv: TypedValue, index: TypedValue, property_ty: Module.Ty) !TypedValue {
    try self.consumeValue(tyv);
    try self.consumeValue(index);
    return try self.getDfg().pushPropertyByIndex(self.active_block, tyv, index, property_ty);
}
pub fn eq(self: *Self, ty: Ty, a: TypedValue, b: TypedValue) !TypedValue {
    _ = ty; // autofix
    try self.consumeValue(a);
    try self.consumeValue(b);
    return try self.getDfg().pushBinary(self.active_block, .bool, .eq, a, b);
}

pub fn ne(self: *Self, ty: Ty, a: TypedValue, b: TypedValue) !TypedValue {
    _ = ty; // autofix
    try self.consumeValue(a);
    try self.consumeValue(b);
    return try self.getDfg().pushBinary(self.active_block, .bool, .ne, a, b);
}
inline fn comparison(
    self: *Self,
    ty: Ty,
    comptime op: Op,
    // comptime op_u: Op,
    // comptime op_s: Op,
    a: TypedValue,
    b: TypedValue,
) !TypedValue {
    _ = ty; // autofix
    try self.consumeValue(a);
    try self.consumeValue(b);
    // if (ty.isUntypedNumber() or !ty.isResolvedNumeric() or ty.isFloat()) {
    //     return try self.getDfg().pushBinary(self.active_block, .bool, op, a, b);
    // }
    // if (ty.isSigned()) {
    //     return try self.getDfg().pushBinary(self.active_block, .bool, op_s, a, b);
    // }
    return try self.getDfg().pushBinary(self.active_block, .bool, op, a, b);
}

pub fn lt(self: *Self, ty: Ty, a: TypedValue, b: TypedValue) !TypedValue {
    return try comparison(self, ty, .lt, a, b);
}

pub fn le(self: *Self, ty: Ty, a: TypedValue, b: TypedValue) !TypedValue {
    return try comparison(self, ty, .le, a, b);
}

pub fn gt(self: *Self, ty: Ty, a: TypedValue, b: TypedValue) !TypedValue {
    return try comparison(self, ty, .gt, a, b);
}

pub fn ge(self: *Self, ty: Ty, a: TypedValue, b: TypedValue) !TypedValue {
    return try comparison(self, ty, .ge, a, b);
}

pub fn makeBranch(self: *Self, cond: TypedValue) !InstData.Ref {
    try self.consumeValue(cond);
    const ref = try self.getDfg().pushBranch(self.active_block, cond, [3]?Block.Ref{ null, null, null });
    try self.initiators_block_map.put(self.dfg.arena.allocator(), ref, self.active_block);
    return ref;
}
pub fn call(self: *Self, callee: TypedValue, args: []TypedValue) !TypedValue {
    try self.consumeValue(callee);
    for (args) |arg| {
        try self.consumeValue(arg);
    }
    return try self.getDfg().pushCall(self.module, self.active_block, callee, args);
}
pub fn makeBlockCall(self: *Self) !InstData.Ref {
    return try self.getDfg().pushBlockCall(self.active_block, undefined);
}
pub fn setBlockCallTarget(self: *Self, inst_ref: InstData.Ref, block: Block.Ref, finally: ?Block.Ref) !void {
    const call_inst = self.getDfg().instructions.getPtr(inst_ref);
    call_inst.block_call.args[0] = block;
    call_inst.block_call.args[1] = finally;
}
pub fn blockCall(self: *Self, block: Block.Ref) !TypedValue {
    const ref = try self.getDfg().pushBlockCall(self.active_block, block);
    try self.initiators_block_map.put(self.dfg.arena.allocator(), ref, block);
    return ref;
}
pub fn initArray(self: *Self, ty: Module.Ty, items: []TypedValue) !TypedValue {
    for (items) |item| {
        try self.consumeValue(item);
    }
    return try self.getDfg().pushInitArray(self.active_block, ty, items);
}
pub fn initStruct(self: *Self, ty: Module.Ty, keys: []const []const u8, values: []const TypedValue) !TypedValue {
    for (values) |value| {
        try self.consumeValue(value);
    }
    return try self.getDfg().pushInitStruct(self.active_block, ty, keys, values);
}

pub fn setBranchTarget(
    self: *Self,
    branch: InstData.Ref,
    then_block: ?Block.Ref,
    else_block: ?Block.Ref,
    finally_block: ?Block.Ref,
) !void {
    const branch_inst = self.getDfg().instructions.getPtr(branch);
    debug.assertPrint(then_block == null or self.getBlock(then_block.?).initiator.?.idx == branch.idx, "Then target {?} wasn't created from branch", .{then_block});
    debug.assertPrint(else_block == null or self.getBlock(else_block.?).initiator.?.idx == branch.idx, "Else target {?} wasn't created from branch", .{else_block});
    debug.assertPrint(finally_block == null or self.getBlock(finally_block.?).initiator.?.idx == branch.idx, "Finally target {?} wasn't created from branch", .{finally_block});
    branch_inst.branch.args[0] = then_block;
    branch_inst.branch.args[1] = else_block;
    branch_inst.branch.args[2] = finally_block;
}

pub fn makeLoop(self: *Self) !InstData.Ref {
    const ref = try self.getDfg().pushLoop(self.active_block, [2]?Block.Ref{ null, null });
    try self.initiators_block_map.put(self.dfg.arena.allocator(), ref, self.active_block);
    return ref;
}
pub fn breakTo(self: *Self, target: InstData.Ref, value: ?TypedValue) !InstData.Ref {
    return try self.getDfg().pushBreak(self.active_block, target, value);
}

pub fn setLoopTarget(self: *Self, loop: InstData.Ref, body: ?Block.Ref, finally: ?Block.Ref) !void {
    const loop_inst = self.getDfg().instructions.getPtr(loop);
    debug.assertPrint(body == null or self.getBlock(body.?).initiator.?.idx == loop.idx, "Body {?} wasn't created from loop", .{body});
    debug.assertPrint(finally == null or self.getBlock(finally.?).initiator.?.idx == loop.idx, "Finally {?} wasn't created from loop", .{finally});

    loop_inst.loop.args[0] = body;
    loop_inst.loop.args[1] = finally;
}

pub fn ret(self: *Self, value: ?TypedValue) !void {
    const val = value orelse TypedValue.Void;
    try self.consumeValue(val);

    try self.getDfg().pushReturn(self.active_block, val);
}

pub fn isBlockEmpty(self: *Self, block: Block.Ref) bool {
    return self.getBlock(block).isEmpty();
}
pub fn isComptime(self: *Self) bool {
    return self.getBlock(self.active_block).is_comptime;
}
