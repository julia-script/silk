const Function = @import("./Function.zig");
const FunctionDeclaration = @import("./FunctionDeclaration.zig");
const Module = @import("./Module.zig");
const Dfg = @import("./Dfg.zig");
const Signature = @import("./Signature.zig");
const std = @import("std");
const Definition = @import("./Definition.zig");
const InstData = @import("./inst.zig").InstData;
const Value = @import("./val.zig").Value;
const Op = @import("./opcodes.zig").Op;
const Block = @import("./Block.zig");
const Ty = @import("./tyval.zig").Ty;
const Set = @import("../data_structures.zig").AutoSet;
const debug = @import("../debug.zig");
const utils = @import("./utils.zig");

allocator: std.mem.Allocator,
module: *Module,
dfg: Dfg,
kind: Kind,
active_block: Block.Ref = Dfg.EntryBlock,
unused_inst_vals: Set(Value),
// decls: Decl.Ref.List(Decl),
// unused_values: Set(Value, void) = Set(Value, void).init(std.testing.allocator),

pub const Kind = union(enum) {
    function_body: Module.Decl.Ref,
    inline_expression,
    global_body,
};

const Self = @This();

pub fn init(allocator: std.mem.Allocator, module: *Module, kind: Kind) !Self {
    var self = try create(allocator, module, kind);
    switch (kind) {
        .function_body => |ref| {
            const func_declaration = self.module.getFunctionDeclaration(ref);
            const sig = self.module.getSignature(func_declaration.signature);
            for (sig.params.items) |param| {
                _ = try self.dfg.pushLocal(param.ty, true, param.is_comptime);
            }
        },
        .inline_expression => {},
        .global_body => {},
    }
    return self;
}
pub fn declareLocal(self: *Self, ty: Ty, is_comptime: bool) !Dfg.Local.Ref {
    return try self.dfg.pushLocal(ty, false, is_comptime);
}
pub fn useParam(self: *Self, index: u32) Value {
    const local = self.dfg.local_values.get(.{ .ref = index });
    debug.assertPrint(local.is_param, "Local {d} is not a param", .{index});
    return local.value;
}

pub fn useLocal(self: *Self, ref: Dfg.Local.Ref) Value {
    const local = self.dfg.local_values.get(ref);
    return local.value;
    // return .{
    //     .local = .{
    //         .index = local.index,
    //         .ty = local.value.getTy(),
    //         .is_comptime = local.is_comptime,
    //         .data = local.value.getData(),
    //     },
    // };
}
pub fn useGlobal(self: *Self, module: *Module, ref: Module.Decl.Ref, is_comptime: bool) Value {
    _ = self; // autofix
    const decl = module.getDeclaration(ref);
    switch (decl.*) {
        .func => {
            const ty = .{
                .func = .{
                    .signature = decl.func.signature,
                    .declaration = ref,
                },
            };
            _ = ty; // autofix
            return .{ .global = .{
                .index = ref.ref,
                .is_comptime = is_comptime,
                .ty = .{
                    .func = .{
                        .signature = decl.func.signature,
                        .declaration = ref,
                    },
                },
                .data = .runtime,
            } };
        },
        // else => {
        //     @panic("not implemented");
        // },
    }
}

pub fn consumeValue(self: *Self, val: Value) void {
    switch (val) {
        .inst => {
            debug.assertPrint(
                !self.unused_inst_vals.contains(val),
                "You're trying to consume '{}', but it either hasn't been defined or has already been consumed",
                .{val},
            );
            _ = self.unused_inst_vals.remove(val);
        },
        else => {},
    }
}
fn create(allocator: std.mem.Allocator, module: *Module, kind: Kind) !Self {
    return .{
        .allocator = allocator,
        .module = module,
        .dfg = try Dfg.init(allocator),
        .kind = kind,
        .unused_inst_vals = Set(Value).init(allocator),
    };
}

pub fn makeBlock(self: *Self, initiator: InstData.Ref) !Block.Ref {
    const ref = try self.dfg.makeBlock(initiator);
    self.active_block = ref;
    return ref;
}
pub fn switchToBlock(self: *Self, block: Block.Ref) void {
    self.active_block = block;
}
pub fn commit(self: *Self) !Definition.Ref {
    switch (self.kind) {
        .function_body => |decl_ref| {
            const def = try self.module.definitions.append(Definition.init(.function_body, self.dfg));
            try self.module.function_definitions_map.put(decl_ref, def);
            return def;
        },
        .global_body => {
            @panic("not implemented");
            // return try self.module.definitions.append(Definition.init(self.kind, self.dfg));
        },
        .inline_expression => {
            return try self.module.definitions.append(Definition.init(.inline_expression, self.dfg));
        },
    }
}

pub fn storeGlobal(self: *Self, global: Module.Decl.Ref, value: Value) !void {
    return self.dfg.pushStoreGlobal(self.active_block, global, value);
}
pub fn store(self: *Self, local: Dfg.Local.Ref, value: Value) !void {
    self.consumeValue(value);
    return self.dfg.pushStore(self.active_block, local, value);
}

inline fn arithmetic(
    self: *Self,
    ty: Ty,
    comptime op: Op,
    comptime iop: Op,
    comptime fop: Op,
    a: Value,
    b: Value,
    is_comptime: bool,
) !Value {
    self.consumeValue(a);
    self.consumeValue(b);
    if (ty.isInt()) {
        return try self.dfg.pushBinary(self.active_block, ty, iop, a, b, is_comptime);
    }
    if (ty.isFloat()) {
        return try self.dfg.pushBinary(self.active_block, ty, fop, a, b, is_comptime);
    }
    return try self.dfg.pushBinary(self.active_block, ty, op, a, b, is_comptime);
}
pub fn add(self: *Self, ty: Ty, a: Value, b: Value, is_comptime: bool) !Value {
    return try arithmetic(self, ty, .add, .iadd, .fadd, a, b, is_comptime);
}
pub fn sub(self: *Self, ty: Ty, a: Value, b: Value, is_comptime: bool) !Value {
    return try arithmetic(self, ty, .sub, .isub, .fsub, a, b, is_comptime);
}
pub fn mul(self: *Self, ty: Ty, a: Value, b: Value, is_comptime: bool) !Value {
    return try arithmetic(self, ty, .mul, .imul, .fmul, a, b, is_comptime);
}

pub fn getBlock(self: *Self, ref: Block.Ref) Block {
    return self.dfg.blocks.get(ref);
}
/// Comparison instructions
///
/// eq and ne are simpler
pub fn eq(self: *Self, ty: Ty, a: Value, b: Value, is_comptime: bool) !Value {
    self.consumeValue(a);
    self.consumeValue(b);
    return try self.dfg.pushBinary(self.active_block, ty, .eq, a, b, is_comptime);
}

pub fn ne(self: *Self, ty: Ty, a: Value, b: Value, is_comptime: bool) !Value {
    self.consumeValue(a);
    self.consumeValue(b);
    return try self.dfg.pushBinary(self.active_block, ty, .ne, a, b, is_comptime);
}
// pub fn typeof(self: *Self, value: Value, is_comptime: bool) !Ty {
//     self.consumeValue(value);
//     const val = try self.dfg.pushOperand(self.active_block, .typeof, value, is_comptime);
//     _ = val; // autofix
//     // return Ty {.ref = }
// }
pub fn cast(self: *Self, ty: Ty, value: Value, is_comptime: bool) !Value {
    self.consumeValue(value);
    return try self.dfg.pushOperand(self.active_block, .cast, ty, value, is_comptime);
}
inline fn comparison(
    self: *Self,
    ty: Ty,
    comptime op: Op,
    comptime op_u: Op,
    comptime op_s: Op,
    a: Value,
    b: Value,
    is_comptime: bool,
) !Value {
    self.consumeValue(a);
    self.consumeValue(b);
    if (ty.isUntypedNumber() or !ty.isResolvedNumeric() or ty.isFloat()) {
        return try self.dfg.pushBinary(self.active_block, .bool, op, a, b, is_comptime);
    }
    if (ty.isSigned()) {
        return try self.dfg.pushBinary(self.active_block, .bool, op_s, a, b, is_comptime);
    }
    return try self.dfg.pushBinary(self.active_block, .bool, op_u, a, b, is_comptime);
}

pub fn lt(self: *Self, ty: Ty, a: Value, b: Value, is_comptime: bool) !Value {
    return try comparison(self, ty, .lt, .lt_u, .lt_s, a, b, is_comptime);
}

pub fn le(self: *Self, ty: Ty, a: Value, b: Value, is_comptime: bool) !Value {
    return try comparison(self, ty, .le, .le_u, .le_s, a, b, is_comptime);
}

pub fn gt(self: *Self, ty: Ty, a: Value, b: Value, is_comptime: bool) !Value {
    return try comparison(self, ty, .gt, .gt_u, .gt_s, a, b, is_comptime);
}

pub fn ge(self: *Self, ty: Ty, a: Value, b: Value, is_comptime: bool) !Value {
    return try comparison(self, ty, .ge, .ge_u, .ge_s, a, b, is_comptime);
}

pub fn makeBranch(self: *Self, cond: Value) !InstData.Ref {
    return try self.dfg.pushBranch(self.active_block, cond, [3]?Block.Ref{ null, null, null });
}
pub fn call(self: *Self, callee: Value, args: []Value) !Value {
    return try self.dfg.pushCall(self.module, self.active_block, callee, args);
}

pub fn setBranchTarget(
    self: *Self,
    branch: InstData.Ref,
    then_block: ?Block.Ref,
    else_block: ?Block.Ref,
    finally_block: ?Block.Ref,
) !void {
    const branch_inst = self.dfg.instructions.getPtr(branch);
    debug.assertPrint(then_block == null or self.getBlock(then_block.?).initiator.?.ref == branch.ref, "Then target {?} wasn't created from branch", .{then_block});
    debug.assertPrint(else_block == null or self.getBlock(else_block.?).initiator.?.ref == branch.ref, "Else target {?} wasn't created from branch", .{else_block});
    debug.assertPrint(finally_block == null or self.getBlock(finally_block.?).initiator.?.ref == branch.ref, "Finally target {?} wasn't created from branch", .{finally_block});
    branch_inst.branch.args[0] = then_block;
    branch_inst.branch.args[1] = else_block;
    branch_inst.branch.args[2] = finally_block;
}

pub fn makeLoop(self: *Self) !InstData.Ref {
    return try self.dfg.pushLoop(self.active_block, [2]?Block.Ref{ null, null });
}
pub fn breakTo(self: *Self, target: InstData.Ref, value: ?Value) !InstData.Ref {
    return try self.dfg.pushBreak(self.active_block, target, value);
}

pub fn setLoopTarget(self: *Self, loop: InstData.Ref, body: Block.Ref, finally: ?Block.Ref) !void {
    const loop_inst = self.dfg.instructions.getPtr(loop);
    debug.assertPrint(self.getBlock(body).initiator.?.ref == loop.ref, "Body {?} wasn't created from loop", .{body});
    debug.assertPrint(finally == null or self.getBlock(finally.?).initiator.?.ref == loop.ref, "Finally {?} wasn't created from loop", .{finally});

    loop_inst.loop.args[0] = body;
    loop_inst.loop.args[1] = finally;
}

pub fn ret(self: *Self, value: ?Value, is_comptime: bool) !void {
    const val = value orelse Value.Imm(.void, .void);
    self.consumeValue(val);

    try self.dfg.pushReturn(self.active_block, val, is_comptime);
}
