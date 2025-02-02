const Function = @import("./Function.zig");
const FunctionDeclaration = @import("./FunctionDeclaration.zig");
const Module = @import("./Module.zig");
const Dfg = @import("./Dfg.zig");
const Signature = @import("./Signature.zig");
const std = @import("std");
const Definition = @import("./Definition.zig");
const Inst = @import("./inst.zig").Inst;
const InstVal = Inst.InstVal;
const assert = std.debug.assert;
allocator: std.mem.Allocator,
module: *Module,
dfg: Dfg,
kind: Kind,

pub const Kind = union(enum) {
    function_body: FunctionDeclaration.Ref,
    inline_expression,
    global_body,
};

const Self = @This();

pub fn init(allocator: std.mem.Allocator, module: *Module, kind: Kind) !Self {
    return .{
        .allocator = allocator,
        .module = module,
        .dfg = try Dfg.init(allocator),
        .kind = kind,
    };
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

pub fn iadd(self: *Self, a: InstVal, b: InstVal) !Inst.Ref {
    // assert(a.ty.eql(b.ty));
    return self.dfg.pushBinary(Dfg.EntryBlock, .iadd, a, b);
}

pub fn isub(self: *Self, a: InstVal, b: InstVal) !Inst.Ref {
    return self.dfg.pushBinary(Dfg.EntryBlock, .isub, a, b);
}

pub fn imul(self: *Self, a: InstVal, b: InstVal) !Inst.Ref {
    return self.dfg.pushBinary(Dfg.EntryBlock, .imul, a, b);
}

pub fn idiv(self: *Self, a: InstVal, b: InstVal) !Inst.Ref {
    return self.dfg.pushBinary(Dfg.EntryBlock, .idiv, a, b);
}
