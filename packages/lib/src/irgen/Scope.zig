const std = @import("std");
const Ast = @import("../Ast.zig");
const Ir = @import("../Ir.zig");
const ErrorManager = @import("../ErrorManager.zig");
const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const HashMap = std.AutoArrayHashMapUnmanaged;
const IrGen = @import("../IrGen.zig");
const InternedStrings = @import("../InternedStrings.zig");
const InternedSlice = InternedStrings.InternedSlice;
const Self = @This();
const Inst = Ir.Inst;
const Type = Ir.Type;
const Value = Ir.Value;
const Def = Ir.Def;
const assert = @import("../assert.zig");
const Block = struct {
    // name: InternedSlice,
    result: Type.Index = .void,
    params: Type.ListIndex = 0,
    inst: Inst.ListIndex = 0,
    inst_list: ?Ir.IrLists.List = null,
    local_definitions_table: HashMap(InternedSlice, Ir.Def) = .{},
};

const Mod = struct {
    definitions_table: HashMap(InternedSlice, Ir.Def.Index) = .{},
};
// gen: *Self,
irGen: *IrGen,
parent: ?*Self,

data: union(Kind) {
    mod: Mod,
    block: Block,
},
const Kind = enum {
    mod,
    block,
};

pub fn initMod(irGen: *IrGen, parent: ?*Self) !Self {
    return .{
        .irGen = irGen,
        .parent = parent,
        .data = .{ .mod = .{} },
    };
}
pub fn initBlock(irGen: *IrGen, parent: *Self) Self {
    return .{
        .irGen = irGen,
        .parent = parent,
        .data = .{
            .block = .{
                // .name = .{},
                // .inst = irGen.newList(),
            },
        },
    };
}
pub fn isKind(self: *Self, kind: Kind) bool {
    return std.meta.activeTag(self.data) == kind;
}
pub fn contains(self: *Self, id: InternedSlice) bool {
    return switch (self.data) {
        .mod => self.data.mod.definitions_table.contains(id),
        .block => self.data.block.local_definitions_table.contains(id),
    };
}
pub fn get(self: *Self, id: InternedSlice) ?*Ir.Def {
    switch (self.data) {
        .mod => {
            const index = self.data.mod.definitions_table.get(id) orelse return null;
            return &self.irGen.ir.defs.items[index];
        },
        .block => {
            return self.data.block.local_definitions_table.getPtr(id);
        },
    }
}
pub fn recursiveGet(self: *Self, id: InternedSlice) ?*Ir.Def {
    if (self.get(id)) |def| return def;
    if (self.parent) |parent| return parent.recursiveGet(id);
    return null;
}
pub fn put(self: *Self, allocator: Allocator, id: InternedSlice, index: Ir.Def.Index) !void {
    try self.definitions_table.put(allocator, id, index);
}

pub fn appendInstruction(self: *Self, inst: Inst.Index) !void {
    if (self.isKind(.block)) {
        assert.fmt(self.data.block.inst_list != null, "block scope must have an instruction list during instruction generation phase", .{});
        try self.data.block.inst_list.?.append(inst);
        return;
    }
    // unreachable;
}

pub fn makeFnWip(self: *Self, name: InternedSlice) !FnDefWip {
    return .{
        .scope = self,
        .name = name,
    };
}
pub fn makeGlobalDefWip(self: *Self, name: InternedSlice) !GlobalDefWip {
    return .{
        .scope = self,
        .name = name,
    };
}
pub fn appendWipDefinition(self: *Self, wip: *DefWip) !void {
    const name = wip.getName() orelse std.debug.panic("wip.name is null, definitions must have a name before being appended to a block scope", .{});
    if (self.isKind(.block)) {
        try self.data.block.local_definitions_table.put(self.irGen.allocator, name, wip.getDef());
        return;
    }

    const index: Ir.Def.Index = @intCast(self.irGen.ir.defs.items.len);
    try self.data.mod.definitions_table.put(self.irGen.arena.allocator(), name, index);
    try self.irGen.ir.defs.append(self.irGen.allocator, wip.getDef());

    if (std.meta.activeTag(wip.*) == .@"fn") {
        wip.@"fn".definition_index = index;
        return;
    }
    wip.global.definition_index = index;
}
pub fn appendDefinition(self: *Self, def: Def) !void {
    if (self.isKind(.block)) {
        try self.data.block.local_definitions_table.put(self.irGen.arena.allocator(), def.name, def);
        return;
    }
    const index: Ir.Def.Index = @intCast(self.irGen.ir.defs.items.len);
    try self.data.mod.definitions_table.put(self.irGen.arena.allocator(), def.name, index);
    try self.irGen.ir.defs.append(self.irGen.allocator, def);
}
pub inline fn makeDefWipList(self: *Self) std.ArrayList(DefWip) {
    return std.ArrayList(DefWip).init(self.irGen.arena.allocator());
}
pub const DefWip = union(enum) {
    @"fn": FnDefWip,
    global: GlobalDefWip,

    pub fn getName(self: *DefWip) ?InternedSlice {
        return switch (self.*) {
            .@"fn" => |*wip| wip.name,
            .global => |*wip| wip.name,
            // else => unreachable,
        };
    }

    pub fn getDef(self: *DefWip) Ir.Def {
        return switch (self.*) {
            .@"fn" => |*wip| .{
                .name = wip.name.?,
                .ty = wip.result_ty,
                .visibility = wip.visibility,
                .external = wip.external,
                .exported = wip.exported,
                .mutable = wip.mutable,
                .is_local = false,
            },
            .global => |*global_wip| .{
                .name = global_wip.name.?,
                .ty = global_wip.ty,
                .visibility = global_wip.visibility,
                .external = global_wip.external,
                .exported = global_wip.exported,
                .mutable = global_wip.mutable,
                .is_local = false,
            },
        };
    }
};
pub const FnDefWip = struct {
    scope: *Self,
    init_scope: Self,
    definition_index: ?Ir.Def.Index = null,
    name: ?InternedSlice = null,
    params_ty: ?Type.Index = null,
    param_defs: ?Def.Index = null,
    result_ty: Type.Index = .void,
    ty: ?Type.Index = null,
    visibility: Def.Visibility = .private,
    proto_node: ?Ast.Node.Index = null,
    init_node: ?Ast.Node.Index = null,
    init_inst: ?Inst.Index = null,
    external: bool = false,
    exported: bool = false,
    mutable: bool = false,

    pub fn init(scope: *Self) FnDefWip {
        return .{
            .scope = scope,
            .init_scope = Self.initBlock(scope.irGen, scope),
            // .param_tys = scope.irGen.newList(),
        };
    }
    pub fn getDef(self: *FnDefWip) Ir.Def {
        return .{
            .name = self.name.?,
            .ty = self.result_ty,
            .visibility = self.visibility,
            .external = self.external,
            .exported = self.exported,
            .mutable = self.mutable,
            .is_local = true,
        };
    }
    // pub fn sync(self: *FnDefWip) !void {
    //     if (self.definition_index) |index| {
    //         self.scope.irGen.ir.defs.items[index] = self.getDef();
    //     }
    // }
};
pub const GlobalDefWip = struct {
    scope: *Self,
    name: ?InternedSlice = null,
    definition_index: ?Ir.Def.Index = null,
    ty: Type.Index = .unknown,
    ty_node: ?Ast.Node.Index = null,
    visibility: Def.Visibility = .private,
    external: bool = false,
    exported: bool = false,
    mutable: bool = false,
    init_node: ?Ast.Node.Index = null,

    pub fn init(scope: *Self) GlobalDefWip {
        return .{
            .scope = scope,
        };
    }
};
