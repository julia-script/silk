const std = @import("std");
const Builder = @import("Builder.zig");
const Mir = @import("../Mir.zig");
const Logger = @import("../Logger.zig");
const InternedSlice = @import("../InternedSlice.zig");

const ScopeKind = enum {
    @"fn",
    block,
    module,
    root,
};

const Symbol = union(enum) {
    global: struct {
        name: []const u8,
        type: Mir.Type.Index,
        value: ?Mir.Value.Index,
    },
    local: struct {
        name: []const u8,
        type: Mir.Type.Index,
        is_param: bool,
    },
};
pub const Scope = struct {
    kind: ScopeKind,
    parent: ?*Scope,
    children: std.ArrayList(Scope),
    builder: *Builder,
    symbols_table: std.AutoHashMap(u32, Mir.Type.Index),
    instructions: std.AutoHashMapUnmanaged(Mir.Instruction.Index, Symbol),

    pub fn init(builder: *Builder, kind: ScopeKind) Scope {
        return .{
            .kind = kind,
            .parent = null,
            .children = std.ArrayList(Scope).init(builder.alloc),
        };
    }
};
