const std = @import("std");
const InternedStrings = @import("InternedStrings.zig");
const InternedSlice = InternedStrings.InternedSlice;
const Array = std.ArrayListUnmanaged;
const Self = @This();
const Allocator = std.mem.Allocator;
const PackedLists = @import("PackedLists.zig");
const shared = @import("shared.zig");
pub const Lists = PackedLists.new(u32, std.math.maxInt(u32));
pub const ChildList = Lists.List;
const IndentedWriter = @import("IndentedWriter.zig");

lists: Lists = .{},
strings: InternedStrings,
instructions_dep: Array(Inst) = .{},
instructions: Array(Instruction) = .{},
definitions: Array(Definition) = .{},
types: Array(Type) = .{},
values: Array(Value) = .{},
allocator: Allocator,

pub fn init(allocator: Allocator) !Self {
    return .{
        .allocator = allocator,

        .strings = try InternedStrings.init(allocator),
    };
}
pub fn deinit(self: *Self) void {
    self.strings.deinit();
    self.lists.deinit(self.allocator);
    self.instructions_dep.deinit(self.allocator);
    self.instructions.deinit(self.allocator);
    self.types.deinit(self.allocator);
    self.values.deinit(self.allocator);
    self.definitions.deinit(self.allocator);
}
pub const Definition = union(enum) {
    // global_fn: GlobalFn,

    param: struct {
        name: InternedSlice,
        type: Type.Index,
    },

    global_decl: GlobalDecl,

    module: struct {
        name: ?InternedSlice,
        member_defs: Definition.List,
    },

    block: struct {
        instructions_dep: List,
        result_type: Type.Index,
    },

    inline_block: struct {
        instructions_dep: List,
        result_type: Type.Index,
    },

    pub const GlobalDecl = struct {
        name: InternedSlice,
        type: Type.Index,
        body: ?Definition.Index,
        // init_inst: ?Inst.Index,

        visibility: shared.Visibility,
        exported: bool,
        @"extern": bool,
    };

    pub const Index = u32;
    pub const List = usize;
    pub const RootIndex: Index = 0;
};

pub const Instruction = struct {
    is_comptime: bool,
    op: Op,
    data: Data,
    type: Type.Index,

    pub const Index = u32;

    pub const Data = union {
        void: void,
        binOp: BinOp,
        instruction: Instruction.Index,
        value: Value.Index,
        type: Type.Index,

        if_expr: struct {
            cond: Instruction.Index,
            then_body: Type.Index,
            else_body: ?Type.Index,
        },
        loop: struct { body: Type.Index },

        scoped: struct {
            name: InternedSlice,
            scope_index: Type.Index,
            index: ?u32,
        },

        pub const BinOp = struct {
            lhs: Instruction.Index,
            rhs: Instruction.Index,
        };
    };

    // op:
    pub const Op = enum {
        constant,
        param_get,
        local_get,
        global_get,
        ret,
        as,
        add,
        sub,
        mul,
        div,
        gt,
        lt,
        block,

        if_expr,
        loop,
        br,

        pub fn fromString(comptime str: []const u8) Op {
            return comptime std.meta.stringToEnum(Op, str) orelse @compileError("instruction op not found: " ++ str);
        }
    };
};
pub const Inst = union(enum) {
    constant: struct {
        type: Type.Index,
        value: Value.Index,
    },
    add: BinOp,

    get_param: NamedType,
    get_global: NamedType,
    ret: TypedInst,

    pub const NamedType = struct {
        type: Type.Index,
        name: InternedSlice,
    };
    pub const NamedInst = struct {
        inst: Inst.Index,
        name: InternedSlice,
    };
    pub const TypedInst = struct {
        inst: Inst.Index,
        type: Type.Index,
    };
    pub const Index = enum(u32) {
        unknown,
        _,
        pub const INDEX_START: u32 = std.meta.tags(Index).len;

        pub fn asInt(self: Index) u32 {
            return @intCast(@intFromEnum(self));
        }

        pub fn asTypeIndex(self: u32) Index {
            return @enumFromInt(self);
        }

        pub fn toTypeIndex(index: u32) Index {
            return @enumFromInt(
                @as(u32, @intCast(index + INDEX_START)),
            );
        }

        pub fn toInt(self: Index) ?u32 {
            const index = @intFromEnum(self);
            if (index < INDEX_START) return null;
            return @intCast(index - INDEX_START);
        }
    };
    pub const List = usize;
    pub const BinOp = struct {
        type: Type.Index,
        lhs: Inst.Index,
        rhs: Inst.Index,
    };
    // pub const INDEX_START: u32 = std.meta.tags(@This().Index).len;
    // pub const Index = enum(u32) {
    //     _,
    //     pub inline fn hash(self: Index) []const u8 {
    //         return std.mem.asBytes(&self);
    //     }
    //     pub fn asInt(self: Index) u32 {
    //         return @intCast(@intFromEnum(self));
    //     }

    //     pub fn asTypeIndex(self: u32) @This().Index {
    //         return @enumFromInt(self);
    //     }

    //     pub fn toTypeIndex(index: u32) Index {
    //         return @enumFromInt(
    //             @as(u32, @intCast(index + INDEX_START)),
    //         );
    //     }

    //     pub fn toInt(self: Index) ?u32 {
    //         const index = @intFromEnum(self);
    //         if (index < INDEX_START) return null;
    //         return @intCast(index - INDEX_START);
    //     }
    //     pub fn fromInt(index: u32) Index {
    //         return @enumFromInt(index + INDEX_START);
    //     }
    // };
    // global_decl: struct {
    //     name: InternedSlice,
    //     init: ?u32,
    // },
};

pub const Type = union(enum) {
    @"fn": Fn,
    while_loop: WhileLoop,
    optional: Optional,
    module: Module,

    // these are not types by themselves but partials of other types
    decl: Module.Decl,
    param: Fn.Param,
    block: Block,

    // local_get: struct {
    //     type: Type.Index,
    //     index: Inst.Index,
    // },
    // load: struct {
    //     type: Type.Index,
    //     index: Inst.Index,
    // },
    pub const List = usize;
    pub const INDEX_START: u32 = std.meta.tags(@This().Index).len;
    pub const Index = enum(u32) {
        unknown,

        boolean,
        number,
        string,
        void,
        i32,
        i64,
        f32,
        f64,

        type_number,
        type_string,
        type_boolean,
        type_void,
        type_i32,
        type_i64,
        type_f32,
        type_f64,

        _,
        pub inline fn hash(self: Index) []const u8 {
            return std.mem.asBytes(&self);
        }
        pub fn asInt(self: Index) u32 {
            return @intCast(@intFromEnum(self));
        }

        pub fn asTypeIndex(self: u32) Index {
            return @enumFromInt(self);
        }

        pub fn toTypeIndex(index: u32) Index {
            return @enumFromInt(
                @as(u32, @intCast(index + INDEX_START)),
            );
        }

        pub fn toInt(self: Index) ?u32 {
            const index = @intFromEnum(self);
            if (index < INDEX_START) return null;
            return @intCast(index - INDEX_START);
        }
        pub fn fromInt(index: u32) Index {
            return @enumFromInt(index + INDEX_START);
        }

        pub const Formatter = struct {
            mir: *Self,
            index: Index,
            pub fn format(
                self: @This(),
                comptime _: []const u8,
                options: std.fmt.FormatOptions,
                writer: std.io.AnyWriter,
            ) !void {
                _ = fmt; // autofix
                _ = options; // autofix
                if (self.index.toInt()) |index| {
                    try writer.print("%({d}:{s})", .{ index, @tagName(self.mir.types.items[index]) });
                } else {
                    try writer.print("%({s})", .{@tagName(self.index)});
                }
            }
        };
        pub fn fmt(self: Index, mir: *Self) Formatter {
            return .{ .mir = mir, .index = self };
        }
    };

    pub const RootIndex: Index = Index.fromInt(0);

    pub const Fn = struct {
        params: Type.List,
        return_type: Type.Index,
        body: ?Type.Index,
        pub const Param = struct {
            name: InternedSlice,
            type: Type.Index,
        };
    };
    pub const Module = struct {
        decls: Type.List,
        pub const Decl = struct {
            name: InternedSlice,
            type: Type.Index,
            init: ?Value.Index,
        };
    };
    pub const Optional = struct {
        child: Type.Index,
    };
    pub const Block = struct {
        name: ?InternedSlice,
        instruction_start: Instruction.Index,
        instruction_count: u32,
    };
    pub const WhileLoop = struct {
        condition: Instruction.Index,
        body: Type.Index,
    };
};
pub const Value = union(enum) {
    type: Type.Index,

    @"fn": Fn,

    float: f64,

    pub const List = usize;
    pub const INDEX_START: u32 = std.meta.tags(@This().Index).len;
    pub const Index = enum(u32) {
        true,
        false,
        none,
        undefined,

        // type_number,
        // type_string,
        // type_boolean,
        // type_void,
        _,
        pub inline fn hash(self: Index) []const u8 {
            return std.mem.asBytes(&self);
        }
        pub fn asInt(self: Index) u32 {
            return @intCast(@intFromEnum(self));
        }

        pub fn asTypeIndex(self: u32) Index {
            return @enumFromInt(self);
        }

        pub fn toTypeIndex(index: u32) Index {
            return @enumFromInt(
                @as(u32, @intCast(index + INDEX_START)),
            );
        }

        pub fn toInt(self: Index) ?u32 {
            const index = @intFromEnum(self);
            if (index < INDEX_START) return null;
            return @intCast(index - INDEX_START);
        }
        pub fn fromInt(index: u32) Index {
            return @enumFromInt(index + INDEX_START);
        }
    };
    pub const Fn = struct {
        type: Type.Index,
        instructions_dep: Inst.List,
    };
};
const Logger = @import("Logger.zig");
pub fn formatValue(self: *Self, writer: *IndentedWriter, value_index: Value.Index) std.io.AnyWriter.Error!void {
    if (value_index.toInt()) |index| {
        const value: Value = self.values.items[index];
        switch (value) {
            .type => |type_index| {
                try formatType(self, writer, type_index);
            },
            .float => |float| {
                try writer.print("{d}", .{float});
            },

            else => {},
        }
        // try writer.writeAll(value.fmt(self));
    } else {
        try writer.writeAllIndented(@tagName(value_index));
    }
}
pub fn formatInstruction(self: *Self, writer: *IndentedWriter, inst_index: Instruction.Index) std.io.AnyWriter.Error!void {
    const inst: Instruction = self.instructions.items[inst_index];
    try writer.printIndented("#{d} = ", .{
        inst_index,
    });
    switch (inst.op) {
        .constant => {
            try writer.writeAll("const.");
            try formatType(self, writer, inst.type);
            try writer.writeAll(" ");
            try formatValue(self, writer, inst.data.value);
            try writer.writeAll(";\n");
        },
        .global_get, .param_get => {
            // try writer.print("{s} ", .{@tagName(inst)});
            try writer.writeAll(@tagName(inst.op));
            try writer.writeAll(" ");
            try formatType(self, writer, inst.type);
            try writer.writeAll(" '");
            try writer.writeAll(self.strings.getSlice(inst.data.scoped.name));
            try writer.writeAll("';\n");
        },
        .add => {
            try writer.writeAll("add ");
            try formatType(self, writer, inst.type);
            try writer.writeAll(" ");
            try writer.print("#{?d} #{?d};\n", .{ inst.data.binOp.lhs, inst.data.binOp.rhs });
        },
        .ret => {
            try writer.print("{s} ", .{@tagName(inst.op)});
            try formatType(self, writer, inst.type);
            try writer.writeAll(" ");
            switch (inst.type) {
                .void => {},
                else => try writer.print("#{?d};\n", .{inst.data.instruction}),
            }
        },
        .if_expr => {
            try writer.print("if #{d}\n", .{inst.data.if_expr.cond});
            writer.indent();
            try formatType(self, writer, inst.data.if_expr.then_body);
            writer.unindent();
            if (inst.data.if_expr.else_body) |else_body| {
                try writer.writeIndent();
                try writer.writeAll("else\n");
                writer.indent();
                try formatType(self, writer, else_body);
                writer.unindent();
            }
        },
        .block => {
            try formatType(self, writer, inst.type);
        },
        .loop => {
            try writer.writeAll("loop\n");
            try formatType(self, writer, inst.data.loop.body);
        },
        else => {
            try writer.print("{s};\n", .{@tagName(inst.op)});
        },
    }
    // try writer.writeAll("\n");
}
pub fn formatType(self: *Self, writer: *IndentedWriter, type_index: Type.Index) !void {
    if (type_index.toInt()) |index| {
        const ty: Type = self.types.items[index];
        switch (ty) {
            .@"fn" => |fn_ty| {
                var iter = self.lists.iterList(fn_ty.params);
                try writer.writeAll("fn(");
                var i: usize = 0;
                while (iter.next()) |param_index| {
                    if (i > 0) try writer.writeAll(", ");
                    try formatType(self, writer, Type.Index.asTypeIndex(param_index));
                    // const param_type: Type.x = self.types.items[param_index];
                    // try formatType(self, writer, param_def.param.type);
                    i += 1;
                }
                try writer.writeAll(") -> ");
                try formatType(self, writer, fn_ty.return_type);

                if (fn_ty.body) |body_index| {
                    try writer.writeAll("\n");
                    // try writer.writeAllIndented("body:\n");
                    try formatType(self, writer, body_index);
                }
                // if (fn_ty.body) |body_index| {
                //     var iter_instructions_dep = self.lists.iterList(body_index);
                //     while (iter_instructions_dep.next()) |inst_index| {
                //         try formatInstruction(self, writer, Inst.Index.asTypeIndex(inst_index));
                //     }
                //     // try formatType(self, writer, Type.Index.asTypeIndex(body_index));
                // }
            },
            .block => |block| {
                try writer.open("block", .{});
                defer writer.close();
                // var iter = self.lists.iterList(block.instructions);

                var i: u32 = block.instruction_start;
                while (i < block.instruction_start + block.instruction_count) : (i += 1) {
                    try formatInstruction(self, writer, i);
                }
            },
            .module => |mod| {
                try writer.open("module", .{});
                defer writer.close();
                var iter = self.lists.iterList(mod.decls);

                while (iter.next()) |decl_index| {
                    try writer.writeIndent();

                    try formatType(self, writer, Type.Index.asTypeIndex(decl_index));
                    // const decl = self.types.items[decl_index];

                    // try writer.writeAll(@tagName(decl.type));
                    // try writer.writeAll(" ");
                    // try writer.writeAll(self.strings.getSlice(decl.name));
                    // try writer.writeAll(";\n");
                    try writer.writeAll("\n");
                }
            },
            .decl => |decl| {
                try writer.writeAll("@");
                try writer.writeAll(self.strings.getSlice(decl.name));
                try writer.writeAll(": ");
                try formatType(self, writer, decl.type);
            },
            .param => |param| {
                try writer.writeAll(self.strings.getSlice(param.name));
                try writer.writeAll(": ");
                try formatType(self, writer, param.type);
            },

            else => {
                try writer.writeAll(@tagName(ty));
            },
        }
        return;
    }

    try writer.writeAll(@tagName(type_index));
}
pub fn format(self_: Self, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
    _ = fmt; // autofix
    _ = options; // autofix

    var self: *Self = @constCast(&self_);
    // const root_type = self.types.items[0];
    var indented_writer = IndentedWriter.init(writer);
    // try indented_writer.open("root_module", .{});
    try self.formatType(&indented_writer, Type.RootIndex);

    // try indented_writer.close();
    // try indented_writer.open("root_module");
    // const root_type = self.types.items[Type.RootIndex];
    // try self.formatType(&indented_writer, root_type);
    // try indented_writer.close();
    // const root_def = self.definitions.items[Definition.RootIndex];

    // std.debug.print("root_def: {}\n", .{root_def});
    // logger.open("#{d} root_module", .{Definition.RootIndex});
    // defer logger.close();
    // var iter = self.lists.iterList(root_def.module.member_defs);
    // while (iter.next()) |member_index| {
    //     const member_def: Definition = self.definitions.items[member_index];
    //     const member_name = self.strings.getSlice(member_def.global_decl.name);
    //     logger.writeIndent();
    //     try writer.print("#{d} member \"{s}\": ", .{ member_index, member_name });
    //     try formatType(self, writer, member_def.global_decl.type);
    //     // logger.openInline("#{d} member \"{s}\":", .{ member_index, member_name });
    //     logger.openInline();

    //     defer logger.close();

    //     logger.writeIndent();

    //     try writer.writeAll("\n");
    //     // defer logger.close();
    // }
    // _ = root_def; // autofix
    // for (self.definitions.items) |def| {
    //     std.debug.print("def: {}\n", .{def});
    // }

    // try formatInstruction(
    //     @constCast(&value),
    //     writer,
    //     options,
    //     Inst.RootIndex,
    //     0,
    // );
}
