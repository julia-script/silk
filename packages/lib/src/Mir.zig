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
const MirBuilder = @import("MirBuilder2.zig");
const Logger = @import("Logger.zig");
const tw = Logger.tw;

lists: Lists = .{},
strings: InternedStrings,
instructions: Array(Instruction) = .{},
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
    self.instructions.deinit(self.allocator);
    self.types.deinit(self.allocator);
    self.values.deinit(self.allocator);
}

pub const Instruction = struct {
    op: Op,
    data: Data,
    type: Type.Index,
    value: Value.Index,
    liveness: u32 = 1,

    pub const Index = u32;
    pub const List = usize;

    pub const Data = union(enum) {
        wip: usize,
        // void: void,
        bin_op: BinOp,
        instruction: Instruction.Index,
        // value: Value.Index,
        type: Type.Index,
        void: void,

        if_expr: struct {
            cond: Instruction.Index,
            then_body: Type.Index,
            else_body: ?Type.Index,
        },
        loop: struct { body: Type.Index },

        scoped: struct {
            name: InternedSlice,
            // scope_index: Type.Index,
            index: u32,
            mutable: bool,
        },
        call: struct {
            callee: Type.Index,
            args_list: List,
        },
        // local: struct {
        //     name: InternedSlice,
        //     type: Type.Index,
        //     index: u32 = 0,
        // },

        pub const BinOp = struct {
            lhs: Instruction.Index,
            rhs: Instruction.Index,
        };
    };
    pub fn getValue(self: Instruction) Value.Index {
        switch (self.data) {
            .scoped => |scoped| {
                if (scoped.mutable) {
                    return .runtime;
                }
                return self.value;
            },
            else => {
                return self.value;
            },
        }
    }

    // pub fn format(
    //     self: @This(),
    //     comptime _: []const u8,
    //     options: std.fmt.FormatOptions,
    //     writer: std.io.AnyWriter,
    // ) !void { // op:
    //     _ = options; // autofix
    //     try writer.print("{s} ", .{@tagName(self.op)});
    //     try writer.print(" {any}", .{self.type});
    //     try writer.print(" {any}", .{self.value});
    //     switch (self.data) {
    //         .bin_op => |bin_op| {
    //             try writer.print(" lhs: #{d} rhs: #{d}", .{ bin_op.lhs, bin_op.rhs });
    //         },
    //         .instruction => |instruction| {
    //             try writer.print(" inst: #{d}", .{instruction});
    //         },
    //         // .value => |value| {
    //         //     try writer.print(" value: #{any}", .{value});
    //         // },
    //         .type => |type_index| {
    //             try writer.print(" type: {any}", .{type_index});
    //         },
    //         .if_expr => |if_expr| {
    //             try writer.print(" cond: #{d} then: #{any}", .{ if_expr.cond, if_expr.then_body });
    //             if (if_expr.else_body) |else_body| {
    //                 try writer.print(" else: #{any}", .{else_body});
    //             }
    //         },
    //         .scoped => |scoped| {
    //             _ = scoped; // autofix
    //             try writer.print(" scoped", .{});
    //         },

    //         else => {},
    //     }
    // }
    pub const Op = enum {
        type,
        constant,

        param,
        param_get,
        param_set,

        local,
        local_get,
        local_set,

        global_get,
        global_set,

        ret,
        as,

        add,
        sub,
        mul,
        div,

        gt,
        ge,
        lt,
        le,
        eq,
        ne,

        block,

        if_expr,
        loop,
        br,
        call,

        pub fn fromString(comptime str: []const u8) Op {
            return comptime std.meta.stringToEnum(Op, str) orelse @compileError("instruction op not found: " ++ str);
        }
    };
    pub fn accept(self: Instruction, op: Op) bool {
        return self.op == op;
    }
    pub fn acceptEither(self: Instruction, comptime ops: []const Op) bool {
        inline for (ops) |op| {
            if (self.accept(op)) return true;
        }
        return false;
    }
};
// pub const Inst = union(enum) {
//     constant: struct {
//         type: Type.Index,
//         value: Value.Index,
//     },
//     add: BinOp,

//     get_param: NamedType,
//     get_global: NamedType,
//     ret: TypedInst,

//     pub const NamedType = struct {
//         type: Type.Index,
//         name: InternedSlice,
//     };
//     pub const NamedInst = struct {
//         inst: Inst.Index,
//         name: InternedSlice,
//     };
//     pub const TypedInst = struct {
//         inst: Inst.Index,
//         type: Type.Index,
//     };
//     pub const Index = enum(u32) {
//         unknown,
//         _,
//         pub const INDEX_START: u32 = std.meta.tags(Index).len;

//         pub fn asInt(self: Index) u32 {
//             return @intCast(@intFromEnum(self));
//         }

//         pub fn asTypeIndex(self: u32) Index {
//             return @enumFromInt(self);
//         }

//         pub fn toTypeIndex(index: u32) Index {
//             return @enumFromInt(
//                 @as(u32, @intCast(index + INDEX_START)),
//             );
//         }

//         pub fn toInt(self: Index) ?u32 {
//             const index = @intFromEnum(self);
//             if (index < INDEX_START) return null;
//             return @intCast(index - INDEX_START);
//         }
//     };
//     pub const List = usize;
//     pub const BinOp = struct {
//         type: Type.Index,
//         lhs: Inst.Index,
//         rhs: Inst.Index,
//     };
//     // pub const INDEX_START: u32 = std.meta.tags(@This().Index).len;
//     // pub const Index = enum(u32) {
//     //     _,
//     //     pub inline fn hash(self: Index) []const u8 {
//     //         return std.mem.asBytes(&self);
//     //     }
//     //     pub fn asInt(self: Index) u32 {
//     //         return @intCast(@intFromEnum(self));
//     //     }

//     //     pub fn asTypeIndex(self: u32) @This().Index {
//     //         return @enumFromInt(self);
//     //     }

//     //     pub fn toTypeIndex(index: u32) Index {
//     //         return @enumFromInt(
//     //             @as(u32, @intCast(index + INDEX_START)),
//     //         );
//     //     }

//     //     pub fn toInt(self: Index) ?u32 {
//     //         const index = @intFromEnum(self);
//     //         if (index < INDEX_START) return null;
//     //         return @intCast(index - INDEX_START);
//     //     }
//     //     pub fn fromInt(index: u32) Index {
//     //         return @enumFromInt(index + INDEX_START);
//     //     }
//     // };
//     // global_decl: struct {
//     //     name: InternedSlice,
//     //     init: ?u32,
//     // },
// };

pub const Type = union(enum) {
    @"fn": Fn,
    optional: Optional,
    module: Module,

    // these are not types by themselves but partials of other types
    global: Module.Decl,
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

        param,

        boolean,
        number,
        string,
        void,
        i32,
        i64,
        i128,

        f32,
        f64,
        type,

        // type_number,
        // type_string,
        // type_boolean,
        // type_void,
        // type_i32,
        // type_i64,
        // type_f32,
        // type_f64,

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

        pub fn toValueIndex(self: Index) Value.Index {
            switch (self) {
                .i32 => return .type_i32,
                .i64 => return .type_i64,
                .f32 => return .type_f32,
                .f64 => return .type_f64,
                .boolean => return .type_boolean,
                .number => return .type_number,
                .unknown => return .runtime,
                else => {
                    std.debug.panic("not implemented: toValueIndex: {s}", .{@tagName(self)});
                },
            }
        }
        pub fn isNumeric(self: Index) bool {
            switch (self) {
                .i32,
                .i64,
                .i128,
                .f32,
                .f64,
                .number,
                => return true,
                else => return false,
            }
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
        name: InternedSlice,
        params: Type.List,
        return_type: Type.Index,
        init_block: ?Type.Index,
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
            init: ?Type.Index,
        };
    };
    pub const Optional = struct {
        child: Type.Index,
    };
    pub const Block = struct {
        locals_count: usize,
        name: ?InternedSlice,
        instructions: Instruction.List,
    };
};
pub const Value = union(enum) {
    type: Type.Index,

    @"fn": Fn,

    float: f64,
    integer: i64,
    big_integer: i128,
    pub const List = usize;
    pub const INDEX_START: u32 = std.meta.tags(@This().Index).len;
    pub const Index = enum(u32) {
        runtime,
        true,
        false,
        none,
        undefined,
        void,

        type_number,
        // type_string,
        // type_boolean,
        // type_void,
        type_f32,
        type_f64,
        type_i32,
        type_i64,
        type_boolean,

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
        pub fn toType(self: Index) Type.Index {
            switch (self) {
                .type_number => return .number,
                .type_f32 => return .f32,
                .type_f64 => return .f64,
                .type_i32 => return .i32,
                .type_i64 => return .i64,
                .type_boolean => return .boolean,
                .runtime => return .unknown,
                else => {
                    std.debug.panic("not implemented: toType: {s}", .{@tagName(self)});
                },
            }
        }
    };
    pub fn isNumeric(self: Value) bool {
        switch (self) {
            .integer, .float, .big_integer => return true,
            else => return false,
        }
    }
    pub fn accept(self: Value, tag: std.meta.Tag(Value)) bool {
        return std.meta.activeTag(self) == tag;
    }
    pub const Fn = struct {
        type: Type.Index,
    };
};
pub fn formatValue(self: *Self, writer: *Logger, value_index: Value.Index) std.io.AnyWriter.Error!void {
    if (value_index.toInt()) |index| {
        try writer.print("[{}]", .{self.values.items[index]});
    } else {
        try writer.print("[{s}]", .{@tagName(value_index)});
    }
}
pub fn formatInstruction(self: *Self, writer: *Logger, inst_index: Instruction.Index) std.io.AnyWriter.Error!void {
    const instruction: Instruction = self.instructions.items[inst_index];
    const tag_name = @tagName(instruction.data);
    const data: Instruction.Data = instruction.data;
    try writer.writeIndent();
    try writer.print("{s: <4}", .{tag_name[0..@min(tag_name.len, 4)]});
    try writer.print("[{d: >3}]", .{inst_index});
    try writer.print("{s: <3}", .{if (instruction.liveness == 0) "!" else ""});
    try writer.print("{s}", .{@tagName(instruction.op)});
    switch (data) {
        .scoped => |scoped| {
            // try self.dumpType(writer, instruction.type, depth + 1);
            try writer.print("('{s}', index = {d})", .{ self.strings.getSlice(scoped.name), scoped.index });
        },
        .bin_op => |bin_op| {
            try writer.print("(%{d}, %{d})", .{ bin_op.lhs, bin_op.rhs });
        },
        .instruction => |instruction_index| {
            try writer.print("(#{d})", .{instruction_index});
        },
        // .value => |value| {
        //     _ = value; // autofix
        //     // try self.dumpValue(writer, value, depth + 1);

        // },
        .type => |type_index| {
            try self.formatType(writer, type_index);
        },
        .void => {},
        .call => |call| {
            var iter = self.lists.iterList(call.args_list);
            try writer.print("(#{?d}) with (", .{call.callee.toInt()});
            var first = true;
            while (iter.next()) |arg_id| {
                if (!first) {
                    try writer.writeAll(", ");
                }
                first = false;
                try writer.print("#{d}", .{arg_id});
            }
            try writer.writeAll(")");
        },
        .if_expr => |if_expr| {
            try writer.print(" condition #{d} then:\n", .{if_expr.cond});
            writer.indent();
            try self.formatType(writer, if_expr.then_body);
            writer.unindent();
            if (if_expr.else_body) |else_body| {
                try writer.writeIndent();
                try writer.writeAll("else:\n");
                writer.indent();
                try self.formatType(writer, else_body);
                writer.unindent();
            }
            return;
        },
        .loop => |loop| {
            try writer.writeAll("\n");
            try self.formatType(writer, loop.body);
            return;
        },
        else => {
            try writer.writeAll("(TODO)");
        },
    }

    try writer.writeAll(": ");

    try self.formatType(writer, instruction.type);
    try writer.writeAll(" -> ");

    try self.formatValue(writer, instruction.value);
    // if (self.getValue(instruction.value)) |value| {
    //     try writer.print("[{}]", .{value});
    // } else {
    //     try writer.print("[{s}]", .{@tagName(instruction.value)});
    // }
    // try tw.gray_500.csiClose(writer);
    // try writer.writeAll("|\n");
    // try writer.printIndented("#{d} = .{s} {any}", .{ inst_index, @tagName(inst.op), inst.data });
    // try writer.printIndented("#{d}:!{d} = ", .{
    //     inst_index,
    //     inst.liveness,
    // });
    // switch (inst.op) {
    //     .constant => {
    //         try writer.writeAll("const.");
    //         try formatType(self, writer, inst.type);
    //         try writer.writeAll(" ");
    //         try formatValue(self, writer, inst.value);
    //         try writer.writeAll(";\n");
    //     },
    //     .global_get => {
    //         // try writer.print("{s} ", .{@tagName(inst)});
    //         try writer.writeAll(@tagName(inst.op));
    //         try writer.writeAll(" ");
    //         try formatType(self, writer, inst.type);
    //         try writer.writeAll(" '");
    //         try formatType(self, writer, inst.data.type);

    //         // try writer.writeAll(self.strings.getSlice(inst.data.scoped.name));
    //         try writer.writeAll("';\n");
    //     },

    //     .add,
    //     .sub,
    //     .mul,
    //     .div,
    //     .gt,
    //     .lt,
    //     .eq,
    //     => {
    //         try writer.writeAll(@tagName(inst.op));
    //         try writer.writeAll(" ");
    //         try formatType(self, writer, inst.type);
    //         try writer.writeAll(" ");
    //         try writer.print("#{?d} #{?d};\n", .{ inst.data.bin_op.lhs, inst.data.bin_op.rhs });
    //     },
    //     .ret => {
    //         try writer.print("{s} ", .{@tagName(inst.op)});
    //         try formatType(self, writer, inst.type);
    //         try writer.writeAll(" ");
    //         switch (inst.type) {
    //             .void => {},
    //             else => try writer.print("#{?d};", .{inst.data.instruction}),
    //         }
    //         try writer.writeAll("\n");
    //     },
    //     .if_expr => {
    //         try writer.print("if #{d}\n", .{inst.data.if_expr.cond});
    //         writer.indent();
    //         try formatType(self, writer, inst.data.if_expr.then_body);
    //         writer.unindent();
    //         if (inst.data.if_expr.else_body) |else_body| {
    //             try writer.writeIndent();
    //             try writer.writeAll("else\n");
    //             writer.indent();
    //             try formatType(self, writer, else_body);
    //             writer.unindent();
    //         }
    //     },
    //     .block => {
    //         try formatType(self, writer, inst.type);
    //     },
    //     .loop => {
    //         try writer.writeAll("loop\n");
    //         try formatType(self, writer, inst.data.type);
    //     },
    //     .as => {
    //         try writer.print("#{d} as ", .{inst.data.bin_op.lhs});
    //         try formatType(self, writer, inst.type);
    //         try writer.writeAll(";\n");
    //     },
    //     .param => {
    //         try writer.writeAll("param(");
    //         try formatType(self, writer, inst.type);
    //         try writer.writeAll(")");
    //         // try writer.writeAll(self.strings.getSlice(inst.data.scoped.name));
    //         try writer.writeAll(";\n");
    //     },
    //     .param_get, .local_get => {
    //         try writer.writeAll(@tagName(inst.op));
    //         try writer.writeAll(" ");
    //         try formatType(self, writer, inst.type);
    //         try writer.print(" #{d};\n", .{inst.data.instruction});
    //     },
    //     .local => {
    //         try writer.writeAll("local ");
    //         try formatType(self, writer, inst.type);
    //         try writer.writeAll(" '");
    //         try writer.writeAll(self.strings.getSlice(inst.data.scoped.name));
    //         try writer.writeAll("';\n");
    //     },
    //     .local_set => {
    //         try writer.writeAll("local_set ");
    //         try formatType(self, writer, inst.type);
    //         try writer.print(" #{d} = #{d};\n", .{ inst.data.bin_op.lhs, inst.data.bin_op.rhs });
    //     },
    //     .type => {
    //         try writer.writeAll("type ");
    //         try formatType(self, writer, inst.data.type);
    //         try writer.writeAll(";\n");
    //     },

    //     else => {
    //         try writer.print("{s};\n", .{@tagName(inst.op)});
    //     },
    // }
    // try writer.writeAll("\n");
}
pub fn formatType(self: *Self, writer: *Logger, type_index: Type.Index) !void {
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

                if (fn_ty.init_block) |body_index| {
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
                writer.open("block", .{});
                // defer writer.close();
                var iter = self.lists.iterList(block.instructions);

                while (iter.next()) |inst_index| {
                    try formatInstruction(self, writer, inst_index);
                    try writer.writeAll("\n");
                }
                writer.unindent();
                try writer.writeIndent();
                try writer.writeAll("}");
            },
            .module => |mod| {
                writer.open("module", .{});
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
                    // try writer.writeAll("\n");
                }
            },
            .global => |global| {
                try writer.writeAll("@");
                try writer.writeAll(self.strings.getSlice(global.name));
                try writer.writeAll(": ");
                try formatType(self, writer, global.type);
                if (global.init) |init_type| {
                    try formatType(self, writer, init_type);
                }
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
    var indented_writer = Logger.init(writer, "Mir");
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
