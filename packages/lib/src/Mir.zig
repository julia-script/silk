const std = @import("std");
const InternedStrings = @import("InternedStrings.zig");
const InternedSlice = InternedStrings.InternedSlice;
const Array = std.ArrayListUnmanaged;
const Self = @This();
const Allocator = std.mem.Allocator;
const PackedLists = @import("PackedLists.zig");
const shared = @import("shared.zig");
// pub const Lists = PackedLists.new(u32, std.math.maxInt(u32));
// pub const ChildList = Lists.List;
const MirBuilder = @import("MirBuilder.zig");
const Hir = @import("Hir.zig");
const ErrorManager = @import("ErrorManager.zig");
const Logger = @import("Logger.zig");
const tw = Logger.tw;
const InternedLists = @import("./interned-lists.zig").InternedLists(Instruction.Index);
pub const List = InternedLists.Range;
pub const WorkingList = InternedLists.WorkingList;
const Color = @import("./Color.zig");
const TreeWriter = @import("./TreeWriter.zig");

// lists: Lists = .{},
interned_lists: InternedLists,
strings: InternedStrings,
globals: Array(Global) = .{},
instructions: Array(Instruction) = .{},
types: Array(Type) = .{},
values: Array(Value) = .{},
allocator: Allocator,

pub fn init(allocator: Allocator) !Self {
    return .{
        .allocator = allocator,
        .interned_lists = InternedLists.init(allocator),
        .strings = try InternedStrings.init(allocator),
    };
}
pub fn deinit(self: *Self) void {
    self.strings.deinit();
    // self.lists.deinit(self.allocator);
    self.instructions.deinit(self.allocator);
    self.globals.deinit(self.allocator);
    self.types.deinit(self.allocator);
    self.values.deinit(self.allocator);
    self.interned_lists.deinit();
}

pub fn build(allocator: std.mem.Allocator, hir: *Hir, errors: *ErrorManager, options: MirBuilder.Options) !Self {
    return try MirBuilder.build(allocator, hir, errors, options);
}
pub const Instruction = struct {
    op: Op,
    data: Data,
    type: Type.Index,
    value: Value.Index,
    liveness: u32 = 1,

    pub const Index = u32;
    pub const List = InternedLists.Range;

    pub const Data = union(enum) {
        wip: usize,
        // void: void,
        bin_op: BinOp,
        instruction: Instruction.Index,
        value: Value.Index,
        type: Type.Index,
        void: void,

        alloc: struct {
            type: Type.Index,
            mutable: bool,
        },
        br: struct {
            instruction: Instruction.Index,
            value: Value.Index,
        },
        loop: struct {
            instructions_list: Instruction.List,
        },
        loop_wip: struct {
            wip: MirBuilder.Wip.Index,
        },
        block: struct {
            instructions_list: Instruction.List,
        },
        block_wip: struct {
            wip: MirBuilder.Wip.Index,
        },
        branch: struct {
            condition: Instruction.Index,
            then_instructions_list: Instruction.List,
            else_instructions_list: ?Instruction.List,
        },
        branch_wip: struct {
            condition: Instruction.Index,
            then_wip: MirBuilder.Wip.Index,
            else_wip: ?MirBuilder.Wip.Index,
        },

        select: struct {
            condition: Instruction.Index,
            then_instruction: Instruction.Index,
            else_instruction: Instruction.Index,
        },

        scoped: struct {
            name: InternedSlice,
            // scope_index: Type.Index,
            index: u32,
            mutable: bool,
        },
        call: struct {
            callee: Global.Index,
            args_list: Instruction.List,
        },
        global_set: struct {
            global: Global.Index,
            value: Instruction.Index,
        },
        global_get: struct {
            global: Global.Index,
        },
        store: struct {
            pointer: Instruction.Index,
            value: Instruction.Index,
        },

        get_element_pointer: struct {
            pointer: Instruction.Index,
            index: Instruction.Index,
        },

        cast: struct {
            instruction: Instruction.Index,
            type: Type.Index,
        },

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
        typeof,
        constant,
        branch,
        alloc,
        store,
        load,
        get_element_pointer,

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

pub const Type = union(enum) {
    @"fn": Fn,
    optional: Optional,
    module: Module,
    array: TyArray,
    pointer: Pointer,
    slice: Slice,

    // these are not types by themselves but partials of other types
    global: Module.Decl,
    param: Fn.Param,
    pub const List = InternedLists.Range;
    pub const INDEX_START: u32 = std.meta.tags(@This().Index).len;
    pub const Index = enum(u32) {
        unknown,

        param,

        boolean,
        number,
        string,
        void,

        i8,
        i16,
        i32,
        i64,
        i128,
        i256,
        u8,
        u16,
        u32,
        u64,
        u128,
        u256,
        usize,

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
                .u8 => return .type_u8,
                .u16 => return .type_u16,
                .u32 => return .type_u32,
                .u64 => return .type_u64,
                .u128 => return .type_u128,
                .u256 => return .type_u256,
                .usize => return .type_usize,
                .i8 => return .type_i8,
                .i16 => return .type_i16,
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

    pub const Pointer = struct {
        child: Type.Index,
    };
    pub const Slice = struct {
        child: Type.Index,
        len: u32,
    };
    pub const TyArray = struct {
        type: Type.Index,
        size: u32,
    };
    pub const Fn = struct {
        name: InternedSlice,
        params_list: Type.List,
        return_type: Type.Index,
        pub const Param = struct {
            name: InternedSlice,
            type: Type.Index,
        };
    };
    pub const Module = struct {
        fields: Type.List,
        decls: Type.List,
        alignment: u32,

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
    array: ArrayValue,

    const ArrayValue = struct {
        type: Type.Index,
        values: Value.List,
    };

    pub const List = InternedLists.Range;
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
        type_i8,
        type_i16,
        type_i32,
        type_i64,
        type_i128,
        type_i256,

        type_u8,
        type_u16,
        type_u32,
        type_u64,
        type_u128,
        type_u256,
        type_usize,
        type_f32,
        type_f64,
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
            if (self.toInt()) |index| {
                std.debug.panic("not implemented: toType: {d}", .{index});
            }
            switch (self) {
                .type_number => return .number,
                .type_i8 => return .i8,
                .type_i16 => return .i16,
                .type_i32 => return .i32,
                .type_i64 => return .i64,
                .type_i128 => return .i128,
                .type_i256 => return .i256,
                .type_u8 => return .u8,
                .type_u16 => return .u16,
                .type_u32 => return .u32,
                .type_u64 => return .u64,
                .type_u128 => return .u128,
                .type_u256 => return .u256,
                .type_usize => return .usize,
                .type_f32 => return .f32,
                .type_f64 => return .f64,
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
pub fn formatValue(self: *Self, writer: std.io.AnyWriter, value_index: Value.Index, depth: usize) std.io.AnyWriter.Error!void {
    _ = depth; // autofix
    if (value_index.toInt()) |index| {
        try writer.print("[{}]", .{self.values.items[index]});
    } else {
        try writer.print("[{s}]", .{@tagName(value_index)});
    }
}

// pub fn formatInstruction(self: *Self, writer: std.io.AnyWriter, inst_index: Instruction.Index, depth: usize) std.io.AnyWriter.Error!void {
//     const instruction: Instruction = self.instructions.items[inst_index];
//     const tag_name = @tagName(instruction.data);
//     const data: Instruction.Data = instruction.data;
//     try fmt.writeIndent(writer, depth, .{});
//     try writer.print("{s: <4}", .{tag_name[0..@min(tag_name.len, 4)]});
//     try writer.print("[{d: >3}]", .{inst_index});
//     try writer.print("{s: <3}", .{if (instruction.liveness == 0) "!" else ""});
//     try writer.print("{s}", .{@tagName(instruction.op)});
//     switch (data) {
//         .scoped => |scoped| {
//             // try self.dumpType(writer, instruction.type, depth + 1);
//             try writer.print("('{s}', index = {d})", .{ self.strings.getSlice(scoped.name), scoped.index });
//         },
//         .bin_op => |bin_op| {
//             try writer.print("(%{d}, %{d})", .{ bin_op.lhs, bin_op.rhs });
//         },
//         .instruction => |instruction_index| {
//             try writer.print("(#{d})", .{instruction_index});
//         },

//         .type => |type_index| {
//             try self.formatType(writer, type_index, depth + 1);
//         },
//         .void => {},
//         .call => |call| {
//             var iter = self.lists.iterList(call.args_list);
//             try writer.print("(#{?d}) with (", .{call.callee.toInt()});
//             var first = true;
//             while (iter.next()) |arg_id| {
//                 if (!first) {
//                     try writer.writeAll(", ");
//                 }
//                 first = false;
//                 try writer.print("#{d}", .{arg_id});
//             }
//             try writer.writeAll(")");
//         },
//         .branch => |branch| {
//             try writer.print("(#{d}) then:\n", .{branch.condition});
//             var then_iter = self.lists.iterList(branch.then_instructions_list);
//             while (then_iter.next()) |then_inst_index| {
//                 try self.formatInstruction(writer, then_inst_index, depth + 1);
//             }
//             if (branch.else_instructions_list) |else_body| {
//                 try writer.writeAll(" else:\n");
//                 var else_iter = self.lists.iterList(else_body);
//                 while (else_iter.next()) |else_inst_index| {
//                     try self.formatInstruction(writer, else_inst_index, depth + 1);
//                 }
//             }
//             return;
//         },

//         .loop => |loop| {
//             try writer.writeAll("\n");
//             var iter = self.lists.iterList(loop.instructions_list);
//             while (iter.next()) |loop_inst_index| {
//                 try self.formatInstruction(writer, loop_inst_index, depth + 1);
//                 // try writer.writeAll("\n");
//             }
//             return;
//         },
//         .global_get => |global_get| {
//             try writer.print("(#{d})", .{global_get.global});
//         },
//         .global_set => |global_set| {
//             try writer.print("(#{d})", .{global_set.global});
//         },

//         // .store => |store| {
//         //     try writer.print("(#{d})", .{store.pointer});
//         // },
//         else => {
//             try writer.writeAll("(TODO)");
//         },
//     }

//     try writer.writeAll(": ");

//     try self.formatType(writer, instruction.type, depth + 1);
//     try writer.writeAll(" -> ");

//     try self.formatValue(writer, instruction.value, depth + 1);
//     try writer.writeAll("\n");
// }
pub fn formatType(self: *Self, writer: std.io.AnyWriter, type_index: Type.Index, depth: usize) !void {
    if (type_index.toInt()) |index| {
        const ty: Type = self.types.items[index];
        switch (ty) {
            .@"fn" => |fn_ty| {
                var iter = self.lists.iterList(fn_ty.params);
                try writer.writeAll("fn(");
                var i: usize = 0;
                while (iter.next()) |param_index| {
                    if (i > 0) try writer.writeAll(", ");
                    try self.formatType(writer, Type.Index.asTypeIndex(param_index), depth + 1);
                    // const param_type: Type.x = self.types.items[param_index];
                    // try formatType(self, writer, param_def.param.type);
                    i += 1;
                }
                try writer.writeAll(") -> ");
                try self.formatType(writer, fn_ty.return_type, depth + 1);

                // if (fn_ty.init_block) |body_index| {
                //     try writer.writeAll("\n");
                //     // try writer.writeAllIndented("body:\n");
                //     try formatType(self, writer, body_index);
                // }
                // if (fn_ty.body) |body_index| {
                //     var iter_instructions_dep = self.lists.iterList(body_index);
                //     while (iter_instructions_dep.next()) |inst_index| {
                //         try formatInstruction(self, writer, Inst.Index.asTypeIndex(inst_index));
                //     }
                //     // try formatType(self, writer, Type.Index.asTypeIndex(body_index));
                // }
            },

            .module => |mod| {
                var iter = self.lists.iterList(mod.decls);
                while (iter.next()) |decl_index| {
                    try self.formatType(writer, Type.Index.asTypeIndex(decl_index), depth + 1);
                    // const decl = self.types.items[decl_index];

                    // try writer.writeAll(@tagName(decl.type));
                    // try writer.writeAll(" ");
                    // try writer.writeAll(self.strings.getSlice(decl.name));
                    // try writer.writeAll(";\n");
                    // try writer.writeAll("\n");
                }
            },

            .param => |param| {
                try writer.writeAll(self.strings.getSlice(param.name));
                try writer.writeAll(": ");
                try self.formatType(writer, param.type, depth + 1);
            },

            else => {
                try writer.writeAll(@tagName(ty));
            },
        }
        return;
    }

    try writer.writeAll(@tagName(type_index));
}
pub const Global = struct {
    name: InternedSlice,
    type: Type.Index,
    value: Value.Index,
    init: ?Instruction.List,
    pub const Index = u32;
};

const fmt = @import("./format_utils.zig");
const FormatOptions = struct {
    color: bool = true,
};

pub fn formatGlobal(self: *Self, writer: std.io.AnyWriter, global_index: Global.Index, options: FormatOptions, tree_writer: *TreeWriter) !void {
    const global: Global = self.globals.items[global_index];

    try writer.print("[{d}] @", .{global_index});
    try writer.writeAll(self.strings.getSlice(global.name));
    try writer.writeAll(": ");

    // try fmt.formatType(self, writer, global.type);
    // try self.formatType(writer, global.type, 0);
    const format_context = FormatInstContext{
        .allocator = self.allocator,
        .instructions = self.instructions.items,
        .types = self.types.items,
        .values = self.values.items,
        .lists = &self.interned_lists,
        .writer = writer,
        .strings = &self.strings,
    };
    try formatTypeShort(format_context, writer, global.type, options);
    try writer.writeAll(" = ");
    try formatValueShort(format_context, writer, global.value, options);
    // try self.formatValue(writer, global.value, 0);
    try writer.writeAll("\n");

    try tree_writer.pushDirLine();
    if (global.init) |init_block| {
        const init_block_list = self.interned_lists.getSlice(init_block);

        for (init_block_list, 0..) |inst_index, i| {
            try tree_writer.writeIndent(true, i == init_block_list.len - 1);
            // try self.formatInstruction(writer, inst_index, 1);

            try formatInst(FormatInstContext{
                .allocator = self.allocator,
                .instructions = self.instructions.items,
                .types = self.types.items,
                .values = self.values.items,
                .lists = &self.interned_lists,
                .writer = writer,
                .strings = &self.strings,
            }, inst_index, tree_writer, options);
        }
    }
    try tree_writer.pop();
}
pub fn formatMir(self: *Self, writer: std.io.AnyWriter, options: FormatOptions) !void {
    try writer.writeAll(";; MIR\n");
    try writer.print(";; {d} instructions\n", .{self.instructions.items.len});
    try writer.print(";; {d} types\n", .{self.types.items.len});
    try writer.print(";; {d} values\n", .{self.values.items.len});
    try writer.print(";; {d} lists\n", .{self.interned_lists.interned_map.count()});
    try writer.print("\n", .{});
    try writer.print("Globals ({d}):\n", .{self.globals.items.len});
    var tree_writer = TreeWriter.init(writer);
    for (0..self.globals.items.len) |global_index| {
        try tree_writer.pushDirLine();
        try tree_writer.writeIndent(true, global_index == self.globals.items.len - 1);
        try self.formatGlobal(writer, @intCast(global_index), options, &tree_writer);
        try tree_writer.pop();
        try writer.writeAll("\n");
    }
}
pub fn format(self_: Self, comptime _: []const u8, options: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
    _ = fmt; // autofix
    _ = options; // autofix

    var self: *Self = @constCast(&self_);
    try self.formatMir(writer, FormatOptions{ .color = true });
}

const FormatInstContext = struct {
    allocator: std.mem.Allocator,
    instructions: []const Instruction,
    types: []const Type,
    values: []const Value,
    lists: *InternedLists,
    builder: ?*MirBuilder.Builder = null,
    writer: std.io.AnyWriter,
    strings: *InternedStrings,
};
pub fn formatTypeShort(context: FormatInstContext, writer: std.io.AnyWriter, type_index: Type.Index, options: FormatOptions) !void {
    const color_options = @import("./Color.zig").FormatColorOptions{ .color = options.color };
    if (type_index.toInt()) |index| {
        const ty: Type = context.types[index];
        try writer.print("({d})", .{index});
        switch (ty) {
            .param => |param| {
                try tw.blue_400.print(
                    writer,
                    "{s}<\"{s}\", ",
                    .{ @tagName(ty), context.strings.getSlice(param.name) },
                    color_options,
                );
                try formatTypeShort(context, writer, param.type, options);
                try tw.blue_400.write(writer, ">", color_options);
            },
            .pointer => |pointer| {

                // try writer.print("*", .{});
                try tw.pink_400.print(writer, "*", .{}, color_options);
                try formatTypeShort(context, writer, pointer.child, options);
            },
            .array => |array| {
                try tw.emerald_400.print(writer, "{s}<", .{@tagName(ty)}, color_options);
                try formatTypeShort(context, writer, array.type, options);
                try tw.emerald_400.print(writer, ",{d}>", .{array.size}, color_options);
            },
            .@"fn" => |fn_ty| {
                try tw.fuchsia_200.print(writer, "{s}(", .{@tagName(ty)}, color_options);
                const params_list = context.lists.getSlice(fn_ty.params_list);
                var i: usize = 0;
                for (params_list) |param_index| {
                    if (i > 0) try writer.writeAll(", ");
                    const param_type = context.types[Type.Index.asTypeIndex(param_index).toInt().?];

                    try writer.print("{s}: ", .{context.strings.getSlice(param_type.param.name)});
                    try formatTypeShort(context, writer, param_type.param.type, options);
                    i += 1;
                }
                try tw.fuchsia_200.print(writer, ")", .{}, color_options);

                try tw.neutral_400.print(writer, " -> ", .{}, color_options);
                try formatTypeShort(context, writer, fn_ty.return_type, options);
            },

            else => {
                try tw.cyan_400.print(writer, "{s}", .{@tagName(ty)}, color_options);
            },
        }
    } else {
        switch (type_index) {
            .f32, .f64 => {
                try tw.violet_400.print(writer, "{s}", .{@tagName(type_index)}, color_options);
            },
            .u8, .u16, .u32, .u64, .u128, .u256, .usize => {
                try tw.emerald_400.print(writer, "{s}", .{@tagName(type_index)}, color_options);
            },
            .i8, .i16, .i32, .i64, .i128 => {
                try tw.rose_400.print(writer, "{s}", .{@tagName(type_index)}, color_options);
            },
            else => {
                try tw.amber_400.print(writer, "{s}", .{@tagName(type_index)}, color_options);
            },
        }
    }
}

pub fn formatValueShort(context: FormatInstContext, writer: std.io.AnyWriter, value_index: Value.Index, options: FormatOptions) !void {
    const color_options = @import("./Color.zig").FormatColorOptions{ .color = options.color };
    if (value_index.toInt()) |index| {
        if (index >= context.values.len) {
            try tw.red_400.print(writer, "VALUE_NOT_FOUND {d}", .{value_index}, color_options);
            return;
        }
        const value: Value = context.values[index];
        // try tw.neutral_400.print(writer, "{s}", .{@tagName(value)}, .{});
        switch (value) {
            .integer => {
                try tw.emerald_400.print(writer, "{d} int", .{value.integer}, color_options);
            },
            .float => {
                try tw.violet_400.print(writer, "{d} float", .{value.float}, color_options);
            },
            .big_integer => {
                try tw.rose_400.print(writer, "{d} big_integer", .{value.big_integer}, color_options);
            },
            .type => {
                try tw.blue_400.print(writer, "type(", .{}, color_options);
                try formatTypeShort(context, writer, value.type, options);
                try tw.blue_400.print(writer, ")", .{}, color_options);
            },

            else => {},
        }
    } else {
        // try tw.neutral_400.print(writer, "{s}", .{@tagName(value_index)}, .{});
        switch (value_index) {
            .runtime => {
                try tw.neutral_400.print(writer, "[runtime]", .{}, color_options);
            },

            else => {
                try tw.amber_400.print(writer, "{s}", .{@tagName(value_index)}, color_options);
            },
        }
    }
}
// \x1b[31m
// \x1b[0m
fn charsWithoutEscapeSeq(str: []const u8) usize {
    var i: usize = 0;
    var count: usize = 0;
    while (i < str.len) {
        switch (str[i]) {
            '\x1b' => {
                while (str[i] != 'm') {
                    i += 1;
                }
                i += 1;
            },
            else => {
                i += 1;
                count += 1;
            },
        }
    }
    return count;
}
pub fn formatInst(context: FormatInstContext, inst_index: Instruction.Index, tree_writer: *TreeWriter, options: FormatOptions) !void {
    try fmt.writeIndent(context.writer, 0, .{ .rainbow = options.color });
    const writer = context.writer;
    const instruction = context.instructions[inst_index];
    const color_options = @import("./Color.zig").FormatColorOptions{ .color = options.color };
    var buf = try std.ArrayList(u8).initCapacity(context.allocator, 1024);
    defer buf.deinit();
    const buf_writer = buf.writer().any();
    try buf_writer.print("%{d}:{s}", .{
        inst_index,
        if (instruction.liveness == 0) "!" else "",
    });
    try writer.print("{s: <5} ", .{buf.items});
    buf.clearRetainingCapacity();
    // try writer.print("{s}", .{if (instruction.liveness == 0) "!: " else ":  "});

    // buf.clear();

    try formatTypeShort(context, buf_writer, instruction.type, options);
    // buf.writer().writeAll(" = ");
    try buf_writer.print(" = ", .{});
    // try writer.print(" {s} = ", .{buf.slice()});

    // buf.clear();

    var print_value = true;
    const data: Instruction.Data = instruction.data;
    switch (data) {
        .scoped => |scoped| {
            try tw.neutral_200.print(buf_writer, "local, {s}, index: {d}", .{ if (scoped.mutable) "mut" else "const", scoped.index }, color_options);
        },
        .global_get => |global_get| {
            try tw.neutral_200.print(buf_writer, "global_get(%{d})", .{global_get.global}, color_options);
        },
        .global_set => |global_set| {
            try tw.neutral_200.print(buf_writer, "global_set(%{d}, %{d})", .{ global_set.global, global_set.value }, color_options);
        },
        .instruction => |instruction_data| {
            try tw.neutral_200.print(buf_writer, "{s}(%{d})", .{ @tagName(instruction.op), instruction_data }, color_options);
        },
        .bin_op => |bin_op| {
            try tw.neutral_200.print(buf_writer, "{s}(%{d}, %{d})", .{ @tagName(instruction.op), bin_op.lhs, bin_op.rhs }, color_options);
        },
        .store => |store| {
            try tw.neutral_200.print(buf_writer, "store(%{d}, %{d})", .{ store.pointer, store.value }, color_options);
        },

        .get_element_pointer => |get_element_pointer| {
            try tw.neutral_200.print(buf_writer, "get_el_pointer(%{d}, %{d})", .{ get_element_pointer.pointer, get_element_pointer.index }, color_options);
        },
        .call => |call| {
            try tw.neutral_200.print(buf_writer, "call(%{d}, args = [", .{call.callee}, color_options);
            const args_list = context.lists.getSlice(call.args_list);

            var first = true;
            for (args_list) |arg_index| {
                if (!first) {
                    try buf_writer.print(", ", .{});
                }
                first = false;
                try buf_writer.print("%{d}", .{arg_index});
            }
            try tw.neutral_200.print(buf_writer, "])", .{}, color_options);
        },
        .type => |ty| {
            try tw.neutral_200.print(buf_writer, "{s}(", .{@tagName(instruction.op)}, color_options);
            try formatTypeShort(context, buf_writer, ty, options);
            try tw.neutral_200.print(buf_writer, ")", .{}, color_options);
        },
        .value => |value| {
            try tw.neutral_200.print(buf_writer, "{s}(", .{@tagName(instruction.op)}, color_options);
            try formatValueShort(context, buf_writer, value, options);
            try tw.neutral_200.print(buf_writer, ")", .{}, color_options);
        },
        .cast => |cast| {
            try tw.neutral_200.print(buf_writer, "cast(%{d}, ", .{cast.instruction}, color_options);
            try formatTypeShort(context, buf_writer, cast.type, options);
            try tw.neutral_200.print(buf_writer, ")", .{}, color_options);
        },
        .branch => |branch| {
            print_value = false;

            try tw.neutral_200.print(buf_writer, "if (%{d}) then:\n", .{branch.condition}, color_options);
            // try tree_writer.pushBlank();
            if (context.builder != null) {
                // try formatWipInsts(context, buf_writer, branch.then_instructions_list, depth + 1);
            } else {
                // try tree_writer.writeIndentTo(buf_writer, false, branch.else_instructions_list == null);
                // try buf_writer.writeAll("then:\n");
                try tree_writer.pushDirLine();
                const then_instructions_list = context.lists.getSlice(branch.then_instructions_list);
                for (then_instructions_list, 0..) |child_inst_index, i| {
                    _ = i; // autofix
                    try tree_writer.writeIndentTo(buf_writer, true, false);
                    try formatInst(.{
                        .allocator = context.allocator,
                        .instructions = context.instructions,
                        .types = context.types,
                        .values = context.values,
                        .lists = context.lists,
                        .builder = context.builder,
                        .writer = buf_writer,
                        .strings = context.strings,
                    }, child_inst_index, tree_writer, options);
                }
                try tree_writer.writeIndentTo(buf_writer, true, true);
                try tw.neutral_200.print(buf_writer, "end if;", .{}, color_options);
                try tree_writer.pop();
            }

            if (branch.else_instructions_list) |else_instructions_list| {
                try tree_writer.writeIndentTo(buf_writer, true, true);
                try tw.neutral_200.print(buf_writer, "else:\n", .{}, color_options);

                if (context.builder != null) {
                    // try formatWipInsts(context, buf_writer, else_instructions_list, depth + 1);
                } else {
                    const else_inst_list = context.lists.getSlice(else_instructions_list);

                    for (else_inst_list, 0..) |child_inst_index, i| {
                        try tree_writer.writeIndentTo(buf_writer, true, i == else_inst_list.len - 1);
                        try formatInst(.{
                            .allocator = context.allocator,
                            .instructions = context.instructions,
                            .types = context.types,
                            .values = context.values,
                            .lists = context.lists,
                            .builder = context.builder,
                            .writer = buf_writer,
                            .strings = context.strings,
                        }, child_inst_index, tree_writer, options);
                    }
                }
            }
            // try tree_writer.writeIndentTo(buf_writer, true, true);
            // try tw.neutral_200.print(buf_writer, "end if;\n", .{}, color_options);
            // try tree_writer.pop();
            // try fmt.writeIndent(buf_writer, 0, .{ .rainbow = options.color });

            // try buf_writer.writeByteNTimes(' ', 26);
            // try tw.neutral_200.print(buf_writer, "end if;\n", .{}, color_options);
            // try tree_writer.pop();
        },
        .loop => |loop| {
            print_value = false;
            try tw.neutral_200.print(buf_writer, "loop:\n", .{}, color_options);

            if (context.builder != null) {
                // try formatWipInsts(context, buf_writer, loop.instructions_list, depth + 1);
            } else {
                const instructions_list = context.lists.getSlice(loop.instructions_list);
                try tree_writer.pushDirLine();
                for (instructions_list, 0..) |child_inst_index, i| {
                    _ = i; // autofix
                    // try tree_writer.writeIndent(true, i == instructions_list.len - 1);
                    try tree_writer.writeIndentTo(buf_writer, true, false);
                    try formatInst(.{
                        .allocator = context.allocator,
                        .instructions = context.instructions,
                        .types = context.types,
                        .values = context.values,
                        .lists = context.lists,
                        .builder = context.builder,
                        .writer = buf_writer,
                        .strings = context.strings,
                    }, child_inst_index, tree_writer, options);
                }
                try tree_writer.writeIndentTo(buf_writer, true, true);
                try tw.neutral_200.print(buf_writer, "end loop;", .{}, color_options);
                try tree_writer.pop();
            }

            // try fmt.writeIndent(buf_writer, 0, .{ .rainbow = options.color });
            // try buf_writer.writeByteNTimes(' ', 26);
        },
        .br => |br| {
            try tw.neutral_200.print(buf_writer, "br(%{d}, ", .{br.instruction}, color_options);
            try formatValueShort(context, buf_writer, br.value, options);
            try tw.neutral_200.print(buf_writer, ")", .{}, color_options);
        },
        .alloc => |alloc| {
            try tw.neutral_200.print(buf_writer, "{s}alloc(", .{if (alloc.mutable) "mut " else ""}, color_options);
            try formatTypeShort(context, buf_writer, alloc.type, options);
            try tw.neutral_200.print(buf_writer, ")", .{}, color_options);
        },
        else => {
            try tw.neutral_400.print(buf_writer, "{s}", .{@tagName(instruction.op)}, color_options);
        },
    }
    const slice = buf.items;
    try writer.print("{s}", .{slice});
    if (print_value) {
        const slice_len = charsWithoutEscapeSeq(slice);
        try writer.writeByteNTimes(' ', if (slice_len <= 50) 50 - slice_len else 0);

        buf.clearRetainingCapacity();
        try formatValueShort(context, buf_writer, instruction.value, options);
        try writer.print("; {s}", .{buf.items});
        try tw.neutral_400.print(writer, " ~ {s}", .{@tagName(instruction.data)[0..@min(@tagName(instruction.data).len, 12)]}, color_options);
        // try writer.print("{s}", .{@tagName(instruction.op)});
    }

    try writer.writeAll("\n");
}

pub fn formatWipInsts(parent_context: FormatInstContext, buf_writer: std.io.AnyWriter, wip_index: MirBuilder.Wip.Index, depth: usize) Logger.Error!void {
    const builder = parent_context.builder orelse unreachable;
    const wip = builder.getWip(wip_index);
    const wip_owner = builder.getWip(wip.data.block.owner);
    const child_context = FormatInstContext{
        .allocator = parent_context.allocator,
        .instructions = wip_owner.instructions.items,
        .types = parent_context.types,
        .values = parent_context.values,
        .lists = parent_context.lists,
        .builder = parent_context.builder,
        .writer = buf_writer,
        .strings = parent_context.strings,
    };
    for (wip.data.block.instructions.items) |instruction_id| {
        try formatInst(child_context, instruction_id, depth);
    }
}
