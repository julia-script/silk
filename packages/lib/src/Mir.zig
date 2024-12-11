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
const Hir = @import("Hir.zig");
const ErrorManager = @import("ErrorManager.zig");
const Logger = @import("Logger.zig");
const tw = Logger.tw;

lists: Lists = .{},
strings: InternedStrings,
globals: Array(Global) = .{},
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

pub fn build(allocator: std.mem.Allocator, hir: *Hir, errors: *ErrorManager) !Self {
    return try MirBuilder.build(allocator, hir, errors);
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
        value: Value.Index,
        type: Type.Index,
        void: void,

        loop: struct {
            instructions_list: Instruction.List,
        },
        block: struct {
            instructions_list: Instruction.List,
        },
        branch: struct {
            condition: Instruction.Index,
            then_instructions_list: Instruction.List,
            else_instructions_list: ?Instruction.List,
        },
        select: struct {
            condition: Instruction.Index,
            then_instruction: Instruction.Index,
            else_instruction: Instruction.Index,
        },
        if_expr: struct {
            cond: Instruction.Index,
            then_body: Type.Index,
            else_body: ?Type.Index,
        },

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

    // these are not types by themselves but partials of other types
    global: Module.Decl,
    param: Fn.Param,
    pub const List = usize;
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
    pub const TyArray = struct {
        type: Type.Index,
        size: u32,
    };
    pub const Fn = struct {
        name: InternedSlice,
        params: Type.List,
        return_type: Type.Index,
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

pub fn formatInstruction(self: *Self, writer: std.io.AnyWriter, inst_index: Instruction.Index, depth: usize) std.io.AnyWriter.Error!void {
    const instruction: Instruction = self.instructions.items[inst_index];
    const tag_name = @tagName(instruction.data);
    const data: Instruction.Data = instruction.data;
    try fmt.writeIndent(writer, depth, .{});
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

        .type => |type_index| {
            try self.formatType(writer, type_index, depth + 1);
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
        .branch => |branch| {
            try writer.print("(#{d}) then:\n", .{branch.condition});
            var then_iter = self.lists.iterList(branch.then_instructions_list);
            while (then_iter.next()) |then_inst_index| {
                try self.formatInstruction(writer, then_inst_index, depth + 1);
            }
            if (branch.else_instructions_list) |else_body| {
                try writer.writeAll(" else:\n");
                var else_iter = self.lists.iterList(else_body);
                while (else_iter.next()) |else_inst_index| {
                    try self.formatInstruction(writer, else_inst_index, depth + 1);
                }
            }
            return;
        },

        .loop => |loop| {
            try writer.writeAll("\n");
            var iter = self.lists.iterList(loop.instructions_list);
            while (iter.next()) |loop_inst_index| {
                try self.formatInstruction(writer, loop_inst_index, depth + 1);
                // try writer.writeAll("\n");
            }
            return;
        },
        .global_get => |global_get| {
            try writer.print("(#{d})", .{global_get.global});
        },
        .global_set => |global_set| {
            try writer.print("(#{d})", .{global_set.global});
        },

        // .store => |store| {
        //     try writer.print("(#{d})", .{store.pointer});
        // },
        else => {
            try writer.writeAll("(TODO)");
        },
    }

    try writer.writeAll(": ");

    try self.formatType(writer, instruction.type, depth + 1);
    try writer.writeAll(" -> ");

    try self.formatValue(writer, instruction.value, depth + 1);
    try writer.writeAll("\n");
}
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
pub fn formatGlobal(self: *Self, writer: std.io.AnyWriter, global_index: Global.Index) !void {
    const global: Global = self.globals.items[global_index];

    try writer.print("[{d}] @", .{global_index});
    try writer.writeAll(self.strings.getSlice(global.name));
    try writer.writeAll(": ");

    // try fmt.formatType(self, writer, global.type);
    // try self.formatType(writer, global.type, 0);
    const format_context = FormatInstContext{
        .instructions = self.instructions.items,
        .types = self.types.items,
        .values = self.values.items,
        .lists = &self.lists,
        .writer = writer,
        .strings = &self.strings,
    };
    try formatTypeShort(format_context, writer, global.type);
    try writer.writeAll(" = ");
    try formatValueShort(format_context, writer, global.value);
    // try self.formatValue(writer, global.value, 0);
    try writer.writeAll("\n");
    if (global.init) |init_block| {
        var iter = self.lists.iterList(init_block);
        while (iter.next()) |inst_index| {
            // try self.formatInstruction(writer, inst_index, 1);
            try formatInst(FormatInstContext{
                .instructions = self.instructions.items,
                .types = self.types.items,
                .values = self.values.items,
                .lists = &self.lists,
                .writer = writer,
                .strings = &self.strings,
            }, inst_index, 1);
        }
    }
}
pub fn format(self_: Self, comptime _: []const u8, options: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
    _ = fmt; // autofix
    _ = options; // autofix

    var self: *Self = @constCast(&self_);
    // const root_type = self.types.items[0];
    // var indented_writer = Logger.init(writer, "Mir");
    // try indented_writer.open("root_module", .{});
    // try self.formatType(&indented_writer, Type.RootIndex);
    for (0..self.globals.items.len) |global_index| {
        try self.formatGlobal(
            writer,
            @intCast(global_index),
        );
        try writer.writeAll("\n");
    }

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

const FormatInstContext = struct {
    instructions: []const Instruction,
    types: []const Type,
    values: []const Value,
    lists: *Lists,
    writer: std.io.AnyWriter,
    strings: *InternedStrings,
};
pub fn formatTypeShort(context: FormatInstContext, writer: std.io.AnyWriter, type_index: Type.Index) !void {
    if (type_index.toInt()) |index| {
        const ty: Type = context.types[index];
        switch (ty) {
            .param => |param| {
                try tw.blue_400.print(
                    writer,
                    "{s}<\"{s}\", ",
                    .{ @tagName(ty), context.strings.getSlice(param.name) },
                    .{},
                );
                try formatTypeShort(context, writer, param.type);
                try tw.blue_400.write(writer, ">", .{});
            },
            .pointer => |pointer| {

                // try writer.print("*", .{});
                try tw.pink_400.print(writer, "*", .{}, .{});
                try formatTypeShort(context, writer, pointer.child);
            },
            .array => |array| {
                try tw.emerald_400.print(writer, "{s}<", .{@tagName(ty)}, .{});
                try formatTypeShort(context, writer, array.type);
                try tw.emerald_400.print(writer, ",{d}>", .{array.size}, .{});
            },
            .@"fn" => |fn_ty| {
                try tw.fuchsia_200.print(writer, "{s}(", .{@tagName(ty)}, .{});
                var iter = context.lists.iterList(fn_ty.params);
                var i: usize = 0;
                while (iter.next()) |param_index| {
                    if (i > 0) try writer.writeAll(", ");
                    const param_type = context.types[Type.Index.asTypeIndex(param_index).toInt().?];

                    try writer.print("{s}: ", .{context.strings.getSlice(param_type.param.name)});
                    try formatTypeShort(context, writer, param_type.param.type);
                    i += 1;
                }
                try tw.fuchsia_200.print(writer, ")", .{}, .{});

                try tw.neutral_400.print(writer, " -> ", .{}, .{});
                try formatTypeShort(context, writer, fn_ty.return_type);
            },

            else => {
                try tw.cyan_400.print(writer, "{s}", .{@tagName(ty)}, .{});
            },
        }
    } else {
        switch (type_index) {
            .f32, .f64 => {
                try tw.violet_400.print(writer, "{s}", .{@tagName(type_index)}, .{});
            },
            .u8, .u16, .u32, .u64, .u128, .u256, .usize => {
                try tw.emerald_400.print(writer, "{s}", .{@tagName(type_index)}, .{});
            },
            .i8, .i16, .i32, .i64, .i128 => {
                try tw.rose_400.print(writer, "{s}", .{@tagName(type_index)}, .{});
            },
            else => {
                try tw.amber_400.print(writer, "{s}", .{@tagName(type_index)}, .{});
            },
        }
    }
}

pub fn formatValueShort(context: FormatInstContext, writer: std.io.AnyWriter, value_index: Value.Index) !void {
    if (value_index.toInt()) |index| {
        const value: Value = context.values[index];
        // try tw.neutral_400.print(writer, "{s}", .{@tagName(value)}, .{});
        switch (value) {
            .integer => {
                try tw.emerald_400.print(writer, "{d} int", .{value.integer}, .{});
            },
            .float => {
                try tw.violet_400.print(writer, "{d} float", .{value.float}, .{});
            },
            .big_integer => {
                try tw.rose_400.print(writer, "{d} big_integer", .{value.big_integer}, .{});
            },
            .type => {
                try tw.blue_400.print(writer, "type(", .{}, .{});
                try formatTypeShort(context, writer, value.type);
                try tw.blue_400.print(writer, ")", .{}, .{});
            },

            else => {},
        }
    } else {
        // try tw.neutral_400.print(writer, "{s}", .{@tagName(value_index)}, .{});
        switch (value_index) {
            .runtime => {
                try tw.neutral_400.print(writer, "[runtime]", .{}, .{});
            },

            else => {
                try tw.amber_400.print(writer, "{s}", .{@tagName(value_index)}, .{});
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
pub fn formatInst(context: FormatInstContext, inst_index: Instruction.Index, depth: usize) !void {
    try fmt.writeIndent(context.writer, depth, .{});
    const writer = context.writer;
    const instruction = context.instructions[inst_index];
    try tw.neutral_400.print(writer, "{s: >12}", .{@tagName(instruction.data)[0..@min(@tagName(instruction.data).len, 12)]}, .{});
    var buf = try std.BoundedArray(u8, 1024).init(0);
    const buf_writer = buf.writer().any();
    try buf_writer.print("{s}%{d}", .{ if (instruction.liveness == 0) "!" else "", inst_index });
    try writer.print("{s: >5}: ", .{buf.slice()});
    buf.clear();
    // try writer.print("{s}", .{if (instruction.liveness == 0) "!: " else ":  "});

    // buf.clear();

    try formatTypeShort(context, buf_writer, instruction.type);
    // buf.writer().writeAll(" = ");
    try buf_writer.print(" = ", .{});
    // try writer.print(" {s} = ", .{buf.slice()});

    // buf.clear();

    const data: Instruction.Data = instruction.data;
    switch (data) {
        .scoped => |scoped| {
            try tw.neutral_200.print(buf_writer, "local, {s}, index: {d}", .{ if (scoped.mutable) "mut" else "const", scoped.index }, .{});
        },
        .global_get => |global_get| {
            try tw.neutral_200.print(buf_writer, "global_get(%{d})", .{global_get.global}, .{});
        },
        .global_set => |global_set| {
            try tw.neutral_200.print(buf_writer, "global_set(%{d}, %{d})", .{ global_set.global, global_set.value }, .{});
        },
        .instruction => |instruction_data| {
            try tw.neutral_200.print(buf_writer, "{s}(%{d})", .{ @tagName(instruction.op), instruction_data }, .{});
        },
        .bin_op => |bin_op| {
            try tw.neutral_200.print(buf_writer, "{s}(%{d}, %{d})", .{ @tagName(instruction.op), bin_op.lhs, bin_op.rhs }, .{});
        },
        .store => |store| {
            try tw.neutral_200.print(buf_writer, "store(%{d}, %{d})", .{ store.pointer, store.value }, .{});
        },

        .get_element_pointer => |get_element_pointer| {
            try tw.neutral_200.print(buf_writer, "get_el_pointer(%{d}, %{d})", .{ get_element_pointer.pointer, get_element_pointer.index }, .{});
        },
        .call => |call| {
            try tw.neutral_200.print(buf_writer, "call(%{d}, args = [", .{call.callee}, .{});
            var iter = context.lists.iterList(call.args_list);

            var first = true;
            while (iter.next()) |arg_index| {
                if (!first) {
                    try buf_writer.print(", ", .{});
                }
                first = false;
                try buf_writer.print("%{d}", .{arg_index});
            }
            try tw.neutral_200.print(buf_writer, "])", .{}, .{});
        },
        .type => |ty| {
            try tw.neutral_200.print(buf_writer, "{s}(", .{@tagName(instruction.op)}, .{});
            try formatTypeShort(context, buf_writer, ty);
            try tw.neutral_200.print(buf_writer, ")", .{}, .{});
        },
        .value => |value| {
            try tw.neutral_200.print(buf_writer, "{s}(", .{@tagName(instruction.op)}, .{});
            try formatValueShort(context, buf_writer, value);
            try tw.neutral_200.print(buf_writer, ")", .{}, .{});
        },
        .cast => |cast| {
            try tw.neutral_200.print(buf_writer, "cast(%{d}, ", .{cast.instruction}, .{});
            try formatTypeShort(context, buf_writer, cast.type);
            try tw.neutral_200.print(buf_writer, ")", .{}, .{});
        },
        // .alloc => {
        //     try tw.neutral_200.print(buf_writer, "alloc(", .{}, .{});
        //     try formatTypeShort(context, buf_writer, instruction.type);
        //     try tw.neutral_200.print(buf_writer, ")", .{}, .{});
        // },
        // .un_op => |un_op| {
        //     try tw.neutral_400.print(buf_writer, "{s}(%{d})", .{ @tagName(un_op.op), un_op.value }, .{});
        // },
        // .call => |call| {
        //     try tw.neutral_400.print(buf_writer, "{s}(%{d})", .{ @tagName(call.op), call.callee }, .{});
        // },
        else => {
            try tw.neutral_400.print(buf_writer, "{s}", .{@tagName(instruction.op)}, .{});
        },
    }
    const slice = buf.slice();
    const slice_len = charsWithoutEscapeSeq(slice);
    try writer.print("{s}", .{slice});
    try writer.writeByteNTimes(' ', if (slice_len <= 50) 50 - slice_len else 0);

    buf.clear();
    try formatValueShort(context, buf_writer, instruction.value);
    try writer.print("; {s}", .{buf.slice()});
    // try writer.print("{s}", .{@tagName(instruction.op)});

    try writer.writeAll("\n");
}
