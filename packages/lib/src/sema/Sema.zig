const std = @import("std");
const Array = std.ArrayListUnmanaged;
const ArrayHashMap = std.AutoArrayHashMapUnmanaged;
const Tracer = @import("../Tracer.zig");
pub const Builder = @import("./gen.zig").Builder;
const Hir = @import("../Hir.zig");
const Ast = @import("../Ast.zig");
const ErrorManager = @import("../ErrorManager.zig");
const Entity = @import("./gen.zig").Entity;
const InternedList = @import("../interned-lists.zig").InternedLists;
pub const Lists = InternedList(usize);
pub const Strings = InternedList(u8);
const activeTag = std.meta.activeTag;
const ComptimeMemory = @import("./ComptimeMemory.zig");

const Self = @This();

allocator: std.mem.Allocator,
memory: ComptimeMemory,
values: ArrayHashMap(u64, Value) = .{},
types: ArrayHashMap(u64, Type) = .{},

lists: Lists,
strings: Strings,
declarations: std.ArrayListUnmanaged(Declaration) = .{},
instructions: std.ArrayListUnmanaged(Instruction) = .{},
builder: Builder,

root: Strings.Range = Strings.Range.empty,
sources: std.AutoArrayHashMapUnmanaged(Strings.Range, Source) = .{},
errors_manager: *ErrorManager,
settings: Settings = .{},
pub const Settings = struct {
    // we can skip dead branches on compilation but keep them for debugging or LSP for example
    compute_dead_branches: bool = true,
};
pub fn init(allocator: std.mem.Allocator, error_manager: *ErrorManager, options: Builder.BuildOptions) !Self {
    _ = options; // autofix
    return Self{
        .allocator = allocator,
        .errors_manager = error_manager,
        .lists = Lists.init(allocator),
        .strings = Strings.init(allocator),
        .builder = undefined,
        .memory = undefined,
    };
}

pub fn makeRootSource(self: *Self, source: []const u8, path: []const u8) !Strings.Range {
    self.builder = try Builder.build(self.allocator, self, self.errors_manager, .{});
    self.memory = ComptimeMemory.init(self.allocator, self);
    return try self.makeSource(source, path);
}

pub fn makeSource(self: *Self, source: []const u8, path: []const u8) !Strings.Range {
    const source_range = try self.strings.internSlice(path);
    const source_gop = try self.sources.getOrPut(self.allocator, source_range);
    if (source_gop.found_existing) {
        return error.SourceAlreadyExists;
    }

    source_gop.value_ptr.* = try Source.init(self.builder.allocator, self.errors_manager, source);
    return source_range;
}
pub fn compileAll(self: *Self, source: Strings.Range) !void {
    try self.builder.compileAll(source);
}

pub fn getSource(self: *Self, source: Strings.Range) *Source {
    return self.sources.getPtr(source) orelse std.debug.panic("source not found", .{});
}
pub fn getHir(self: *Self, source: Strings.Range) *Hir {
    return self.getSource(source).hir;
}
pub const Source = struct {
    id: u64,
    path: Strings.Range,
    source: []const u8,
    ast: *Ast,
    hir: *Hir,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, errors_manager: *ErrorManager, source: []const u8) !Source {
        const source_dupe = try allocator.dupe(u8, source);
        errdefer allocator.free(source_dupe);
        const ast = try allocator.create(Ast);
        errdefer allocator.destroy(ast);
        ast.* = try Ast.parse(
            allocator,
            errors_manager,
            source_dupe,
            .{},
        );
        const hir = try allocator.create(Hir);
        errdefer allocator.destroy(hir);
        hir.* = try Hir.build(allocator, ast, errors_manager, .{});
        try hir.format("", .{}, std.io.getStdErr().writer().any());
        return Source{
            .id = std.hash.Wyhash.hash(0, source_dupe),
            .path = Strings.Range.empty,
            .source = source_dupe,
            .allocator = allocator,
            .ast = ast,
            .hir = hir,
        };
    }
    pub fn deinit(self: *Source) void {
        self.ast.deinit();
        self.hir.deinit();
        self.allocator.free(self.source);
        self.allocator.destroy(self.ast);
        self.allocator.destroy(self.hir);
    }
};

pub fn deinit(self: *Self) void {
    self.builder.deinit();
    const sources = self.sources.values();
    for (sources) |*source| {
        source.deinit();
    }
    self.memory.deinit();
    self.sources.deinit(self.allocator);
    self.strings.deinit();
    self.lists.deinit();
    self.declarations.deinit(self.allocator);
    self.instructions.deinit(self.allocator);
    self.values.deinit(self.allocator);
    self.types.deinit(self.allocator);
}
pub const Type = struct {
    hash: u64,
    data: Data,
    size: u32 = 0,
    alignment: u32 = 0,

    pub const Key = union(enum) {
        simple: Simple,
        complex: usize,
        const SIMPLE_COUNT = std.meta.fields(Type.Simple).len;
        pub fn encode(self: Key) usize {
            return switch (self) {
                .simple => |s| @intFromEnum(s),
                .complex => |c| c + SIMPLE_COUNT,
            };
        }
        pub fn decode(self: usize) Key {
            return if (self < SIMPLE_COUNT)
                .{ .simple = @enumFromInt(self) }
            else
                .{ .complex = self - SIMPLE_COUNT };
        }
        // pub fn complexFromIndex(self: usize) Key {
        //     return .{ .complex = self + SIMPLE_COUNT };
        // }

        pub fn isEqual(self: Key, other: Key) bool {
            if (activeTag(self) != activeTag(other)) return false;
            return switch (self) {
                .simple => |s| s == other.simple,
                .complex => |c| c == other.complex,
            };
        }
        pub fn isEqualSimple(self: Key, other: Simple) bool {
            return switch (self) {
                .simple => |s| s == other,
                .complex => false,
            };
        }

        pub fn isNumeric(self: Key) bool {
            return switch (self) {
                .simple => |s| switch (s) {
                    .number, .usize => true,

                    inline else => |tag| comptime {
                        const tag_name = @tagName(tag);
                        if (tag_name[0] != 'i' and tag_name[0] != 'u' and tag_name[0] != 'f') {
                            return false;
                        }
                        if (tag_name.len < 2) return false;
                        return tag_name[1] >= '0' and tag_name[1] <= '9';
                    },
                },
                .complex => false,
            };
        }
    };
    pub const List = Lists.Range;
    pub const Simple = enum {
        unknown,
        type,
        infer,
        number,

        i8,
        i16,
        i32,
        i64,

        u8,
        u16,
        u32,
        u64,
        usize,

        f32,
        f64,

        bool,
        void,

        builtin_fn_as,
    };

    pub fn simple(self: Simple) Key {
        return .{ .simple = self };
    }
    pub fn complex(self: usize) Key {
        return .{ .complex = self };
    }

    pub fn geSimpletSize(key: Key) u32 {
        return switch (key) {
            .simple => |s| switch (s) {
                .bool => 1,
                .i8 => 1,
                .u8 => 1,
                .i16 => 2,
                .u16 => 2,
                .i32 => 4,
                .u32 => 4,
                .i64 => 8,
                .u64 => 8,
                .usize => 8,
                .f32 => 4,
                .f64 => 8,
                .void => 0,
                else => unreachable,
            },
            .complex => unreachable,
        };
    }

    pub const Data = union(enum) {
        // module: struct {
        //     entity: Entity.Key,
        //     struct_type: Type.Key,
        // },

        @"struct": struct {
            entity: Entity.Key,
            fields: Type.List,
        },
        struct_field: struct {
            type: Type.Key,
            offset: u32,
        },
        function: Function,
        array: struct {
            child: Type.Key,
            size: u32,
        },
        pointer: struct {
            child: Type.Key,
        },
        pub const Ent = struct {
            entity: Entity.Key,
        };
        pub const Function = struct {
            entity: Entity.Key,
            params: Type.List,
            ret: Type.Key,
        };
    };
};
pub const Value = struct {
    hash: u64,
    data: Data,
    pub const Data = union(enum) {
        integer: i64,
        float: f64,
        type: Type.Key,
        pointer: usize,
        comptime_pointer: usize,
        comptime_register: [8]u8,

        struct_instance: struct {
            struct_entity: Entity.Key,
            values: Value.List,
        },

        function: struct {
            type: Type.Key,
            init: ?Instruction.Index,
        },
        global: struct {
            type: Type.Key,
            value: Value.Key,
            init: ?Instruction.Index,
        },
        array_init: struct {
            items_list: Value.List,
            type: Type.Key,
        },
        type_init: struct {
            field_init_list: Value.List,
            type: Type.Key,
        },
        field_init: struct {
            field_name: Strings.Range,
            value_inst: Instruction.Index,
        },
    };

    pub const List = Lists.Range;
    pub const Key = union(enum) {
        simple: Simple,
        complex: usize,
        comptime_pointer: usize,

        pub const SIMPLE_COUNT = std.meta.fields(Value.Simple).len;
        pub fn complexFromIndex(self: usize) Key {
            return .{ .complex = self + SIMPLE_COUNT };
        }
        pub fn decode(self: usize) Key {
            return if (self < SIMPLE_COUNT)
                .{ .simple = @enumFromInt(self) }
            else
                .{ .complex = self - SIMPLE_COUNT };
        }

        pub fn encode(self: Key) usize {
            return switch (self) {
                .simple => |s| @intFromEnum(s),
                .complex => |c| c + SIMPLE_COUNT,
            };
        }
        pub fn isEqual(self: Key, other: Key) bool {
            return switch (self) {
                .simple => |s| s == other.simple,
                .complex => |c| c == other.complex,
            };
        }
        pub fn isEqualSimple(self: Key, other: Simple) bool {
            return switch (self) {
                .simple => |s| s == other,
                .complex => false,
                .comptime_pointer => false,
            };
        }
    };
    pub const Simple = enum {
        unknown,
        exec_time,
        runtime,
        true,
        false,
        undefined,
        nil,
        void,

        type_number,
        type_i8,
        type_i16,
        type_i32,
        type_i64,

        type_u8,
        type_u16,
        type_u32,
        type_u64,

        type_usize,

        type_f32,
        type_f64,

        type_bool,
        type_string,
        type_void,

        type_builtin_fn_as,
    };
    pub fn simple(self: Simple) Key {
        return .{ .simple = self };
    }
    pub fn complex(self: usize) Key {
        return .{ .complex = self };
    }
};
pub const TypedValue = struct {
    type: Type.Key,
    value: Value.Key,
};

pub const Instruction = struct {
    op: Op,
    type: Type.Key,
    value: Value.Key,
    data: Data,
    liveness: u8 = 1,

    pub const Data = union(enum) {
        void,
        // param: struct {
        //     name: Value.Key,
        //     value: Value.Key,
        // },
        param: struct {
            index: usize,
        },
        alloc: struct {
            type: Type.Key,
            mutable: bool,
        },

        bin_op: struct {
            lhs: Instruction.Index,
            rhs: Instruction.Index,
        },

        operand: Instruction.Index,
        maybe_operand: ?Instruction.Index,
        operand_payload: struct {
            operand: Instruction.Index,
            payload: Instruction.Index,
        },
        declaration: Declaration.Index,
        @"if": struct {
            condition: Instruction.Index,
            then_block: Instruction.Index,
            else_block: ?Instruction.Index,
        },
        select: struct {
            condition: Instruction.Index,
            then_block: Instruction.Index,
            else_block: Instruction.Index,
        },

        loop: struct {
            body_block: Instruction.Index,
        },

        block: struct {
            instructions_count: usize,
            is_comptime: bool,
        },
        get_element_pointer: struct {
            base: Instruction.Index,
            index: Instruction.Index,
        },
        fn_call: struct {
            // callee: Instruction.Index,
            callee_entity: Entity.Key,
            callee: Declaration.Index,
            args_list: Instruction.List,
        },
        array_init: struct {
            items_list: Instruction.List,
            type_inst: Instruction.Index,
        },
    };
    pub const InstRange = struct {
        start: Instruction.Index,
        len: usize,
    };

    pub const OperandValue = struct {
        operand: Instruction.Index,
        value: Instruction.Index,
    };

    pub const List = Lists.Range;

    pub const Op = enum {
        void, // noop
        param,
        constant,
        type,
        alloc,
        store,
        cast,
        loop,
        load,
        typeof,
        global_get,
        global_set,
        fn_call,
        array_init,
        type_init,
        field_init,

        param_get,
        param_set,

        block,

        gt,
        ge,
        lt,
        le,
        eq,
        ne,

        add,
        sub,
        mul,
        div,

        @"if",
        select,
        br,
        ret,
        get_element_pointer,

        get_builtin_fn_as,

        cast_promote,
        cast_demote,
        cast_extend,
        cast_truncate,
        cast_convert,
        cast_wrap,
        cast_reinterpret,

        max,
        min,
    };

    pub const Index = usize;
};

pub const Declaration = struct {
    name: Strings.Range,
    is_pub: bool,
    is_export: bool,
    type: Type.Key,
    value: Value.Key,
    pub const Index = usize;
};

const TreeWriter = @import("../TreeWriter.zig");

pub fn formatType(self: *Self, writer: std.io.AnyWriter, type_key: Type.Key) !void {
    if (self.builder.getType(type_key)) |ty| {
        switch (ty.data) {
            .pointer => |pointer| {
                try writer.writeAll("*");
                try self.formatType(writer, pointer.child);
            },
            .array => |array| {
                try writer.print("[{d}]", .{array.size});
                try self.formatType(writer, array.child);
            },

            // .module => |module| {
            //     try writer.print("mod{{ent{{{d}}}, ", .{module.entity});
            //     try self.formatType(writer, module.struct_type);
            //     try writer.writeAll("}");
            // },
            .struct_field => |struct_field| {
                try writer.writeAll("struct_field{");
                try self.formatType(writer, struct_field.type);
                try writer.writeAll("}");
            },
            .@"struct" => |struct_type| {
                try writer.writeAll("struct{");
                for (self.lists.getSlice(struct_type.fields), 0..) |field, i| {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }
                    const field_type = self.builder.getType(Type.Key.decode(field)) orelse unreachable;
                    try self.formatType(writer, field_type.data.struct_field.type);
                }
                try writer.writeAll("}");
            },

            else => {
                try writer.print("todo({s})", .{@tagName(ty.data)});
            },
        }
        return;
    }
    try writer.print("{s}", .{@tagName(type_key.simple)});
}
pub fn formatTypeLong(
    self: *Self,
    writer: std.io.AnyWriter,
    tree_writer: *TreeWriter,
    type_key: Type.Key,
) !void {
    if (self.builder.getType(type_key)) |ty| {
        switch (ty.data) {
            .@"struct" => |struct_type| {
                try writer.print("struct:\n", .{});
                try tree_writer.pushDirLine();
                try tree_writer.writeIndent(true, false);
                try writer.print("type: ", .{});
                try self.formatType(writer, type_key);
                try writer.print("\n", .{});

                try tree_writer.writeIndent(true, false);
                try writer.print("alignment: {d}\n", .{ty.alignment});

                try tree_writer.writeIndent(true, false);
                try writer.print("size: {d}\n", .{ty.size});

                try tree_writer.writeIndent(true, true);
                const slice = self.lists.getSlice(struct_type.fields);
                try writer.print("fields: {d}\n", .{slice.len});
                try tree_writer.pushDirLine();
                for (slice, 0..) |field, i| {
                    try tree_writer.writeIndent(true, i == slice.len - 1);
                    // if (i > 0) {
                    //     try writer.writeAll(" ");
                    // }
                    // try self.formatTypeLong(writer, tree_writer, Type.Key.decode(field));
                    const field_type = self.builder.getType(Type.Key.decode(field)) orelse unreachable;

                    try writer.print("[{d}]\n", .{i});
                    try tree_writer.pushDirLine();

                    try tree_writer.writeIndent(true, false);
                    try writer.print("alignment: {d}\n", .{field_type.alignment});

                    try tree_writer.writeIndent(true, false);
                    try writer.print("size: {d}\n", .{field_type.size});

                    try tree_writer.writeIndent(true, false);
                    try writer.print("offset: {d}\n", .{field_type.data.struct_field.offset});

                    try tree_writer.writeIndent(true, true);
                    try writer.writeAll("type: ");
                    try self.formatType(writer, field_type.data.struct_field.type);

                    try tree_writer.pop();
                    try writer.writeAll("\n");
                }

                try tree_writer.pop(); // fields
                try tree_writer.pop(); // struct
                return;
            },
            // .struct_field => |struct_field| {
            //     try writer.print("struct_field: ", .{});
            //     try self.formatTypeLong(writer, tree_writer, struct_field.type);
            // },
            else => {},
        }
    }
    try self.formatType(writer, type_key);
}
pub fn formatValue(self: *Self, writer: std.io.AnyWriter, value: Value.Key) !void {
    if (self.builder.getValue(value)) |val| {
        switch (val.data) {
            .float => |f| {
                try writer.print("float({d})", .{f});
            },
            .integer => |i| {
                try writer.print("int({d})", .{i});
            },
            .type => |type_key| {
                try writer.print("type(", .{});
                try self.formatType(writer, type_key);
                try writer.print(")", .{});
            },

            else => {
                try writer.print("todo({s})", .{@tagName(val.data)});
            },
        }
        return;
    }
    switch (value) {
        .comptime_pointer => |comptime_pointer| {
            try writer.print("comp_ptr({x})", .{comptime_pointer});
        },
        else => {
            const tag_name = @tagName(value.simple);
            if (std.mem.startsWith(u8, tag_name, "type_")) {
                try writer.print("type({s})", .{tag_name[5..]});
            } else {
                try writer.print("{s}", .{tag_name});
            }
        },
    }
}
const InstInput = union(enum) {
    instruction: Instruction,
    index: Instruction.Index,
};
pub fn formatInstruction(self: *Self, writer: std.io.AnyWriter, inst_input: InstInput) !void {
    var buf = try std.BoundedArray(u8, 1024).init(0);
    const buf_writer = buf.writer().any();
    const inst = switch (inst_input) {
        .instruction => |inst| blk: {
            if (inst.liveness == 0) try buf_writer.writeAll("!");
            try buf_writer.print("%i: ", .{});
            break :blk inst;
        },
        .index => |index| blk: {
            const inst = self.instructions.items[index];
            if (inst.liveness == 0) try buf_writer.writeAll("!");
            try buf_writer.print("%{d}: ", .{index});
            break :blk inst;
        },
    };
    switch (inst.data) {
        .@"if" => {
            try buf_writer.print("if (%{d}) then block(%{d})", .{
                inst.data.@"if".condition,
                inst.data.@"if".then_block,
            });
            if (inst.data.@"if".else_block) |else_block| {
                try buf_writer.print(" else block(%{d})", .{else_block});
            }
        },
        .block => {
            try buf_writer.print("block({d} insts)", .{
                inst.data.block.instructions_count,
            });
        },
        inline else => |data| {
            try self.formatType(buf_writer, inst.type);
            try buf_writer.print(" = .{s}", .{@tagName(inst.op)});
            const T = @TypeOf(data);
            switch (@typeInfo(T)) {
                .@"struct" => {
                    const fields = std.meta.fields(T);
                    inline for (fields, 0..) |field, j| {
                        _ = j; // autofix

                        try buf_writer.print(" {s}=", .{
                            field.name,
                        });
                        const value = @field(data, field.name);

                        if (comptime std.mem.eql(u8, field.name, "type")) {
                            try buf_writer.print("(", .{});
                            try self.formatType(buf_writer, value);
                            try buf_writer.print(")", .{});
                        } else if (comptime std.mem.endsWith(u8, field.name, "list")) {
                            const slice = self.lists.getSlice(value);
                            try buf_writer.writeAll("({");
                            for (slice, 0..) |item, k| {
                                if (k > 0) {
                                    try buf_writer.writeAll(", ");
                                }
                                // try self.formatValue(buf_writer, item);
                                try buf_writer.print("%{d}", .{item});
                            }
                            try buf_writer.writeAll("})");
                        } else if (field.type == Instruction.Index) {
                            try buf_writer.print("(%{d})", .{value});
                        } else {
                            try buf_writer.print("({any})", .{value});
                        }
                    }
                },
                else => {
                    const tag = @tagName(inst.data);
                    if (T == void) {} else if (T == Instruction.Index) {
                        try buf_writer.print(" {s}=(%{d})", .{ tag, data });
                    } else if (T == ?Instruction.Index) {
                        if (data) |value| {
                            try buf_writer.print(" {s}=(%{d})", .{ tag, value });
                        } else {
                            try buf_writer.print(" {s}=(null)", .{tag});
                        }
                    } else {
                        try buf_writer.print(" {s}=({any})", .{ tag, data });
                    }
                },
            }
        },
    }

    try writer.print("{s: <70}; ", .{buf.slice()});
    try self.formatValue(writer, inst.value);
}
pub fn formatInstructionRange(
    self: *Self,
    writer: std.io.AnyWriter,
    tree_writer: *TreeWriter,
    instructions: []Instruction,
    range: Instruction.InstRange,
) !void {
    var i: usize = range.start;
    var buf = try std.BoundedArray(u8, 1024).init(0);
    const buf_writer = buf.writer().any();
    const range_end = range.start + range.len - 1;
    while (i <= range_end) : (i += 1) {
        const inst = instructions[i];
        buf.clear();

        switch (inst.data) {
            .@"if" => |if_expr| {
                const index = i;
                {
                    const then_block = instructions[if_expr.then_block];
                    const inner_range: Instruction.InstRange = .{
                        .start = if_expr.then_block,
                        .len = then_block.data.block.instructions_count,
                    };
                    const last_inst = inner_range.start + inner_range.len - 1;
                    try tree_writer.writeIndent(
                        true,
                        last_inst == range_end,
                    );
                    if (inst.liveness == 0) {
                        try writer.writeAll("!");
                    }
                    try writer.print("%{d}: if (%{d}) then: [%{d}-%{d}]\n", .{
                        index,
                        if_expr.condition,
                        inner_range.start,
                        last_inst,
                    });
                    try tree_writer.pushDirLine();

                    try self.formatInstructionRange(
                        writer,
                        tree_writer,
                        instructions,
                        inner_range,
                    );
                    try tree_writer.pop();
                    i += then_block.data.block.instructions_count;
                }
                if (if_expr.else_block) |else_block_index| {
                    const else_block = instructions[else_block_index];
                    const inner_range: Instruction.InstRange = .{
                        .start = else_block_index,
                        .len = else_block.data.block.instructions_count,
                    };
                    const last_inst = inner_range.start + inner_range.len - 1;
                    try tree_writer.writeIndent(true, last_inst == range_end);
                    if (inst.liveness == 0) {
                        try writer.writeAll("!");
                    }
                    try writer.print("%{d} else [%{d}-%{d}]:\n", .{
                        index,
                        inner_range.start,
                        last_inst,
                    });
                    try tree_writer.pushDirLine();
                    try self.formatInstructionRange(
                        writer,
                        tree_writer,
                        instructions,
                        inner_range,
                    );
                    try tree_writer.pop();
                    i += else_block.data.block.instructions_count;
                }
            },
            .loop => |loop| {
                // const last_inst = loop.body_block;

                const then_block = instructions[loop.body_block];
                const inner_range: Instruction.InstRange = .{
                    .start = loop.body_block,
                    .len = then_block.data.block.instructions_count,
                };
                const last_inst = inner_range.start + inner_range.len - 1;
                try tree_writer.writeIndent(true, last_inst == range_end);
                if (inst.liveness == 0) {
                    try buf_writer.writeAll("!");
                }
                try writer.print("%{d} loop: [%{d}-%{d}]\n", .{
                    i,
                    inner_range.start,
                    last_inst,
                });
                try tree_writer.pushDirLine();
                try self.formatInstructionRange(
                    writer,
                    tree_writer,
                    instructions,
                    inner_range,
                );
                try tree_writer.pop();
                i += then_block.data.block.instructions_count;
            },
            .block => |block| {
                const inner_range: Instruction.InstRange = .{
                    .start = i + 1,
                    .len = block.instructions_count - 1,
                };
                const last_inst = inner_range.start + inner_range.len - 1;
                try tree_writer.writeIndentTo(buf_writer, true, last_inst == range_end);
                if (inst.liveness == 0) {
                    try buf_writer.writeAll("!");
                }
                try buf_writer.print("%{d}: ", .{i});
                try self.formatType(buf_writer, inst.type);
                try buf_writer.print(" = {s}.{s}: [%{d}-%{d}]", .{
                    if (block.is_comptime) "comptime " else "",
                    @tagName(inst.op),
                    inner_range.start,
                    last_inst,
                });
                try writer.print("{s: <70}; ", .{buf.slice()});
                try self.formatValue(writer, inst.value);
                try writer.print("\n", .{});

                try tree_writer.pushDirLine();
                try self.formatInstructionRange(
                    writer,
                    tree_writer,
                    instructions,
                    inner_range,
                );
                try tree_writer.pop();
                i += block.instructions_count;
            },
            inline else => |data| {
                try tree_writer.writeIndentTo(buf_writer, true, i == range_end);
                if (inst.liveness == 0) {
                    try buf_writer.writeAll("!");
                }
                try buf_writer.print("%{d}: ", .{i});
                try self.formatType(buf_writer, inst.type);
                try buf_writer.print(" = .{s}", .{@tagName(inst.op)});
                const T = @TypeOf(data);
                switch (@typeInfo(T)) {
                    .@"struct" => {
                        const fields = std.meta.fields(T);
                        inline for (fields, 0..) |field, j| {
                            _ = j; // autofix

                            try buf_writer.print(" {s}=", .{
                                field.name,
                            });
                            const value = @field(data, field.name);

                            if (comptime std.mem.eql(u8, field.name, "type")) {
                                try buf_writer.print("(", .{});
                                try self.formatType(buf_writer, value);
                                try buf_writer.print(")", .{});
                            } else if (comptime std.mem.endsWith(u8, field.name, "list")) {
                                const slice = self.lists.getSlice(value);
                                try buf_writer.writeAll("({");
                                for (slice, 0..) |item, k| {
                                    if (k > 0) {
                                        try buf_writer.writeAll(", ");
                                    }
                                    // try self.formatValue(buf_writer, item);
                                    try buf_writer.print("%{d}", .{item});
                                }
                                try buf_writer.writeAll("})");
                            } else if (field.type == Instruction.Index) {
                                try buf_writer.print("(%{d})", .{value});
                            } else {
                                try buf_writer.print("({any})", .{value});
                            }
                        }
                    },
                    else => {
                        const tag = @tagName(inst.data);
                        if (T == void) {} else if (T == Instruction.Index) {
                            try buf_writer.print(" {s}=(%{d})", .{ tag, data });
                        } else if (T == ?Instruction.Index) {
                            if (data) |value| {
                                try buf_writer.print(" {s}=(%{d})", .{ tag, value });
                            } else {
                                try buf_writer.print(" {s}=(null)", .{tag});
                            }
                        } else {
                            try buf_writer.print(" {s}=({any})", .{ tag, data });
                        }
                    },
                }
                try writer.print("{s: <70}; ", .{buf.slice()});
                try self.formatValue(writer, inst.value);
                try writer.print("\n", .{});
            },
        }
    }
}
pub fn formatDeclaration(self: *Self, writer: std.io.AnyWriter, declaration_index: Declaration.Index) !void {
    var tree_writer = TreeWriter.init(writer);
    const declaration = self.declarations.items[declaration_index];

    // try tree_writer.pushDirLine();
    const name = self.strings.getSlice(declaration.name);
    if (declaration.is_pub) {
        try writer.print("pub ", .{});
    }
    if (declaration.is_export) {
        try writer.print("export ", .{});
    }

    if (self.builder.getValue(declaration.value)) |value| {
        switch (value.data) {
            .function => |func| {
                const ty = self.builder.getType(func.type) orelse unreachable;
                try writer.print("fn @\"{s}\"", .{name});
                const params_iter = self.lists.getSlice(ty.data.function.params);
                try writer.print("(", .{});

                for (params_iter, 0..) |param, i| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try self.formatType(writer, Type.Key.decode(param));
                }

                try writer.print(") -> ", .{});
                try self.formatType(writer, ty.data.function.ret);
                try writer.print("\n", .{});
                const func_value = self.builder.getValue(declaration.value) orelse unreachable;
                if (func_value.data.function.init) |init_inst_index| {
                    try tree_writer.pushDirLine();
                    const init_inst = self.instructions.items[init_inst_index];
                    const count = init_inst.data.block.instructions_count;
                    // std.debug.panic("TODO: formatDeclaration {any}", .{range});
                    try self.formatInstructionRange(
                        writer,
                        &tree_writer,
                        self.instructions.items[init_inst_index..],
                        .{ .start = 0, .len = count },
                    );
                    try tree_writer.pop();
                }
            },
            .global => {
                try writer.print("global ", .{});
                try self.formatType(writer, value.data.global.type);
                try writer.print(" @\"{s}\"", .{name});
                try writer.print(" ", .{});
                try self.formatValue(writer, value.data.global.value);
                try writer.print("\n", .{});
                // try writer.print("global {}\n", .{value.data.global.value});
            },
            .type => {
                try writer.print("type @\"{s}\" = ", .{name});
                try self.formatTypeLong(writer, &tree_writer, value.data.type);
                try writer.print("\n", .{});
            },
            else => {
                try writer.print("todo_decl({s})\n", .{@tagName(value.data)});
            },
            // try formatType(writer, builder, ty);
        }
    } else {
        try writer.print("todo({s})\n", .{@tagName(declaration.value.simple)});
        try writer.print("todo({})\n", .{declaration});
    }
    // switch (declaration.type) {
    //     .simple => |simple| {},
    //     .complex => |complex| {
    //         const ty = builder.getType(key: Sema.Type.Key)

    //     },
    // }

    // try tree_writer.pop();
}

pub fn format(self: *Self, writer: std.io.AnyWriter) !void {
    const builder = self.builder;
    const declarations = self.declarations.items;
    const len = declarations.len;

    try writer.print(";; Sema\n", .{});

    try writer.print(";; {d} declarations\n", .{len});
    try writer.print(";; {d} entities\n", .{builder.entities.len});
    try writer.print(";; {d} symbols\n", .{builder.symbols.count()});
    try writer.print(";; {d} instructions\n", .{self.instructions.items.len});
    try writer.print(";; {d} values\n", .{self.values.count()});
    try writer.print(";; {d} types\n", .{self.types.count()});
    try writer.print(";; {d} lists\n", .{self.lists.count()});
    try writer.print(";; {d} strings\n", .{self.strings.count()});
    try writer.print(";; {d}/{d} comptime memory used/allocated bytes\n", .{ self.memory.memory.items.len, self.memory.memory.capacity });

    try writer.print("\n", .{});

    for (0..self.declarations.items.len) |i| {
        try writer.print("%{d} = ", .{i});
        try self.formatDeclaration(
            writer,
            i,
        );
        try writer.print("\n", .{});
    }
}
test "Sema" {
    const allocator = std.testing.allocator;
    var errors_manager = try ErrorManager.init(allocator);
    defer errors_manager.deinit();
    const source =
        // \\pub fn a():void {
        // \\  const _ = b
        // \\}
        // \\pub fn b():void {
        // \\  const _ = a
        // \\}

        \\ pub fn fib(n: i32): i32 {
        \\   var a:i32 = 0
        \\   var b:i32 = 1
        \\   if (n > 0) {
        \\     while (n > 1) {
        \\       var t:i32 = a + b
        \\       a = b
        \\       b = t
        \\       n = n - 1
        \\     }
        \\     return b
        \\   }
        \\   return a
        \\ }
        \\

        // \\type A = struct {
        // \\  a: usize,
        // \\  b: i32 = 2,
        // \\  c: i32 = 3,
        // \\  pub fn foo(): void {
        // \\    const b: B = B{
        // \\      d = 4,
        // \\      e = 5,
        // \\      f = 6,
        // \\    }
        // \\    const a: A = A{
        // \\      a = 1,
        // \\      b = 2,
        // \\      c = 3,
        // \\    }
        // \\  }
        // \\}
        // \\type B = struct {
        // \\  d: usize,
        // \\  e: i32 = 2,
        // \\  f: i32 = 3,
        // \\  pub fn bar(): void {
        // \\    const a: A = A{
        // \\      a = 1,
        // \\      b = 2,
        // \\      c = 3,
        // \\    }
        // \\    const b: B = B{
        // \\      d = 4,
        // \\      e = 5,
        // \\      f = 6,
        // \\    }
        // \\  }
        // \\}
        // \\fn main(b: i32): void {
        // \\  const a: A = A{
        // \\    a = 1,
        // \\    b = 2,
        // \\    c = 3,
        // \\  }
        // \\  a.foo();
        // \\}
    ;

    var sema = try Self.init(allocator, &errors_manager, .{});
    defer sema.deinit();
    const root = try sema.makeRootSource(source, "root.sk");
    try sema.compileAll(root);
}
