const std = @import("std");
const Array = std.ArrayListUnmanaged;
const ArrayHashMap = std.AutoArrayHashMapUnmanaged;
const Tracer = @import("../Tracer.zig");
const Builder = @import("./gen.zig").Builder;
const Hir = @import("../Hir.zig");
const Ast = @import("../Ast.zig");
const ErrorManager = @import("../ErrorManager.zig");
const Entity = @import("./gen.zig").Entity;
const InternedList = @import("../interned-lists.zig").InternedLists;
pub const Lists = InternedList(usize);
pub const Strings = InternedList(u8);

allocator: std.mem.Allocator,

instructions: Array(Instruction) = .{},
values: ArrayHashMap(Value.Key, Value) = .{},
types: ArrayHashMap(Type.Key, Type) = .{},
strings: Strings,
lists: Lists,

const Self = @This();

pub fn build(
    allocator: std.mem.Allocator,
    hir: *Hir,
    errors_manager: *ErrorManager,
    options: Builder.BuildOptions,
) !Self {
    return Builder.build(
        allocator,
        hir,
        errors_manager,
        options,
    );
}
pub fn deinit(self: *Self) void {
    self.instructions.deinit(self.allocator);
    self.values.deinit(self.allocator);
    self.types.deinit(self.allocator);
    self.strings.deinit();
    self.lists.deinit();
}

pub const Type = struct {
    hash: u64,
    data: Data,

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
        type,
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
        string,
        void,
    };

    pub fn simple(self: Simple) Key {
        return .{ .simple = self };
    }
    pub fn complex(self: usize) Key {
        return .{ .complex = self };
    }
    pub const Data = union(enum) {
        module: Ent,

        @"struct": struct {
            fields: Type.List,
        },
        function: struct {
            params: Type.List,
            ret: Type.Key,
        },
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
    };
};
pub const Value = struct {
    hash: u64,
    data: Data,
    pub const Data = union(enum) {
        integer: i64,
        float: f64,
        type: Type.Key,

        struct_instance: struct {
            struct_entity: Entity.Key,
            values: Value.List,
        },

        function: struct {
            type: Type.Key,
            init: ?Instruction.InstRange,
        },
    };

    pub const List = Lists.Range;
    pub const Key = union(enum) {
        simple: Simple,
        complex: usize,

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
            };
        }
    };
    pub const Simple = enum {
        unknown,
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
    };
    pub fn simple(self: Simple) Key {
        return .{ .simple = self };
    }
    pub fn complex(self: usize) Key {
        return .{ .complex = self };
    }
};

pub const Instruction = struct {
    op: Op,
    type: Type.Key,
    value: Value.Key,
    data: Data,

    pub const Data = union(enum) {
        void,
        // param: struct {
        //     name: Value.Key,
        //     value: Value.Key,
        // },
        alloc: struct {
            type_inst: Instruction.Index,
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
        @"if": struct {
            condition: Instruction.Index,
            then_instructions: InstRange,
            else_instructions: ?InstRange,
        },

        loop: struct {
            instructions: InstRange,
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
        param,
        constant,
        type,
        alloc,
        store,
        cast,
        loop,
        load,

        param_get,
        param_set,

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
        br,
        ret,
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

pub fn formatType(writer: std.io.AnyWriter, builder: *Builder, type_key: Type.Key) !void {
    if (builder.getType(type_key)) |ty| {
        switch (ty.data) {
            .pointer => |pointer| {
                try writer.writeAll("*");
                try formatType(writer, builder, pointer.child);
            },
            .array => |array| {
                try writer.print("[{d}]", .{array.size});
                try formatType(writer, builder, array.child);
            },

            else => {
                try writer.print("todo({s})", .{@tagName(ty.data)});
            },
        }
        return;
    }
    try writer.print("{s}", .{@tagName(type_key.simple)});
}

fn formatValue(writer: std.io.AnyWriter, builder: *Builder, value: Value.Key) !void {
    if (builder.getValue(value)) |val| {
        switch (val.data) {
            .float => |f| {
                try writer.print("float({d})", .{f});
            },
            .integer => |i| {
                try writer.print("int({d})", .{i});
            },
            .type => |type_key| {
                try writer.print("type(", .{});
                try formatType(writer, builder, type_key);
                try writer.print(")", .{});
            },

            else => {
                try writer.print("todo({s})", .{@tagName(val.data)});
            },
        }
        return;
    }

    if (std.mem.startsWith(u8, @tagName(value.simple), "type_")) {
        try writer.print("type({s})", .{@tagName(value.simple)[5..]});
    } else {
        try writer.print("{s}", .{@tagName(value.simple)});
    }
}
pub fn formatInstructionRange(
    writer: std.io.AnyWriter,
    builder: *Builder,
    tree_writer: *TreeWriter,
    instructions: []Instruction,
    range: Instruction.InstRange,
) !void {
    var i: usize = range.start;
    var buf = try std.BoundedArray(u8, 1024).init(0);
    const buf_writer = buf.writer().any();
    const range_end = range.start + range.len;
    while (i < range.start + range.len) : (i += 1) {
        const inst = instructions[i];
        // try writer.print("{s}\n", .{@tagName(inst.op)});
        buf.clear();

        switch (inst.data) {
            .@"if" => |if_expr| {
                const last_inst = if_expr.then_instructions.start + if_expr.then_instructions.len;
                try tree_writer.writeIndent(true, last_inst == range_end);
                try writer.print("%{d} if (%{d}):\n", .{ i, if_expr.condition });

                try tree_writer.pushDirLine();
                try formatInstructionRange(
                    writer,
                    builder,
                    tree_writer,
                    instructions,
                    if_expr.then_instructions,
                );
                try tree_writer.pop();
                i = if_expr.then_instructions.start + if_expr.then_instructions.len;

                if (if_expr.else_instructions) |else_instructions| {
                    try tree_writer.pushDirLine();
                    try formatInstructionRange(
                        writer,
                        builder,
                        tree_writer,
                        instructions,
                        else_instructions,
                    );
                    try tree_writer.pop();
                    i = else_instructions.start + else_instructions.len;
                }
            },
            .loop => |loop| {
                const last_inst = loop.instructions.start + loop.instructions.len;

                try tree_writer.writeIndent(true, last_inst == range_end);
                try writer.print("%{d} loop:\n", .{i});
                try writer.writeAll(buf.slice());
                try tree_writer.pushDirLine();
                try formatInstructionRange(
                    writer,
                    builder,
                    tree_writer,
                    instructions,
                    loop.instructions,
                );
                try tree_writer.pop();
                i = loop.instructions.start + loop.instructions.len - 1;
            },
            inline else => |data| {
                try tree_writer.writeIndentTo(buf_writer, true, i == range.start + range.len - 1);
                try buf_writer.print("%{d}: ", .{i});
                try formatType(buf_writer, builder, inst.type);
                try buf_writer.print(" = .{s}", .{@tagName(inst.op)});
                // try writer.print("%{d} .{s}", .{ i, @tagName(inst.op) });
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
                                try formatType(buf_writer, builder, value);
                                try buf_writer.print(")", .{});
                            } else if (field.type == Instruction.Index) {
                                try buf_writer.print("(%{d})", .{value});
                            } else {
                                try buf_writer.print("({any})", .{value});
                            }
                        }
                    },
                    else => {
                        if (T == Instruction.Index) {
                            try buf_writer.print(" {s}=(%{d})", .{ @tagName(inst.data), data });
                        } else if (T == ?Instruction.Index) {
                            if (data) |value| {
                                try buf_writer.print(" {s}=(%{d})", .{ @tagName(inst.data), value });
                            } else {
                                try buf_writer.print(" {s}=(null)", .{@tagName(inst.data)});
                            }
                        } else {
                            try buf_writer.print(" {s}=({any})", .{ @tagName(inst.data), data });
                        }
                    },
                }
                try writer.print("{s: <70}; ", .{buf.slice()});
                try formatValue(writer, builder, inst.value);
                try writer.print("\n", .{});
            },
        }
        // switch (inst.data) {
        //     else => {},
        // }
    }
}
pub fn formatDeclaration(writer: std.io.AnyWriter, builder: *Builder, declaration_index: Declaration.Index) !void {
    var tree_writer = TreeWriter.init(writer);
    const declaration = builder.declarations.items[declaration_index];

    try tree_writer.pushDirLine();
    const name = builder.strings.getSlice(declaration.name);
    if (declaration.is_pub) {
        try writer.print("pub ", .{});
    }
    if (declaration.is_export) {
        try writer.print("export ", .{});
    }
    try writer.print("@{s}", .{name});

    if (builder.getType(declaration.type)) |ty| {
        switch (ty.data) {
            .function => |func| {
                const params_iter = builder.lists.getSlice(func.params);
                try writer.print("(", .{});

                for (params_iter, 0..) |param, i| {
                    if (i > 0) {
                        try writer.print(", ", .{});
                    }
                    try formatType(writer, builder, Type.Key.decode(param));
                }

                try writer.print(") -> ", .{});
                try formatType(writer, builder, func.ret);
                try writer.print("\n", .{});
                const func_value = builder.getValue(declaration.value) orelse unreachable;
                if (func_value.data.function.init) |init| {
                    try formatInstructionRange(
                        writer,
                        builder,
                        &tree_writer,
                        builder.instructions.items[init.start .. init.start + init.len],
                        .{
                            .start = 0,
                            .len = init.len,
                        },
                    );
                }

                // declaration.value
                // try tree_writer.pushDirLine();
                // try tree_writer.writeLine("function");
                // try tree_writer.pop();
            },
            else => {},
        }
        // try formatType(writer, builder, ty);
    }
    // switch (declaration.type) {
    //     .simple => |simple| {},
    //     .complex => |complex| {
    //         const ty = builder.getType(key: Sema.Type.Key)

    //     },
    // }

    try tree_writer.pop();
}
test "Sema" {
    const allocator = std.testing.allocator;
    var errors_manager = try ErrorManager.init(allocator);
    defer errors_manager.deinit();
    const source =
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
    var ast = try Ast.parse(allocator, &errors_manager, source[0..], .{});
    defer ast.deinit();
    var hir = try Hir.build(allocator, &ast, &errors_manager, .{});
    defer hir.deinit();
    std.debug.print("hir: {s}\n", .{hir});
    var sema = try Builder.build(allocator, &hir, &errors_manager, .{});
    defer sema.deinit();
    try sema.collectRoot();
    const compiled = try sema.compileDeclaration("root::fib");
    try formatDeclaration(
        std.io.getStdErr().writer().any(),
        &sema,
        compiled,
    );
}
