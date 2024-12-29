const std = @import("std");
const Array = std.ArrayListUnmanaged;
const PackedLists = @import("PackedLists.zig").new;
const ErrorManager = @import("ErrorManager.zig");
const Allocator = std.mem.Allocator;
const Ast = @import("Ast.zig");
const HirBuilder = @import("HirBuilder.zig");

const shared = @import("shared.zig");
const InternedStrings = @import("InternedStrings.zig");
const InternedSlice = InternedStrings.InternedSlice;
const assert = std.debug.assert;
const Self = @This();
const fmt = @import("format_utils.zig");
// pub const IrLists = PackedLists(u32, std.math.maxInt(u32));
// pub const Lists = PackedLists(u32, std.math.maxInt(u32));
pub const InternedLists = @import("interned-lists.zig").InternedLists(u32);

// All arrays of indexes, just aliases to make usage intention clearer
// pub const TypeLists = IrLists;
// pub const ValueLists = IrLists;
// pub const DefLists = IrLists;

ast: *Ast,
allocator: std.mem.Allocator,
lists: InternedLists,
insts: Array(Inst) = .{},

// inst: Array(Inst) = .{},
// types: Array(Type) = .{},
// values: Array(Value) = .{},
// defs: Array(Def) = .{},

pub fn init(allocator: Allocator, ast: *Ast) !Self {
    return .{
        .allocator = allocator,
        .ast = ast,
        .lists = InternedLists.init(allocator),
    };
}
pub fn build(allocator: Allocator, ast: *Ast, errors: *ErrorManager, options: HirBuilder.Options) !Self {
    return try HirBuilder.build(allocator, ast, errors, options);
}
pub fn deinit(self: *Self) void {
    self.lists.deinit();
    self.insts.deinit(self.allocator);
}
const Formater = struct {
    writer: std.io.AnyWriter,
    level: usize,
    value: *Self,
    pub fn indent(self: *@This()) !void {
        // std.debug.print("{}|", .{self.level});
        try self.writer.writeByteNTimes(' ', self.level * 2);
    }
    pub fn printNode(self: *@This(), node: Ast.Node.Index) !void {
        const token = self.value.ast.getNodeStartToken(node);
        try self.writer.print("{s}", .{self.value.ast.getTokenSlice(token)});
    }
    pub fn openBrace(self: *@This()) !void {
        try self.writer.writeAll("{");
    }
    pub fn closeBrace(self: *@This()) !void {
        try self.writer.writeAll("}");
    }
    pub fn breakLine(self: *@This()) !void {
        try self.writer.writeAll("\n");
    }
};
const TreeWriter = struct {
    writer: std.io.AnyWriter,

    indents: std.BoundedArray(bool, 1024),
    pub fn init(writer: std.io.AnyWriter) @This() {
        return .{
            .writer = writer,

            .indents = std.BoundedArray(bool, 1024).init(0) catch unreachable,
        };
    }
    pub fn pushBlank(self: *@This()) !void {
        try self.indents.append(false);
    }
    pub fn pushDirLine(self: *@This()) !void {
        try self.indents.append(true);
    }
    pub fn pop(self: *@This()) !void {
        _ = self.indents.pop();
    }
    pub const Kind = enum {
        item,
        last_item,
    };
    pub fn writeIndent(self: *@This(), is_item: bool, is_last: bool) !void {
        for (self.indents.slice(), 0..) |indent, i| {
            const is_last_indent = i == self.indents.slice().len - 1;
            if (indent) {
                // if (kind == .last_item or i == self.indents.slice().len - 1) {
                //     try self.writer.writeAll(" └ ");
                //     continue;
                // }
                // if (kind == null) {
                //     try self.writer.writeAll(" │ ");
                //     continue;
                // }
                if (is_item and is_last_indent) {
                    if (is_last) {
                        try self.writer.writeAll(" └ ");
                        try self.pop();
                        try self.pushBlank();
                    } else {
                        try self.writer.writeAll(" ├ ");
                    }
                } else {
                    try self.writer.writeAll(" │ ");
                }
            } else {
                try self.writer.writeAll("   ");
            }
        }
    }
    pub fn writeLastItem(self: *@This()) !void {
        for (self.indents.slice(), 0..) |indent, i| {
            if (indent) {
                if (i == self.indents.items.len - 1) {
                    try self.writer.writeAll("└ ");
                } else {
                    try self.writer.writeAll("├ ");
                }
            } else {
                try self.writer.writeAll("  ");
            }
        }
    }
};
pub fn formatInstShort(self: *Self, writer: std.io.AnyWriter, tree_writer: *TreeWriter, inst_index: Inst.Index) std.io.AnyWriter.Error!void {
    _ = tree_writer; // autofix
    const instruction: Inst = self.insts.items[inst_index];

    try writer.print("%{} = .{s} ", .{ inst_index, @tagName(instruction) });
    switch (instruction) {
        inline else => |data| {
            const fields = comptime std.meta.fields(@TypeOf(data));
            inline for (fields, 0..) |field, i| {
                if (i > 0) {
                    try writer.print(", ", .{});
                }
                const value = @field(data, field.name);

                try writer.print("{s}=", .{
                    field.name,
                });
                if (comptime std.mem.endsWith(u8, field.name, "node")) {
                    try writer.print("({s})", .{NodeRef.init(self, value, true)});
                } else if (comptime std.mem.endsWith(u8, field.name, "list")) {
                    const list = self.lists.getSlice(value);
                    try writer.writeAll("({");
                    for (list, 0..list.len) |item, j| {
                        if (j > 0) {
                            try writer.print(", ", .{});
                        }
                        try writer.print("%{d}", .{item});
                    }
                    try writer.writeAll("})");
                    // try writer.print("({d} items)", .{value.len});
                } else if (field.type == Inst.Index) {
                    try writer.print("(%{d})", .{
                        value,
                    });
                } else {
                    try writer.print("({any})", .{
                        value,
                    });
                }
            }
        },
    }
    try writer.print("\n", .{});
}
const NodeRef = struct {
    node_index: ?Ast.Node.Index,
    show_slice: bool,
    hir: *Self,
    pub fn init(hir: *Self, node_index: ?Ast.Node.Index, show_slice: bool) @This() {
        return .{
            .node_index = node_index,
            .show_slice = show_slice,
            .hir = hir,
        };
    }
    pub fn format(value: @This(), comptime _: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        _ = options; // autofix
        if (value.node_index) |index| {
            if (value.show_slice) {
                try writer.print("n[{d}:\"{s}\"]", .{ index, value.hir.ast.getNodeSlice(index) });
            } else {
                try writer.print("n[{d}]", .{index});
            }
        } else {
            try writer.writeAll("NONE");
        }
    }
};

pub fn formatInst(self: *Self, writer: std.io.AnyWriter, tree_writer: *TreeWriter, inst_index: Inst.Index) std.io.AnyWriter.Error!void {
    const instruction: Inst = self.insts.items[inst_index];
    if (tree_writer.indents.len > 10) {
        return;
    }

    switch (instruction) {
        .block, .inline_block => |data| {
            try writer.print("%{} = {s}.{s}", .{ inst_index, if (data.is_comptime) "comptime " else "", @tagName(instruction) });

            try writer.print("\n", .{});
            const instructions_list = self.lists.getSlice(data.instructions_list);
            try tree_writer.pushDirLine();
            for (instructions_list, 0..instructions_list.len) |instruction_index, i| {
                try tree_writer.writeIndent(true, i == instructions_list.len - 1);
                const inst = self.insts.items[instruction_index];
                switch (inst) {
                    .block, .inline_block, .loop, .global_decl, .struct_decl => {
                        try formatInst(self, writer, tree_writer, instruction_index);
                    },
                    .if_expr => {
                        try formatInst(self, writer, tree_writer, instruction_index);
                    },
                    else => {
                        try formatInstShort(self, writer, tree_writer, instruction_index);
                    },
                }
            }
            try tree_writer.pop();
        },

        .if_expr => |if_expr| {
            try writer.print("%{} = .{s}", .{ inst_index, @tagName(instruction) });
            try tree_writer.pushDirLine();
            // try tree_writer.writeIndent(true, false);
            try writer.print(" cond (%{d})\n", .{if_expr.cond});
            try tree_writer.writeIndent(true, if_expr.else_body == null);
            try writer.print("then: ", .{});
            try formatInst(self, writer, tree_writer, if_expr.then_body);
            if (if_expr.else_body) |else_body| {
                try tree_writer.writeIndent(true, true);
                try writer.print("else:", .{});

                try formatInst(self, writer, tree_writer, else_body);
            }
            try tree_writer.pop();
        },

        inline else => |data| {
            const Data: type = @TypeOf(data);

            try writer.print("%{} = .{s}", .{ inst_index, @tagName(instruction) });

            try writer.print("\n", .{});

            const fields = std.meta.fields(Data);
            try tree_writer.pushDirLine();
            inline for (fields, 0..) |field, j| {
                const is_last = j == fields.len - 1;
                try tree_writer.writeIndent(true, is_last);
                try writer.print("{s}: ", .{field.name});

                const field_value = @field(data, field.name);

                if (comptime std.mem.endsWith(u8, field.name, "node")) {
                    // @compileLog(field.name);
                    if (@typeInfo(field.type) == .optional) {
                        if (field_value) |value| {
                            // try writer.print("node(#{d}: \"{s}\")\n", .{ value, self.ast.getNodeSlice(value) });
                            try writer.print("({s})", .{NodeRef.init(self, value, true)});
                        } else {
                            try writer.print("NONE\n", .{});
                        }
                    } else {
                        try writer.print("node(#{d}: \"{s}\")\n", .{ field_value, self.ast.getNodeSlice(field_value) });
                    }
                } else if (field.type == Inst.Index and std.meta.activeTag(self.insts.items[field_value]) == .global_decl and comptime std.mem.endsWith(u8, field.name, "type")) {
                    const type_inst = self.insts.items[field_value];

                    try writer.print(".{s}(%{d}: '{s}')\n", .{ @tagName(type_inst), field_value, self.ast.getNodeSlice(type_inst.global_decl.name_node) });
                } else {
                    switch (field.type) {
                        u32 => {
                            try self.formatInst(writer, tree_writer, field_value);
                            // try writer.writeAll("\n");
                        },
                        ?u32 => {
                            if (field_value) |value| {
                                try self.formatInst(writer, tree_writer, value);
                            } else {
                                try writer.print("NONE\n", .{});
                            }
                        },
                        InternedLists.Range => {
                            try writer.print("{d} items\n", .{field_value.len});
                            if (field_value.len > 0) {
                                try tree_writer.pushDirLine();
                                const list = self.lists.getSlice(field_value);

                                for (list, 0..list.len) |item, i| {
                                    // const type_name = @typeName(@TypeOf(item));
                                    // try fmt.writeIndent(writer, indent + 2, options);
                                    try tree_writer.writeIndent(true, i == list.len - 1);

                                    const inst = self.insts.items[item];
                                    _ = inst; // autofix
                                    try writer.print("{d}: ", .{
                                        i,
                                    });

                                    try self.formatInst(writer, tree_writer, item);
                                }
                                try tree_writer.pop();
                            }
                        },
                        bool => {
                            try writer.print("{}\n", .{field_value});
                        },

                        else => {
                            if (comptime std.meta.activeTag(@typeInfo(field.type)) == .@"enum") {
                                try writer.print("\"{s}\"\n", .{@tagName(field_value)});
                            } else {
                                try writer.print("{s}\n", .{@typeName(field.type)});
                            }
                        },
                    }
                }
                //     }
            }

            try tree_writer.pop();
        },
    }
}
pub fn formatRoot(self: *Self, writer: std.io.AnyWriter, inst_index: Inst.Index) std.io.AnyWriter.Error!void {
    try writer.print(";; HIR: {d} instructions\n\n", .{self.insts.items.len});

    var tree_writer = TreeWriter.init(writer);
    try formatInst(self, writer, &tree_writer, inst_index);
}
pub fn format(self: Self, comptime _: []const u8, _: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
    try formatRoot(
        @constCast(&self),
        writer,
        Inst.RootIndex,
    );
}
pub const Value = struct {
    // tag: Tag,
    // data: Data,
    data: union(enum) {
        comptime_number: InternedSlice,
    },
    pub const Index = enum(u32) {
        true,
        false,
        null,

        _,
        pub const INDEX_START: u32 = std.meta.tags(@This().Index).len;

        pub fn asInt(self: Index) u32 {
            return @intCast(@intFromEnum(self));
        }

        pub fn asTypeIndex(self: u32) @This().Index {
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
};

// std.zig.Zir.Header

pub const Inst = union(enum) {
    fn_decl: FnDecl,
    fn_call: FnCall,
    struct_decl: StructDecl,
    struct_field: StructField,
    param_decl: Param,
    param: UnaryOp,
    param_get: UnaryOp,
    global_get: UnaryOp,
    global_decl: GlobalDecl,
    local_get: UnaryOp,
    load: UnaryOp,
    block: Block,
    inline_block: Block,
    local_set: BinaryOp,
    param_set: BinaryOp,
    global_set: BinaryOp,
    loop: Loop,
    assign: BinaryOp,
    alloc: Alloc,
    array_init: ArrayInit,
    type_init: TypeInit,
    field_init: FieldInit,

    store: Store,
    get_element_pointer: GetElement,
    get_element_value: GetElement,
    get_property_pointer: GetProperty,
    get_property_value: GetProperty,

    constant_int: Constant,

    as: BinaryOp,
    add: BinaryOp,
    sub: BinaryOp,
    mul: BinaryOp,
    div: BinaryOp,

    gt: BinaryOp,
    ge: BinaryOp,
    lt: BinaryOp,
    le: BinaryOp,
    eq: BinaryOp,
    ne: BinaryOp,

    typeof: UnaryOp,
    sizeof: UnaryOp,
    ret: MaybeOperand,
    if_expr: IfExpr,
    select_expr: IfExpr,
    br: Break,

    decl_ref: AstNode,
    comptime_number: AstNode,
    comptime_boolean: AstNode,
    ty_number: AstNode,
    ty_boolean: AstNode,
    undefined_value: MaybeAstNode,

    ty_i8: AstNode,
    ty_i16: AstNode,
    ty_i32: AstNode,
    ty_i64: AstNode,
    ty_i128: AstNode,
    ty_i256: AstNode,

    ty_u8: AstNode,
    ty_u16: AstNode,
    ty_u32: AstNode,
    ty_u64: AstNode,
    ty_u128: AstNode,
    ty_u256: AstNode,
    ty_usize: AstNode,
    ty_array: TyArray,

    ty_f32: AstNode,
    ty_f64: AstNode,
    ty_void: AstNode,
    ty_pointer: UnaryOp,
    ty_global: UnaryOp,

    debug_var: DebugVar,

    pub const Tag: type = std.meta.Tag(@This());
    pub const Return = struct {
        value: ?Inst.Index,
    };
    pub const AstNode = struct {
        node: Ast.Node.Index,
    };
    pub const MaybeAstNode = struct {
        node: ?Ast.Node.Index,
    };
    pub const Alloc = struct {
        mutable: bool,

        type: Inst.Index,
    };
    pub const ArrayInit = struct {
        type: Inst.Index,
        items_list: List,
    };
    pub const TypeInit = struct {
        type: Inst.Index,
        field_init_list: List,
    };
    pub const FieldInit = struct {
        name_node: Ast.Node.Index,
        value: Inst.Index,
    };
    pub const Constant = struct {
        value: i64,
    };
    pub const TyArray = struct {
        type: Inst.Index,
        size: Inst.Index,
    };
    pub const Store = struct {
        pointer: Inst.Index,
        value: Inst.Index,
    };
    pub const GetElement = struct {
        base: Inst.Index,
        index: Inst.Index,
    };
    pub const GetProperty = struct {
        base: Inst.Index,
        property_name_node: Ast.Node.Index,
        is_builtin: bool,
    };

    pub const FnCall = struct {
        callee: Index,
        args_list: List,
    };
    pub const Index = u32;
    pub const RootIndex: Index = 0;
    pub const Enum = std.meta.Tag(@This());
    pub const List = InternedLists.Range;
    pub const IfExpr = struct {
        cond: Index,
        then_body: Index,
        else_body: ?Index,
    };
    pub const Loop = struct {
        body: Index,
    };
    pub const UnaryOp = struct {
        operand: Index,
    };
    pub const MaybeOperand = struct {
        operand: ?Index,
    };
    pub const BinaryOp = struct {
        lhs: Index,
        rhs: Index,
    };
    pub const Break = struct {
        operand: ?Index,
        target: Index,
    };
    pub const FnDecl = struct {
        // name: InternedSlice,
        name_node: Ast.Node.Index,
        return_type: Inst.Index,
        params_list: List,

        // @"extern": bool,
        // visibility: shared.Visibility,
        // exported: bool,
        // mutable: bool,
    };
    pub const DebugVar = struct {
        name_node: Ast.Node.Index,
        instruction: Index,
    };
    pub const LocalDecl = struct {
        name_node: Ast.Node.Index,
        ty_node: ?Ast.Node.Index,
        init: ?Inst.Index,
        mutable: bool,
    };
    pub const Block = struct {
        name_node: ?Ast.Node.Index,
        instructions_list: InternedLists.Range,
        is_comptime: bool = false,
    };
    pub const GlobalDecl = struct {
        name_node: Ast.Node.Index,
        @"extern": bool,
        is_fn: bool,
        is_type: bool,
        visibility: shared.Visibility,
        exported: bool,
        mutable: bool,
        type: ?Inst.Index,
        init: ?Inst.Index,
    };
    pub const StructDecl = struct {
        name_node: ?Ast.Node.Index,
        fields_list: List,
        declarations_list: List,
    };
    pub const StructField = struct {
        name_node: Ast.Node.Index,
        type: ?Inst.Index,
        init: ?Inst.Index,
    };
    pub const Param = struct {
        name_node: Ast.Node.Index,
        // ty_node: Ast.Node.Index,
        type: Inst.Index,
    };
};

pub const Definition = union(enum) {
    fn_definition: FnDefinition,

    pub const Index = u32;
    pub const List = usize;

    const FnDefinition = struct {
        name: InternedSlice,
        visibility: Visibility,
        exported: bool,
        mutable: bool,
        params_list: List,
        return_type: Inst.Index,
        init: ?Inst.Index = null,
    };
    pub const Visibility = enum {
        @"pub",
        private,
    };
};

const Patience = @import("./patience_diff.zig");
fn hirMatch(source: []const u8, instruction_index: Inst.Index, expected: []const u8) !void {
    var stderr_writer = std.io.getStdErr().writer().any();
    const test_allocator = std.testing.allocator;
    var errors = try ErrorManager.init(test_allocator);
    defer errors.deinit();
    try stderr_writer.writeAll("\n");
    try stderr_writer.writeAll("=" ** 50);
    try stderr_writer.writeAll("\n");
    try stderr_writer.writeAll(source);
    try stderr_writer.writeAll("\n");
    var ast = try Ast.parse(test_allocator, &errors, source, .{});
    defer ast.deinit();

    var arr = std.ArrayList(u8).init(std.testing.allocator);
    defer arr.deinit();
    const writer = arr.writer().any();
    var hir = try Self.build(test_allocator, &ast, &errors, .{
        .trace_dir = "./.tmp/trace",
        .trace_name = "hir-test",
        .unique_trace_name = true,
    });
    defer hir.deinit();
    // try writer.print("{}", .{hir});
    var tree_writer = TreeWriter.init(writer);
    try hir.formatInst(writer, &tree_writer, instruction_index);

    var res = try Patience.diff(
        std.testing.allocator,
        arr.items,
        expected,
    );
    defer res.deinit();
    if (res.operations.len > 0) {
        try ast.formatRoot(stderr_writer, 0, .{
            .color = false,
            .indent_size = 2,
            .show_node_index = false,
            .show_slice = false,
            .show_token_range = false,
        });
        try stderr_writer.writeAll("\\\\");
        for (arr.items) |c| {
            try stderr_writer.writeByte(c);
            if (c == '\n') {
                try stderr_writer.writeAll("\\\\");
            }
        }
        try stderr_writer.writeAll("\n");
        try res.format(stderr_writer, .{
            .color = true,
        });
        return error.TestFailed;
    }
}

test "Hir" {
    try hirMatch(
        \\fn foo() void {
        \\    return;
        \\}
        \\
        \\pub fn bar(n: u32, m: u32) i32 {
        \\    return 2;
        \\}
    , Inst.RootIndex,
        \\%0 = struct_decl {
        \\ ├ name_node: NONE
        \\ ├ fields_list: 0 items
        \\ └ declarations_list: 2 items
        \\    ├ 0: %1 = global_decl {
        \\    │  ├ name_node: node(#1: "foo")
        \\    │  ├ extern: false
        \\    │  ├ is_fn: true
        \\    │  ├ visibility: "private"
        \\    │  ├ exported: false
        \\    │  ├ mutable: false
        \\    │  ├ type: %6 = fn_decl {
        \\    │  │  ├ name_node: node(#1: "foo")
        \\    │  │  ├ return_type: %3 = ty_void {
        \\    │  │  │  └ node: node(#2: "void")
        \\    │  │  └ params_list: 0 items
        \\    │  └ init: %5 = block
        \\    │     └ %4 = ret value=(null)
        \\    └ 1: %2 = global_decl {
        \\       ├ name_node: node(#7: "bar")
        \\       ├ extern: false
        \\       ├ is_fn: true
        \\       ├ visibility: "private"
        \\       ├ exported: false
        \\       ├ mutable: false
        \\       ├ type: %15 = fn_decl {
        \\       │  ├ name_node: node(#7: "bar")
        \\       │  ├ return_type: %11 = ty_i32 {
        \\       │  │  └ node: node(#14: "i32")
        \\       │  └ params_list: 2 items
        \\       │     ├ 0: %8 = param_decl {
        \\       │     │  ├ name_node: node(#9: "n")
        \\       │     │  └ ty: %7 = ty_u32 {
        \\       │     │     └ node: node(#10: "u32")
        \\       │     └ 1: %10 = param_decl {
        \\       │        ├ name_node: node(#12: "m")
        \\       │        └ ty: %9 = ty_u32 {
        \\       │           └ node: node(#13: "u32")
        \\       └ init: %14 = block
        \\          ├ %12 = comptime_number node=(16)
        \\          └ %13 = ret value=(12)
        \\
    );

    try hirMatch(
        \\const Foo = struct {
        \\    a: f32,
        \\    b: u32 = 3,
        \\};
    , Inst.RootIndex,
        \\%0 = struct_decl {
        \\ ├ name_node: NONE
        \\ ├ fields_list: 0 items
        \\ └ declarations_list: 1 items
        \\    └ 0: %1 = global_decl {
        \\       ├ name_node: node(#1: "Foo")
        \\       ├ extern: false
        \\       ├ is_fn: false
        \\       ├ visibility: "private"
        \\       ├ exported: false
        \\       ├ mutable: false
        \\       ├ type: NONE
        \\       └ init: %8 = inline_block
        \\          └ %2 = struct_decl {
        \\             ├ name_node: NONE
        \\             ├ fields_list: 2 items
        \\             │  ├ 0: %4 = struct_field {
        \\             │  │  ├ name_node: node(#3: "a")
        \\             │  │  ├ ty: %3 = ty_f32 {
        \\             │  │  │  └ node: node(#4: "f32")
        \\             │  │  └ init: NONE
        \\             │  └ 1: %7 = struct_field {
        \\             │     ├ name_node: node(#6: "b")
        \\             │     ├ ty: %5 = ty_u32 {
        \\             │     │  └ node: node(#7: "u32")
        \\             │     └ init: %6 = comptime_number {
        \\             │        └ node: node(#8: "3")
        \\             └ declarations_list: 0 items
        \\
    );
    try hirMatch(
        \\const Foo = struct {
        \\    a: f32,
        \\    b: u32 = 3,
        \\    pub fn foo() void {
        \\        return;
        \\    }
        \\};
    , Inst.RootIndex,
        \\%0 = struct_decl {
        \\ ├ name_node: NONE
        \\ ├ fields_list: 0 items
        \\ └ declarations_list: 1 items
        \\    └ 0: %1 = global_decl {
        \\       ├ name_node: node(#1: "Foo")
        \\       ├ extern: false
        \\       ├ is_fn: false
        \\       ├ visibility: "private"
        \\       ├ exported: false
        \\       ├ mutable: false
        \\       ├ type: NONE
        \\       └ init: %13 = inline_block
        \\          └ %2 = struct_decl {
        \\             ├ name_node: NONE
        \\             ├ fields_list: 2 items
        \\             │  ├ 0: %4 = struct_field {
        \\             │  │  ├ name_node: node(#3: "a")
        \\             │  │  ├ ty: %3 = ty_f32 {
        \\             │  │  │  └ node: node(#4: "f32")
        \\             │  │  └ init: NONE
        \\             │  └ 1: %7 = struct_field {
        \\             │     ├ name_node: node(#6: "b")
        \\             │     ├ ty: %5 = ty_u32 {
        \\             │     │  └ node: node(#7: "u32")
        \\             │     └ init: %6 = comptime_number {
        \\             │        └ node: node(#8: "3")
        \\             └ declarations_list: 1 items
        \\                └ 0: %8 = global_decl {
        \\                   ├ name_node: node(#10: "foo")
        \\                   ├ extern: false
        \\                   ├ is_fn: true
        \\                   ├ visibility: "public"
        \\                   ├ exported: false
        \\                   ├ mutable: false
        \\                   ├ type: %12 = fn_decl {
        \\                   │  ├ name_node: node(#10: "foo")
        \\                   │  ├ return_type: %9 = ty_void {
        \\                   │  │  └ node: node(#11: "void")
        \\                   │  └ params_list: 0 items
        \\                   └ init: %11 = block
        \\                      └ %10 = ret value=(null)
        \\
    );
}
