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
pub fn build(allocator: Allocator, ast: *Ast, errors: *ErrorManager) !Self {
    return try HirBuilder.build(allocator, ast, errors);
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
pub fn formatInstruction(value: *Self, writer: std.io.AnyWriter, options: std.fmt.FormatOptions, inst_index: Inst.Index, indent: usize) !void {
    const inst: Inst = value.insts.items[inst_index];
    var fmt_ = Formater{
        .writer = writer,
        .level = indent,
        .value = value,
    };
    // std.debug.print("indent: {}\n", .{indent});
    switch (inst) {
        .struct_decl => |struct_decl| {
            try fmt_.indent();
            try writer.writeAll("struct_decl {\n");
            fmt_.level += 1;
            try fmt_.indent();
            try writer.writeAll("fields:\n");
            const fields = value.lists.getSlice(struct_decl.fields_list);
            for (fields) |field| {
                try formatInstruction(value, writer, options, field, indent + 2);
            }
            try fmt_.indent();
            try writer.writeAll("declarations:\n");
            const declarations = value.lists.getSlice(struct_decl.declarations_list);
            for (declarations) |declaration_index| {
                try formatInstruction(value, writer, options, declaration_index, indent + 2);
            }
            fmt_.level -= 1;
            try fmt_.indent();
            try writer.writeAll("}\n");
        },
        .local => |local| {
            try fmt_.indent();
            try writer.print("%{d} = local %{} '", .{ inst_index, local.type });

            try fmt_.printNode(local.name_node);
            try writer.print("'\n", .{});
        },
        .local_set => |local_set| {
            try fmt_.indent();
            try writer.print("%{d} = local(%{d}).set %{d}\n", .{ inst_index, local_set.lhs, local_set.rhs });
        },
        .typeof => |typeof| {
            try fmt_.indent();
            try writer.print("%{d} = typeof %{d}\n", .{ inst_index, typeof.operand });
        },
        .fn_decl => |fn_decl| {
            try fmt_.indent();

            if (fn_decl.params_list.len == 0) {
                try writer.writeAll("fn ()\n");
            } else {
                try writer.writeAll("fn (");
                try fmt_.breakLine();

                const params_iter = value.lists.getSlice(fn_decl.params_list);

                for (params_iter) |param_index| {
                    try formatInstruction(value, writer, options, param_index, fmt_.level + 1);
                }

                try fmt_.indent();
                try writer.writeAll(")\n");
            }

            // try writer.writeAll(" ");
            try fmt_.indent();

            try writer.writeAll("ret:\n");
            try formatInstruction(value, writer, options, fn_decl.return_type, fmt_.level + 1);
        },
        .constant_int => |constant_int| {
            try fmt_.indent();
            try writer.print("%{d} = const {d}\n", .{ inst_index, constant_int.value });
            // try fmt_.printNode(const_int.name_node);
            // try fmt_.breakLine();
            // try formatInstruction(value, writer, options, const_int, indent + 1);
        },
        .param_decl => |param_decl| {
            try fmt_.indent();
            try writer.print("%{d} = param @", .{inst_index});
            try fmt_.printNode(param_decl.name_node);
            try fmt_.breakLine();
            try formatInstruction(value, writer, options, param_decl.ty, indent + 1);
        },
        .inline_block, .block => |block| {
            try fmt_.indent();
            try writer.print("%{d} {s} {{\n", .{ inst_index, @tagName(inst) });
            fmt_.level += 1;
            const instructions_list = value.lists.getSlice(block.instructions_list);
            for (instructions_list) |instruction_index| {
                try formatInstruction(value, writer, options, instruction_index, indent + 1);
            }
            try fmt_.indent();
            try writer.writeAll("}\n");
        },
        .comptime_number => |comptime_number| {
            try fmt_.indent();
            const token = value.ast.getNodeStartToken(comptime_number);
            try writer.print("%{d} = comp.number {s}\n", .{ inst_index, value.ast.getTokenSlice(token) });
        },
        .add,
        .sub,
        .mul,
        .div,

        .gt,
        .lt,
        .ge,
        .le,
        .eq,
        .ne,
        .as,
        => |bin| {
            try fmt_.indent();
            try writer.print("%{d} = {s} %{d} %{d}\n", .{ inst_index, @tagName(inst), bin.lhs, bin.rhs });
        },

        .decl_ref => |decl_ref| {
            try fmt_.indent();
            try writer.print("%{d} = ref @", .{inst_index});
            try fmt_.printNode(decl_ref);
            try fmt_.breakLine();
        },
        .ret, .param_get, .local_get, .global_get => |ret| {
            try fmt_.indent();
            try writer.print("%{d} = {s} %{d}\n", .{ inst_index, @tagName(inst), ret.operand });
        },
        .if_expr => |if_expr| {
            try fmt_.indent();
            try writer.print("if %{d} ", .{if_expr.cond});
            try fmt_.openBrace();
            try fmt_.breakLine();
            try formatInstruction(value, writer, options, if_expr.then_body, indent + 1);
            if (if_expr.else_body) |else_body| {
                try fmt_.indent();
                try writer.writeAll("} else {\n");
                try formatInstruction(value, writer, options, else_body, indent + 1);
            }
            try fmt_.indent();
            try writer.writeAll("}");
            try fmt_.breakLine();
        },
        .global_decl => |global_decl| {
            try fmt_.indent();

            try writer.print("%{d} = define global @", .{inst_index});
            try fmt_.printNode(global_decl.name_node);
            try writer.writeAll(" {\n");
            if (global_decl.type) |type_index| {
                try formatInstruction(value, writer, options, type_index, indent + 1);
            }
            // if (global_decl.init) |init_index| {
            // std.debug.print("init: {} {}\n", .{ inst_index, init_index });
            if (global_decl.init) |init_index| {
                try formatInstruction(value, writer, options, init_index, indent + 1);
            }
            // } else {}
            try fmt_.indent();
            try writer.writeAll("}\n");
        },
        .undefined_value => {
            try fmt_.indent();
            try writer.print("%{d} = undefined\n", .{inst_index});
        },
        .ty_i8,
        .ty_i16,
        .ty_i32,
        .ty_i64,
        .ty_i128,
        .ty_i256,

        .ty_u8,
        .ty_u16,
        .ty_u32,
        .ty_u64,
        .ty_u128,
        .ty_u256,
        .ty_usize,

        .ty_f32,
        .ty_f64,
        .ty_void,
        => {
            try fmt_.indent();
            try writer.print("%{d} = {s}\n", .{
                inst_index,
                @tagName(inst),
            });
        },
        .loop => |loop| {
            try fmt_.indent();
            try writer.print("%{d} = loop\n", .{inst_index});
            try formatInstruction(value, writer, options, loop.body, indent + 1);
        },
        .br => |br| {
            try fmt_.indent();
            try writer.print("%{d} = break %{d}\n", .{ inst_index, br.operand });
        },
        .assign => |assign| {
            try fmt_.indent();
            try writer.print("%{d} = assign %{d} = %{d}\n", .{ inst_index, assign.lhs, assign.rhs });
        },
        .debug_var => |debug_var| {
            try fmt_.indent();
            try writer.print("%{d} = debug_var '", .{inst_index});
            try fmt_.printNode(debug_var.name_node);
            try writer.print("' %{d}\n", .{debug_var.instruction});
        },
        .fn_call => |fn_call| {
            try fmt_.indent();
            try writer.print("%{d} = call %{d} ", .{ inst_index, fn_call.callee });
            if (fn_call.args_list.len > 0) {
                // try fmt_.indent();
                try writer.writeAll("with {\n");
                const args_list = value.lists.getSlice(fn_call.args_list);
                fmt_.level += 1;
                for (args_list) |arg_index| {
                    try fmt_.indent();
                    try writer.print("%{d}\n", .{arg_index});
                    // try formatInstruction(value, writer, options, arg_index, indent + 1);
                }
                fmt_.level -= 1;
                try fmt_.indent();
                try writer.writeAll("}\n");
            }
        },
        .ty_array => |ty_array| {
            try fmt_.indent();
            try writer.print("%{d} = type array<%{d}, size=%{d}> \n", .{ inst_index, ty_array.type, ty_array.size });
        },
        .alloc => |alloc| {
            try fmt_.indent();
            try writer.print("%{d} = alloc %{d}\n", .{ inst_index, alloc.type });
        },
        .store => |store| {
            try fmt_.indent();
            try writer.print("%{d} = store %{d} = %{d}\n", .{ inst_index, store.pointer, store.value });
        },
        .get_element_pointer => |get_element_pointer| {
            try fmt_.indent();
            try writer.print("%{d} = get_element_pointer(%{d}, index=%{d})\n", .{ inst_index, get_element_pointer.pointer, get_element_pointer.index });
        },
        .get_element_value => |get_element_value| {
            try fmt_.indent();
            try writer.print("%{d} = get_element_value(%{d}, index=%{d})\n", .{ inst_index, get_element_value.pointer, get_element_value.index });
        },
        .get_property_pointer, .get_property_value => |get_property_value| {
            try fmt_.indent();
            try writer.print("%{d} = {s}(%{d}, property_name='%{s}')\n", .{ inst_index, @tagName(inst), get_property_value.base, get_property_value.property_name });
        },
        .load => |load| {
            try fmt_.indent();
            try writer.print("%{d} = load %{d}\n", .{ inst_index, load.operand });
        },
        .ty_pointer => |ty_pointer| {
            try fmt_.indent();
            try writer.print("%{d} = type pointer<%{d}>\n", .{ inst_index, ty_pointer.operand });
        },

        else => {
            try fmt_.indent();
            try writer.print("{s}\n", .{@tagName(inst)});
        },
    }
}
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
pub fn formatInst(self: *Self, writer: std.io.AnyWriter, tree_writer: *TreeWriter, inst_index: Inst.Index) std.io.AnyWriter.Error!void {
    // @setEvalBranchQuota(10000);
    const instruction: Inst = self.insts.items[inst_index];

    try writer.print("%{} = {s}", .{ inst_index, @tagName(instruction) });
    switch (instruction) {
        inline else => |data| {
            const Data: type = @TypeOf(data);

            try writer.print(" {{\n", .{});

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
                            try writer.print("node(#{d}: \"{s}\")\n", .{ value, self.ast.getNodeSlice(value) });
                        } else {
                            try writer.print("NONE\n", .{});
                        }
                    } else {
                        try writer.print("node(#{d}: \"{s}\")\n", .{ field_value, self.ast.getNodeSlice(field_value) });
                    }
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

            // try fmt.writeIndent(writer, indent, options);
            // try writer.print("}}\n", .{});
            try tree_writer.pop();
        },
    }
}
pub fn format(value: Self, comptime _: []const u8, options: std.fmt.FormatOptions, writer: std.io.AnyWriter) !void {
    _ = fmt; // autofix

    try writer.print(";; HIR: {d} instructions\n\n", .{value.insts.items.len});
    // for (value.insts.items, 0..) |inst, i| {
    //     std.debug.print("{d}: {}\n", .{ i, inst });
    // }
    try formatInst(
        @constCast(&value),
        writer,
        options,
        Inst.RootIndex,
        0,
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
    param_get: UnaryOp,
    global_get: UnaryOp,
    global_decl: GlobalDecl,
    local_get: UnaryOp,
    load: UnaryOp,
    block: Block,
    inline_block: Block,
    local: Local,
    local_set: BinaryOp,
    param_set: BinaryOp,
    global_set: BinaryOp,
    loop: Loop,
    assign: BinaryOp,
    alloc: Alloc,
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
    ret: struct {
        value: ?Inst.Index,
    },
    if_expr: IfExpr,
    br: UnaryOp,

    decl_ref: AstNode,
    comptime_number: AstNode,
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

    debug_var: DebugVar,

    pub const Tag: type = std.meta.Tag(@This());
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
        pointer: Inst.Index,
        index: Inst.Index,
    };
    pub const GetProperty = struct {
        base: Inst.Index,
        property_name_node: Ast.Node.Index,
    };
    pub const Local = struct {
        name_node: Ast.Node.Index,
        mutable: bool,
        type: Inst.Index,
        // init: Inst.Index,
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
    pub const BinaryOp = struct {
        lhs: Index,
        rhs: Index,
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
    };
    pub const GlobalDecl = struct {
        name_node: Ast.Node.Index,
        @"extern": bool,
        is_fn: bool,
        visibility: shared.Visibility,
        exported: bool,
        mutable: bool,
        type: ?Inst.Index,
        init: ?Inst.Index,
    };
    pub const StructDecl = struct {
        name_node: ?Ast.Node.Index,
        declarations_list: List,
        fields_list: List,
    };
    pub const StructField = struct {
        name_node: Ast.Node.Index,
        ty: ?Inst.Index,
        init: ?Inst.Index,
    };
    pub const Param = struct {
        name_node: Ast.Node.Index,
        // ty_node: Ast.Node.Index,
        ty: Inst.Index,
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
    var ast = try Ast.parse(test_allocator, &errors, source);
    defer ast.deinit();

    var arr = std.ArrayList(u8).init(std.testing.allocator);
    defer arr.deinit();
    const writer = arr.writer().any();
    var hir = try Self.build(test_allocator, &ast, &errors);
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
        \\ ├ declarations_list: 2 items
        \\ │  ├ 0: %1 = global_decl {
        \\ │  │  ├ name_node: node(#1: "foo")
        \\ │  │  ├ extern: false
        \\ │  │  ├ is_fn: true
        \\ │  │  ├ visibility: "private"
        \\ │  │  ├ exported: false
        \\ │  │  ├ mutable: false
        \\ │  │  ├ type: %6 = fn_decl {
        \\ │  │  │  ├ name_node: node(#1: "foo")
        \\ │  │  │  ├ return_type: %3 = ty_void {
        \\ │  │  │  │  └ node: node(#2: "void")
        \\ │  │  │  └ params_list: 0 items
        \\ │  │  └ init: %5 = block {
        \\ │  │     ├ name_node: NONE
        \\ │  │     └ instructions_list: 1 items
        \\ │  │        └ 0: %4 = ret {
        \\ │  │           └ value: NONE
        \\ │  └ 1: %2 = global_decl {
        \\ │     ├ name_node: node(#7: "bar")
        \\ │     ├ extern: false
        \\ │     ├ is_fn: true
        \\ │     ├ visibility: "public"
        \\ │     ├ exported: false
        \\ │     ├ mutable: false
        \\ │     ├ type: %15 = fn_decl {
        \\ │     │  ├ name_node: node(#7: "bar")
        \\ │     │  ├ return_type: %11 = ty_i32 {
        \\ │     │  │  └ node: node(#14: "i32")
        \\ │     │  └ params_list: 2 items
        \\ │     │     ├ 0: %8 = param_decl {
        \\ │     │     │  ├ name_node: node(#9: "n")
        \\ │     │     │  └ ty: %7 = ty_u32 {
        \\ │     │     │     └ node: node(#10: "u32")
        \\ │     │     └ 1: %10 = param_decl {
        \\ │     │        ├ name_node: node(#12: "m")
        \\ │     │        └ ty: %9 = ty_u32 {
        \\ │     │           └ node: node(#13: "u32")
        \\ │     └ init: %14 = block {
        \\ │        ├ name_node: NONE
        \\ │        └ instructions_list: 2 items
        \\ │           ├ 0: %12 = comptime_number {
        \\ │           │  └ node: node(#16: "2")
        \\ │           └ 1: %13 = ret {
        \\ │              └ value: %12 = comptime_number {
        \\ │                 └ node: node(#16: "2")
        \\ └ fields_list: 0 items
        \\
    );

    try hirMatch(
        \\const Foo = struct {
        \\    a: u32,
        \\    b: u32 = 3,
        \\    pub fn foo() void {
        \\        return;
        \\    }
        \\};
    , Inst.RootIndex,
        \\%0 = struct_decl {
        \\ ├ name_node: NONE
        \\ ├ declarations_list: 1 items
        \\ │  └ 0: %1 = global_decl {
        \\ │     ├ name_node: node(#1: "Foo")
        \\ │     ├ extern: false
        \\ │     ├ is_fn: false
        \\ │     ├ visibility: "private"
        \\ │     ├ exported: false
        \\ │     ├ mutable: false
        \\ │     ├ type: NONE
        \\ │     └ init: %13 = inline_block {
        \\ │        ├ name_node: NONE
        \\ │        └ instructions_list: 7 items
        \\ │           ├ 0: %3 = ty_u32 {
        \\ │           │  └ node: node(#4: "u32")
        \\ │           ├ 1: %5 = ty_u32 {
        \\ │           │  └ node: node(#7: "u32")
        \\ │           ├ 2: %6 = comptime_number {
        \\ │           │  └ node: node(#8: "3")
        \\ │           ├ 3: %8 = global_decl {
        \\ │           │  ├ name_node: node(#10: "foo")
        \\ │           │  ├ extern: false
        \\ │           │  ├ is_fn: true
        \\ │           │  ├ visibility: "public"
        \\ │           │  ├ exported: false
        \\ │           │  ├ mutable: false
        \\ │           │  ├ type: %12 = fn_decl {
        \\ │           │  │  ├ name_node: node(#10: "foo")
        \\ │           │  │  ├ return_type: %9 = ty_void {
        \\ │           │  │  │  └ node: node(#11: "void")
        \\ │           │  │  └ params_list: 0 items
        \\ │           │  └ init: %11 = block {
        \\ │           │     ├ name_node: NONE
        \\ │           │     └ instructions_list: 1 items
        \\ │           │        └ 0: %10 = ret {
        \\ │           │           └ value: NONE
        \\ │           ├ 4: %9 = ty_void {
        \\ │           │  └ node: node(#11: "void")
        \\ │           ├ 5: %11 = block {
        \\ │           │  ├ name_node: NONE
        \\ │           │  └ instructions_list: 1 items
        \\ │           │     └ 0: %10 = ret {
        \\ │           │        └ value: NONE
        \\ │           └ 6: %12 = fn_decl {
        \\ │              ├ name_node: node(#10: "foo")
        \\ │              ├ return_type: %9 = ty_void {
        \\ │              │  └ node: node(#11: "void")
        \\ │              └ params_list: 0 items
        \\ └ fields_list: 0 items
        \\
    );
}
