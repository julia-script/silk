const std = @import("std");
const MultiArray = std.MultiArrayList;
const PackedLists = @import("PackedLists.zig").new;
const Allocator = std.mem.Allocator;
const Array = std.ArrayListUnmanaged;
const Token = @import("Lexer.zig").Token;
const Color = @import("Color.zig");
const AstGen = @import("AstGen.zig");
const tw = Color.tw;
const ErrorManager = @import("ErrorManager.zig");
const Ast = @This();
const Lexer = @import("Lexer.zig");
const format_utils = @import("format_utils.zig");
const serializer = @import("serializer.zig");

nodes: MultiArray(Node).Slice,
node_lists: PackedLists(Node.Index, 0),
allocator: Allocator,
tokens: Array(Token),
source: []const u8,

pub const Node = struct {
    start_token: Token.Index,
    end_token: Token.Index,
    data: Data,

    pub const Index = u32;

    pub const NONE: Index = 0;

    const NodeListsIndex = u32;
    const BinaryExpression = struct {
        lhs: Index,
        rhs: Index,
    };

    pub const Data = union(Tag) {
        root: ChildList,

        add: BinaryExpression,
        div: BinaryExpression,
        mod: BinaryExpression,
        mul: BinaryExpression,
        pow: BinaryExpression,
        sub: BinaryExpression,

        eq: BinaryExpression,
        gt: BinaryExpression,
        ge: BinaryExpression,
        lt: BinaryExpression,
        le: BinaryExpression,
        ne: BinaryExpression,

        @"and": BinaryExpression,
        @"or": BinaryExpression,
        xor: BinaryExpression,

        prop_access: BinaryExpression,

        band: BinaryExpression,
        bor: BinaryExpression,
        bshl: BinaryExpression,
        bshr: BinaryExpression,
        bxor: BinaryExpression,

        bnot: NodeIndex,
        neg: NodeIndex,
        not: NodeIndex,

        decrement: NodeIndex,
        increment: NodeIndex,

        @"export": NodeIndex,
        @"extern": NodeIndex,
        @"pub": NodeIndex,
        assign: BinaryExpression,
        ty_assign: BinaryExpression,

        char_literal: TokenIndex,
        false_literal: TokenIndex,
        hex_literal: TokenIndex,
        identifier: TokenIndex,
        number_literal: TokenIndex,
        string_literal: TokenIndex,
        true_literal: TokenIndex,
        list_literal: struct {
            list: NodeListsIndex,
            ty: Index,
        },

        ty_boolean: TokenIndex,
        ty_number: TokenIndex,
        ty_option: TokenIndex,
        ty_string: TokenIndex,
        ty_void: TokenIndex,

        ty_generic: struct {
            name: Index,
            args_list: NodeListsIndex,
        },
        ty_list: struct {
            size_expr: Index,
            ty: Index,
        },
        array_init: struct {
            type: Index,
            items_list: NodeListsIndex,
        },
        ty_array: struct {
            size_expr: Index,
            ty: Index,
        },

        const_decl: Declaration,
        var_decl: Declaration,

        fn_decl: struct {
            proto: Index,
            body: Index,
        },
        fn_proto: struct {
            name: Index,
            params_list: NodeListsIndex,
            ret_type: Index,
        },
        fn_param: FnParam,
        fn_call: struct {
            callee: Index,
            args_list: NodeListsIndex,
        },

        expr: ChildList,
        group: NodeIndex,
        if_expr: struct {
            condition: Index,
            then_branch: Index,
            else_branch: Index,
        },
        ret_expression: NodeIndex,
        block: ChildList,
        while_loop: WhileLoop,
        type_init: struct {
            type: Index,
            field_init_list: NodeListsIndex,
        },

        field_init: struct {
            name: Index,
            value: Index,
        },

        ty_i8: TokenIndex,
        ty_i16: TokenIndex,
        ty_i32: TokenIndex,
        ty_i64: TokenIndex,
        ty_i128: TokenIndex,
        ty_i256: TokenIndex,

        ty_u8: TokenIndex,
        ty_u16: TokenIndex,
        ty_u32: TokenIndex,
        ty_u64: TokenIndex,
        ty_u128: TokenIndex,
        ty_u256: TokenIndex,
        ty_usize: TokenIndex,

        ty_f32: TokenIndex,
        ty_f64: TokenIndex,

        const TokenIndex = struct {
            token: Token.Index,
        };
        const NodeIndex = struct {
            node: Index,
        };
        const Declaration = struct {
            name: Index,
            type: Index,
            value: Index,
        };
        // binary_expression: BinaryExpression,
        // unary_expression: Index,
        // literal: Token.Index,
        // children_list: NodeListsIndex,
        // if_expression: struct {
        //     condition: Index,
        //     then_branch: Index,
        //     else_branch: Index,
        // },
        // declaration: struct {
        //     name: Token.Index,
        //     ty: Index,
        //     value: Index,
        // },
        // ret_expression: Index,
        // increment_expression: Index,
        // decrement_expression: Index,
        // fn_decl: struct {
        //     proto: Index,
        //     body: Index,
        // },
        // group: Index,
        // fn_proto: struct {
        //     name: Index,
        //     params_list: NodeListsIndex,
        //     ret_ty: Index,
        // },
        // fn_param: FnParam,
        // fn_call: struct {
        //     callee: Index,
        //     args_list: NodeListsIndex,
        // },
        // ty_generic: struct {
        //     name: Token.Index,
        //     args_list: NodeListsIndex,
        // },
        // while_loop: WhileLoop,

        pub const ChildList = struct {
            list: NodeListsIndex,
        };
        pub const FnParam = struct {
            name: Token.Index,
            type: Index,
        };
    };
    pub const Visibility = enum {
        private,
        @"pub",
        @"export",
    };
    pub const Tag = enum {
        root,
        // Binary operators
        // - Math
        add,
        div,
        mod,
        mul,
        pow,
        sub,

        // - Comparison
        eq,
        gt,
        ge,
        lt,
        le,
        ne,

        // - Logical
        @"and",
        @"or",
        xor,

        // - Property access
        prop_access, // a.b

        // Bitwise operators
        band,
        bor,
        bshl,
        bshr,
        bxor,

        // Unary operators
        bnot,
        neg,
        not,
        // Postfix unary operators
        decrement,
        increment,

        @"export",
        @"extern",
        @"pub",
        assign,
        ty_assign,

        // Literals
        char_literal,
        false_literal,
        hex_literal,
        identifier,
        number_literal,
        string_literal,
        true_literal,
        list_literal,

        ty_boolean,
        ty_number,
        ty_option,
        ty_string,
        ty_void,

        ty_generic,
        ty_list,
        array_init,
        ty_array,
        const_decl,
        var_decl,

        // fn
        fn_decl,
        fn_proto,
        fn_param,
        fn_call,

        // Statements
        expr,
        group,
        if_expr,
        ret_expression,
        block,
        while_loop,
        type_init,
        field_init,

        ty_i8,
        ty_i16,
        ty_i32,
        ty_i64,
        ty_i128,
        ty_i256,

        ty_u8,
        ty_u16,
        ty_u32,
        ty_u64,
        ty_u128,
        ty_u256,
        ty_usize,

        ty_f32,
        ty_f64,
    };

    pub const WhileLoop = struct {
        condition: Index,
        body: Index,
    };
};
pub fn parse(allocator: Allocator, errors: *ErrorManager, source: []const u8) !Ast {
    var ast_gen = AstGen{
        .allocator = allocator,
        .errors = errors,
        .tokens = .{},
        .node_lists = .{},
        .nodes = .{},
        .source = source,
    };
    var lexer = Lexer.init(source);

    while (lexer.next()) |token| {
        try ast_gen.tokens.append(ast_gen.allocator, token);
    }

    try ast_gen.parseRoot();
    return Ast{
        .nodes = ast_gen.nodes.slice(),
        .node_lists = ast_gen.node_lists,
        .tokens = ast_gen.tokens,
        .allocator = allocator,
        .source = source,
    };
}
pub fn getNodeSlice(self: *Ast, node_i: Node.Index) []const u8 {
    const start_token_index = self.nodes.items(.start_token)[node_i];
    const end_token_index = self.nodes.items(.end_token)[node_i];
    const start_token = self.tokens.items[start_token_index];
    const end_token = self.tokens.items[@min(end_token_index, self.tokens.items.len - 1)];
    return self.source[start_token.start..end_token.end];
}
pub fn deinit(self: *Ast) void {
    self.nodes.deinit(self.allocator);
    self.node_lists.deinit(self.allocator);
    self.tokens.deinit(self.allocator);
}
const RAINBOW = [7]Color{
    tw.red_200,
    tw.orange_200,
    tw.yellow_200,
    tw.green_200,
    tw.blue_200,
    tw.indigo_200,
    tw.violet_200,
};
const FormatOptions = struct {
    color: bool = true,
    indent_size: usize = 2,
    show_node_index: bool = true,
    show_slice: bool = true,
    show_token_range: bool = true,
};
pub fn getTokenSlice(self: *Ast, token_i: Token.Index) []const u8 {
    if (token_i >= self.tokens.items.len) {
        return "";
    }
    const token = self.tokens.items[token_i];
    return self.source[token.start..token.end];
}
pub fn getNodeStartToken(self: *Ast, node_i: Node.Index) Token.Index {
    return self.nodes.items(.start_token)[node_i];
}
pub fn getNodeEndToken(self: *Ast, node_i: Node.Index) Token.Index {
    return self.nodes.items(.end_token)[node_i];
}
pub fn getNodeData(self: *Ast, node_i: Node.Index) *Node.Data {
    return &self.nodes.items(.data)[node_i];
}
pub fn getNodeTag(self: *Ast, node_i: Node.Index) Node.Tag {
    return std.meta.activeTag(self.getNodeData(node_i).*);
}
pub fn getNodeTags(self: *Ast) []const Node.Tag {
    return self.nodes.items(.tag);
}
pub fn nodeIs(self: *Ast, node_i: Node.Index, tag: Node.Tag) bool {
    return self.nodes.items(.tag)[node_i] == tag;
}

pub fn format(self: *Ast, writer: std.io.AnyWriter, node: Node.Index, options: FormatOptions) !void {
    _ = options; // autofix
    try self.formatNode(writer, node, 0, .{});
    // try self.format_inner(writer, node, 0, options);
}

fn format_inner(self: *Ast, writer: std.io.AnyWriter, node: Node.Index, indent: usize, options: FormatOptions) !void {
    if (node == 0 and indent > 0) return;
    const tag: Node.Tag = self.getNodeTag(node);
    const data: *Node.Data = self.getNodeData(node);
    const indentOptions: format_utils.IndentOptions = .{
        .rainbow = options.color,
        .size = options.indent_size,
    };
    const color_options: Color.FormatColorOptions = .{ .color = options.color };
    try format_utils.writeIndent(writer, indent, indentOptions);
    if (options.show_node_index) try tw.blue_400.brighter(-0.3)
        .print(
        writer,
        "[{d}]",
        .{node},
        color_options,
    );
    try tw.blue_400.brighter(-0.3)
        .print(
        writer,
        "[{d}-{d}]",
        .{ self.getNodeStartToken(node), self.getNodeEndToken(node) },
        color_options,
    );
    try tw.blue_400.bold().print(
        writer,
        "{s}: ",
        .{@tagName(tag)},
        color_options,
    );

    switch (data.*) {
        .block,
        .root,
        .expr,
        => |children_list| {
            _ = try writer.write("\n");
            var iter = self.node_lists.iterList(children_list.list);
            while (iter.next()) |child| {
                try self.format_inner(writer, child, indent + 1, options);
            }
        },
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .pow,
        .eq,
        .ne,
        .lt,
        .le,
        .gt,
        .ge,
        .band,
        .bor,
        .bxor,
        .bshl,
        .bshr,
        .@"or",
        .@"and",
        .xor,
        .assign,
        .ty_assign,
        .prop_access,
        => |bin_op| {
            _ = try writer.write("\n");
            try self.format_inner(writer, bin_op.lhs, indent + 1, options);
            try self.format_inner(writer, bin_op.rhs, indent + 1, options);
        },
        .@"extern",
        .@"pub",
        .@"export",
        .increment,
        .decrement,
        .ret_expression,
        .group,
        .bnot,
        .not,
        .neg,
        => |un| {
            _ = try writer.write("\n");
            try self.format_inner(writer, un.node, indent + 1, options);
        },
        .const_decl, .var_decl => |decl| {
            _ = try writer.write("\n");
            try self.format_inner(writer, decl.name, indent + 1, options);
            if (decl.type != 0) {
                try format_utils.writeIndent(writer, indent + 1, indentOptions);
                try tw.gray_400.write(writer, "[type]:\n", color_options);
                try self.format_inner(writer, decl.type, indent + 2, options);
            }
            if (decl.value != 0) {
                try format_utils.writeIndent(writer, indent + 1, indentOptions);
                try tw.gray_400.write(writer, "[init]:\n", color_options);
                try self.format_inner(writer, decl.value, indent + 2, options);
            }
        },
        .if_expr => |expr| {
            _ = try writer.write("\n");
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[condition]:\n", color_options);
            try self.format_inner(writer, expr.condition, indent + 2, options);
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[then]:\n", color_options);
            try self.format_inner(writer, expr.then_branch, indent + 2, options);
            if (expr.else_branch != 0) {
                try format_utils.writeIndent(writer, indent + 1, indentOptions);
                try tw.gray_400.write(writer, "[else]:\n", color_options);
                try self.format_inner(writer, expr.else_branch, indent + 2, options);
            }
        },

        .fn_call => |call| {
            _ = try writer.write("\n");
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[callee]:\n", color_options);
            try self.format_inner(writer, call.callee, indent + 2, options);
            // _ = try writer.write("\n");
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            var iter = self.node_lists.iterList(call.args_list);
            var i: usize = 0;
            while (iter.next()) |arg| {
                if (i == 0) try tw.gray_400.write(writer, "[args]:\n", color_options);
                try self.format_inner(writer, arg, indent + 2, options);
                i += 1;
            }
        },
        // .group => {
        //     _ = try writer.write("\n");
        //     try self.format_inner(writer, data.group.list, indent + 1, options);
        // // },
        .ty_boolean,
        .ty_number,
        .ty_string,
        .ty_void,
        .ty_f32,
        .ty_f64,
        .ty_i32,
        .ty_i64,
        .ty_option,
        .number_literal,
        .true_literal,
        .false_literal,
        .string_literal,
        .identifier,
        .char_literal,
        .hex_literal,
        => |tok| {
            try tw.yellow_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(tok.token)}, color_options);
        },
        .ty_generic => |gen| {
            // try tw.cyan_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.ty_generic.name)});
            _ = try writer.write("\n");

            try self.format_inner(writer, gen.name, indent + 1, options);
            var iter = self.node_lists.iterList(gen.args_list);
            while (iter.next()) |arg| {
                if (iter.index == 1) {
                    try format_utils.writeIndent(writer, indent + 1, indentOptions);
                    try tw.gray_400.write(writer, "[args]:\n", color_options);
                }
                try self.format_inner(writer, arg, indent + 2, options);
            }
        },

        .ty_list => |list| {
            try tw.gray_400.write(writer, "[size]:\n", color_options);
            try self.format_inner(writer, list.size_expr, indent + 2, options);
            try tw.gray_400.write(writer, "[type]:\n", color_options);
            try self.format_inner(writer, list.ty, indent + 2, options);
        },

        .fn_decl => |decl| {
            _ = try writer.write("\n");

            try self.format_inner(writer, decl.proto, indent + 1, options);
            if (decl.body != 0) {
                try format_utils.writeIndent(writer, indent + 1, indentOptions);
                try tw.gray_400.write(writer, "[body]:\n", color_options);
                try self.format_inner(writer, decl.body, indent + 2, options);
            }
        },
        .fn_proto => |proto| {
            _ = try writer.write("\n");
            try self.format_inner(writer, proto.name, indent + 1, options);
            if (proto.params_list != 0) {
                var iter = self.node_lists.iterList(proto.params_list);
                while (iter.next()) |param| {
                    if (iter.index == 1) {
                        try format_utils.writeIndent(writer, indent + 1, indentOptions);
                        try tw.gray_400.write(writer, "[params]:\n", color_options);
                    }
                    try self.format_inner(writer, param, indent + 2, options);
                }
            }
            if (proto.ret_type != 0) {
                try format_utils.writeIndent(writer, indent + 1, indentOptions);
                try tw.gray_400.write(writer, "[ret]:\n", color_options);
                try self.format_inner(writer, proto.ret_type, indent + 2, options);
            }
        },
        .fn_param => |param| {
            _ = try writer.write("\n");
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[name]:\n", color_options);
            try self.format_inner(writer, param.name, indent + 2, options);
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[type]:\n", color_options);
            try self.format_inner(writer, param.type, indent + 2, options);
        },
        // // .const_decl, .var_decl => {
        // //     _ = try writer.write("\n");
        // //     // if (data.var_decl.ty != 0) {
        // //     //     try writeIndent(writer, indent + 1, .{});
        // //     //     try tw.gray_400.print(writer, "[type]:\n", .{});
        // //     //     try self.format_inner(writer, data.var_decl.ty, indent + 2, options);
        // //     // }
        // //     if (data.var_decl.value != 0) {
        // //         try writeIndent(writer, indent + 1, .{});
        // //         try tw.gray_400.print(writer, "[value]:\n", .{});
        // //         try self.format_inner(writer, data.var_decl.value, indent + 2, options);
        // //     }
        // //     // try writeIndent(writer, indent + 1, .{});
        // //     // try tw.gray_400.print(writer, "[name]:\n", .{});
        // //     // try self.format_inner(writer, data.var_decl.name, indent + 2, options);
        // //     // try writeIndent(writer, indent + 1, .{});
        // //     // try tw.gray_400.print(writer, "[type]:\n", .{});
        // //     // try self.format_inner(writer, data.var_decl.ty, indent + 2, options);
        // //     // try writeIndent(writer, indent + 1, .{});
        // //     // try tw.gray_400.print(writer, "[value]:\n", .{});
        // // },
        .while_loop => |loop| {
            _ = try writer.write("\n");
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[condition]:\n", color_options);
            try self.format_inner(writer, loop.condition, indent + 2, options);
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[body]:\n", color_options);
            try self.format_inner(writer, loop.body, indent + 2, options);
        },
        .list_literal => |list| {
            try tw.gray_400.write(writer, "[list]:\n", color_options);
            var iter = self.node_lists.iterList(list.list);
            while (iter.next()) |item| {
                try self.format_inner(writer, item, indent + 2, options);
            }
        },
        .array_init => |init| {
            _ = try writer.write("\n");
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[size]:\n", color_options);
            try self.format_inner(writer, init.size_expr, indent + 2, options);
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[init]:\n", color_options);
            try self.format_inner(writer, init.init_expr, indent + 2, options);
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[type]:\n", color_options);
            try self.format_inner(writer, init.type, indent + 2, options);
        },

        // .ty_i32,
        // .ty_i64,
        // .ty_f32,
        // .ty_f64,
        // => {
        //     try tw.cyan_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.literal)}, color_options);
        // },
        // else => {
        //     _ = try writer.write("\n");
        // },
    }
}

fn writeIndent(writer: std.io.AnyWriter, indent: usize, options: struct {
    rainbow: bool = true,
    size: usize = 2,
}) !void {
    if (options.rainbow) {
        for (0..indent) |color| {
            try RAINBOW[color % RAINBOW.len].brighter(-0.3).write(writer, "┆");
            try writer.writeByteNTimes(' ', options.size - 1);
        }
    } else {
        for (0..indent) |_| {
            _ = try writer.write("┆");
            try writer.writeByteNTimes(' ', options.size - 1);
        }
    }
}
const fmt = @import("./format_utils.zig");
pub fn formatNodeSlice(
    self: *Ast,
    writer: std.io.AnyWriter,
    node_index: Node.Index,
) !void {
    const slice = self.getNodeSlice(node_index);
    if (slice.len < 15) {
        for (slice) |c| {
            switch (c) {
                '\n' => {},
                else => try writer.writeByte(c),
            }
        }
        return;
    }
    // const start_token = self.nodes.items(.start_token)[node_index];
    // const end_token = self.nodes.items(.end_token)[node_index];
    // const start_slice = self.getTokenSlice(start_token);
    // const end_slice = self.getTokenSlice(end_token);
    // try writer.print("{s}...{s}", .{ start_slice, end_slice });
    const start = slice[0..7];
    const end = slice[slice.len - 7 ..];
    for (start) |c| {
        switch (c) {
            '\n' => {},
            else => try writer.writeByte(c),
        }
    }
    try writer.print("...", .{});
    for (end) |c| {
        switch (c) {
            '\n' => {},
            else => try writer.writeByte(c),
        }
    }
}
pub fn formatNode(
    self: *Ast,
    writer: std.io.AnyWriter,
    node_index: Node.Index,
    depth: usize,
    options: FormatOptions,
) !void {
    const indent_options = fmt.IndentOptions{
        .rainbow = options.color,
        .size = options.indent_size,
        .indent_guide_char = ' ',
        .indent_guides = false,
    };
    const node = self.nodes.get(node_index);
    if (node_index == 0 and depth != 0) {
        try fmt.writeIndent(writer, depth, indent_options);
        try writer.writeAll("NONE\n");
        return;
    }

    try fmt.writeIndent(writer, depth, indent_options);

    if (options.show_token_range)
        try writer.print("[{d}-{d}]", .{ node.start_token, node.end_token });

    try writer.print(
        ".{s}",
        .{
            // node.start_token,
            // node.end_token,
            @tagName(std.meta.activeTag(node.data)),
        },
    );
    if (options.show_slice) {
        try writer.print(" `", .{});
        try formatNodeSlice(self, writer, node_index);
        try writer.print("`", .{});
    }
    try writer.print("\n", .{});

    switch (node.data) {
        inline else => |data| {
            const T = @TypeOf(data);
            const fields = comptime std.meta.fields(T);
            inline for (fields) |field| {
                try fmt.writeIndent(writer, depth + 1, indent_options);
                const value = @field(data, field.name);

                try writer.print("[{s}]:", .{field.name});
                if (std.mem.endsWith(u8, field.name, "list")) {
                    try writer.print("\n", .{});
                    var iter = self.node_lists.iterList(value);
                    while (iter.next()) |child| {
                        try self.formatNode(writer, child, depth + 2, options);
                    }
                    // try fmt.writeIndent(writer, depth + 1, indent_options);
                    // try writer.writeAll("\n");
                    // try writer.print("}}\n", .{});
                } else if (std.mem.eql(u8, field.name, "token")) {
                    const token = self.tokens.items[value];
                    try writer.print(".{s} '{s}'\n", .{ @tagName(token.tag), self.getTokenSlice(value) });
                } else {
                    try writer.print("\n", .{});
                    try self.formatNode(writer, value, depth + 2, options);
                    // try writer.print("{s}\n", .{value});
                }

                // try writer.print("\n", .{});
            }
            // try writer.print("{s}: {s}\n", .{ @tagName(std.meta.activeTag(data)), data });
        },
        // .block, .root, .expr => |children_list| {
        //     var iter = self.node_lists.iterList(children_list.list);
        //     while (iter.next()) |child| {
        //         try self.formatNode(writer, child, depth + 1);
        //     }
        // },
    }
    // try fmt.writeIndent(writer, depth, indent_options);
    // try writer.print("\n", .{});
    // inline for (fields) |field| {
    //     try writer.writeAll(field.name);
    //     try writer.writeAll(": \n");
    //     // try self.formatNode(writer, node.data.get(field.name), depth + 1);
    // }
}

fn assertMatchTree(allocator: Allocator, source: []const u8, expected: []const u8) !void {
    _ = expected; // autofix
    var errors = try ErrorManager.init(allocator);
    defer errors.deinit();
    var ast = try parse(allocator, &errors, source);
    defer ast.deinit();
    var tree = std.ArrayList(u8).init(allocator);
    defer tree.deinit(allocator);

    try ast.format(tree.writer().any(), 0, .{
        .color = false,
        .indent_size = 2,
    });
}

pub const Navigator = struct {
    ast: *Ast,
    node: Node.Index = 0,
    data: *Node.Data,
    tag: Node.Tag,
    pub fn init(ast: *Ast, node: Node.Index) Navigator {
        return .{
            .ast = ast,
            .node = node,
            .data = ast.getNodeData(node),
            .tag = ast.getNodeTag(node),
        };
    }
    pub fn is(self: *Navigator, tag: Node.Tag) bool {
        return self.ast.getNodeTag(self.node) == tag;
    }
    pub fn accept(self: *Navigator, node_index: Node.Index, tag: Node.Tag) bool {
        const ok = self.ast.getNodeTag(self.node) == tag;
        if (ok) self.node = node_index;
        return ok;
    }
    pub fn assertTag(self: *Navigator, tag: Node.Tag) void {
        const ok = self.is(tag);
        if (!ok) {
            std.debug.panic("expected '{s}' but got '{s}'\n", .{ @tagName(tag), @tagName(self.tag) });
        }
    }
    pub fn move(self: *Navigator, node_index: Node.Index) void {
        self.node = node_index;
        self.data = self.ast.getNodeData(node_index);
        self.tag = self.ast.getNodeTag(node_index);
    }

    // pub fn acceptData(self: *Navigator, comptime tag: Node.Tag) ?*@TypeOf(@field(Node.Data, @tagName(tag))) {
    //     switch (tag) {
    //         inline else => |t| {
    //             if (t == tag) return &@field(self.data, @tagName(tag));
    //         },
    //     }
    //     // if (self.is(tag)) {
    //     //     return @field(self.data, @tagName(tag));
    //     // }
    //     return null;
    // }

    pub fn acceptFork(self: *Navigator, tag: Node.Tag) ?Navigator {
        if (self.is(tag)) {
            return self.fork();
        }
        return null;
    }
    pub fn fork(self: *Navigator) Navigator {
        return Navigator.init(self.ast, self.node);
    }

    pub fn forkTo(self: *Navigator, node_index: Node.Index) Navigator {
        return Navigator.init(self.ast, node_index);
    }
    pub fn getData(self: *Navigator) *Node.Data {
        return self.ast.getNodeData(self.node);
    }
    pub fn getDataOf(self: *Navigator, node: Node.Index) *Node.Data {
        return self.ast.getNodeData(node);
    }
    pub fn getTag(self: *Navigator) Node.Tag {
        return self.ast.getNodeTag(self.node);
    }
    pub fn getStartToken(self: *Navigator) Token.Index {
        return self.ast.getNodeStartToken(self.node);
    }
    pub fn getEndToken(self: *Navigator) Token.Index {
        return self.ast.getNodeEndToken(self.node);
    }
    pub fn getNodeSlice(self: *Navigator) []const u8 {
        return self.ast.getTokenSlice(self.getStartToken());
    }
};

const Patience = @import("./patience_diff.zig");
fn testAst(source: []const u8, node_index: Node.Index, expected: []const u8) !void {
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
    try ast.formatNode(writer, node_index, 0, .{
        .color = false,
        .indent_size = 2,
        .show_node_index = false,
        .show_slice = false,
        .show_token_range = false,
    });

    // std.debug.print("{s}\n", .{arr.items});
    var res = try Patience.diff(
        std.testing.allocator,
        expected,
        arr.items,
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
    // defer res.deinit();
    // try std.testing.expectEqualStrings(expected, arr.items);
}
// test "Ast" {
//     const test_allocator = std.testing.allocator;
//     const file = try std.fs.cwd().openFile("./playground.zig", .{});
//     defer file.close();
//     const source = try file.readToEndAlloc(test_allocator, 1024 * 1024);
//     defer test_allocator.free(source);

//     var errors = try ErrorManager.init(test_allocator);
//     defer errors.deinit();
//     var ast = try Ast.parse(test_allocator, &errors, source);
//     defer ast.deinit();

//     const writer = std.io.getStdErr().writer().any();
//     try ast.formatNode(writer, 0, 0, .{});

// }

test "Ast" {
    // const test_allocator = std.testing.allocator;

    try testAst(
        \\fn main() void {
        \\}
    , 0,
        \\.root
        \\  [list]:
        \\    .fn_decl
        \\      [proto]:
        \\        .fn_proto
        \\          [name]:
        \\            .identifier
        \\              [token]:.identifier 'main'
        \\          [params_list]:
        \\          [ret_type]:
        \\            .ty_void
        \\              [token]:.keyword_void 'void'
        \\      [body]:
        \\        .block
        \\          [list]:
        \\
    );

    try testAst(
        \\fn main(a: i32, b: f32) void {
        \\}
    , 0,
        \\.root
        \\  [list]:
        \\    .fn_decl
        \\      [proto]:
        \\        .fn_proto
        \\          [name]:
        \\            .identifier
        \\              [token]:.identifier 'main'
        \\          [params_list]:
        \\            .fn_param
        \\              [name]:
        \\                .identifier
        \\                  [token]:.identifier 'a'
        \\              [type]:
        \\                .ty_i32
        \\                  [token]:.keyword_i32 'i32'
        \\            .fn_param
        \\              [name]:
        \\                .identifier
        \\                  [token]:.identifier 'b'
        \\              [type]:
        \\                .ty_f32
        \\                  [token]:.keyword_f32 'f32'
        \\          [ret_type]:
        \\            .ty_void
        \\              [token]:.keyword_void 'void'
        \\      [body]:
        \\        .block
        \\          [list]:
        \\
    );

    try testAst(
        \\fn main() i32 {
        \\  return 1 + 2
        \\}
    , 0,
        \\.root
        \\  [list]:
        \\    .fn_decl
        \\      [proto]:
        \\        .fn_proto
        \\          [name]:
        \\            .identifier
        \\              [token]:.identifier 'main'
        \\          [params_list]:
        \\          [ret_type]:
        \\            .ty_i32
        \\              [token]:.keyword_i32 'i32'
        \\      [body]:
        \\        .block
        \\          [list]:
        \\            .ret_expression
        \\              [node]:
        \\                .add
        \\                  [lhs]:
        \\                    .number_literal
        \\                      [token]:.number_literal '1'
        \\                  [rhs]:
        \\                    .number_literal
        \\                      [token]:.number_literal '2'
        \\
    );

    try testAst(
        \\fn main() i32 {
        \\  if (1 > 2) {
        \\    return 1
        \\  }
        \\  return 2
        \\}
    , 0,
        \\.root
        \\  [list]:
        \\    .fn_decl
        \\      [proto]:
        \\        .fn_proto
        \\          [name]:
        \\            .identifier
        \\              [token]:.identifier 'main'
        \\          [params_list]:
        \\          [ret_type]:
        \\            .ty_i32
        \\              [token]:.keyword_i32 'i32'
        \\      [body]:
        \\        .block
        \\          [list]:
        \\            .if_expr
        \\              [condition]:
        \\                .gt
        \\                  [lhs]:
        \\                    .number_literal
        \\                      [token]:.number_literal '1'
        \\                  [rhs]:
        \\                    .number_literal
        \\                      [token]:.number_literal '2'
        \\              [then_branch]:
        \\                .block
        \\                  [list]:
        \\                    .ret_expression
        \\                      [node]:
        \\                        .number_literal
        \\                          [token]:.number_literal '1'
        \\              [else_branch]:
        \\                NONE
        \\            .ret_expression
        \\              [node]:
        \\                .number_literal
        \\                  [token]:.number_literal '2'
        \\
    );
    try testAst(
        \\fn main() i32 {
        \\  if (1 > 2) return 1
        \\  return 2
        \\}
    , 0,
        \\.root
        \\  [list]:
        \\    .fn_decl
        \\      [proto]:
        \\        .fn_proto
        \\          [name]:
        \\            .identifier
        \\              [token]:.identifier 'main'
        \\          [params_list]:
        \\          [ret_type]:
        \\            .ty_i32
        \\              [token]:.keyword_i32 'i32'
        \\      [body]:
        \\        .block
        \\          [list]:
        \\            .if_expr
        \\              [condition]:
        \\                .gt
        \\                  [lhs]:
        \\                    .number_literal
        \\                      [token]:.number_literal '1'
        \\                  [rhs]:
        \\                    .number_literal
        \\                      [token]:.number_literal '2'
        \\              [then_branch]:
        \\                .ret_expression
        \\                  [node]:
        \\                    .number_literal
        \\                      [token]:.number_literal '1'
        \\              [else_branch]:
        \\                NONE
        \\            .ret_expression
        \\              [node]:
        \\                .number_literal
        \\                  [token]:.number_literal '2'
        \\
    );
    try testAst(
        \\fn main() i32 {
        \\  while (1 > 2) {
        \\    return 1
        \\  }
        \\  return 2
        \\}
    , 0,
        \\.root
        \\  [list]:
        \\    .fn_decl
        \\      [proto]:
        \\        .fn_proto
        \\          [name]:
        \\            .identifier
        \\              [token]:.identifier 'main'
        \\          [params_list]:
        \\          [ret_type]:
        \\            .ty_i32
        \\              [token]:.keyword_i32 'i32'
        \\      [body]:
        \\        .block
        \\          [list]:
        \\            .while_loop
        \\              [condition]:
        \\                .gt
        \\                  [lhs]:
        \\                    .number_literal
        \\                      [token]:.number_literal '1'
        \\                  [rhs]:
        \\                    .number_literal
        \\                      [token]:.number_literal '2'
        \\              [body]:
        \\                .block
        \\                  [list]:
        \\                    .ret_expression
        \\                      [node]:
        \\                        .number_literal
        \\                          [token]:.number_literal '1'
        \\            .ret_expression
        \\              [node]:
        \\                .number_literal
        \\                  [token]:.number_literal '2'
        \\
    );

    try testAst(
        \\fn main() []i32 {}
    , 3,
        \\.ty_array
        \\  [size_expr]:
        \\    NONE
        \\  [ty]:
        \\    .ty_i32
        \\      [token]:.keyword_i32 'i32'
        \\
    );
    try testAst(
        \\fn main(a: [123]i32) void {}
    , 6,
        \\.ty_array
        \\  [size_expr]:
        \\    .number_literal
        \\      [token]:.number_literal '123'
        \\  [ty]:
        \\    .ty_i32
        \\      [token]:.keyword_i32 'i32'
        \\
    );

    try testAst(
        \\fn main() void {
        \\  const a: [123]i32 = _{1, 2, 3};
        \\}
    , 0,
        \\.root
        \\  [list]:
        \\    .fn_decl
        \\      [proto]:
        \\        .fn_proto
        \\          [name]:
        \\            .identifier
        \\              [token]:.identifier 'main'
        \\          [params_list]:
        \\          [ret_type]:
        \\            .ty_void
        \\              [token]:.keyword_void 'void'
        \\      [body]:
        \\        .block
        \\          [list]:
        \\            .const_decl
        \\              [name]:
        \\                .identifier
        \\                  [token]:.identifier 'a'
        \\              [type]:
        \\                .ty_array
        \\                  [size_expr]:
        \\                    .number_literal
        \\                      [token]:.number_literal '123'
        \\                  [ty]:
        \\                    .ty_i32
        \\                      [token]:.keyword_i32 'i32'
        \\              [value]:
        \\                .array_init
        \\                  [type]:
        \\                    .identifier
        \\                      [token]:.identifier '_'
        \\                  [items_list]:
        \\                    .number_literal
        \\                      [token]:.number_literal '1'
        \\                    .number_literal
        \\                      [token]:.number_literal '2'
        \\                    .number_literal
        \\                      [token]:.number_literal '3'
        \\
    );
    try testAst(
        \\fn main() void {
        \\  const a = [123]i32{1, 2, 3};
        \\}
    , 0,
        \\.root
        \\  [list]:
        \\    .fn_decl
        \\      [proto]:
        \\        .fn_proto
        \\          [name]:
        \\            .identifier
        \\              [token]:.identifier 'main'
        \\          [params_list]:
        \\          [ret_type]:
        \\            .ty_void
        \\              [token]:.keyword_void 'void'
        \\      [body]:
        \\        .block
        \\          [list]:
        \\            .const_decl
        \\              [name]:
        \\                .identifier
        \\                  [token]:.identifier 'a'
        \\              [type]:
        \\                NONE
        \\              [value]:
        \\                .array_init
        \\                  [type]:
        \\                    .ty_array
        \\                      [size_expr]:
        \\                        .number_literal
        \\                          [token]:.number_literal '123'
        \\                      [ty]:
        \\                        .ty_i32
        \\                          [token]:.keyword_i32 'i32'
        \\                  [items_list]:
        \\                    .number_literal
        \\                      [token]:.number_literal '1'
        \\                    .number_literal
        \\                      [token]:.number_literal '2'
        \\                    .number_literal
        \\                      [token]:.number_literal '3'
        \\
    );
    // try testAst(
    //     \\fn main() void {
    //     \\  const a = [_]i32{0 ~ 20};
    //     \\}
    // , 0,
    //     \\.root
    //     \\  [list]:
    //     \\    .fn_decl
    //     \\      [proto]:
    //     \\        .fn_proto
    //     \\          [name]:
    //     \\            .identifier
    //     \\              [token]:.identifier 'main'
    //     \\          [params_list]:
    //     \\          [ret_type]:
    //     \\            .ty_void
    //     \\              [token]:.keyword_void 'void'
    //     \\      [body]:
    //     \\        .block
    //     \\          [list]:
    //     \\            .const_decl
    //     \\              [name]:
    //     \\                .identifier
    //     \\                  [token]:.identifier 'a'
    //     \\              [type]:
    //     \\                NONE
    //     \\              [value]:
    //     \\                .array_init
    //     \\                  [type]:
    //     \\                    .ty_array
    //     \\                      [size_expr]:
    //     \\                        .number_literal
    //     \\                          [token]:.number_literal '123'
    //     \\                      [ty]:
    //     \\                        .ty_i32
    //     \\                          [token]:.keyword_i32 'i32'
    //     \\                  [items_list]:
    //     \\                    .number_literal
    //     \\                      [token]:.number_literal '1'
    //     \\                    .number_literal
    //     \\                      [token]:.number_literal '2'
    //     \\                    .number_literal
    //     \\                      [token]:.number_literal '3'
    //     \\
    // );
}
