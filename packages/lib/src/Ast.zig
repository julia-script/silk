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

        ty_boolean: TokenIndex,
        ty_number: TokenIndex,
        ty_option: TokenIndex,
        ty_string: TokenIndex,
        ty_void: TokenIndex,

        ty_generic: struct {
            name: Index,
            args_list: NodeListsIndex,
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

        ty_i32: TokenIndex,
        ty_i64: TokenIndex,
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

        ty_boolean,
        ty_number,
        ty_option,
        ty_string,
        ty_void,

        ty_generic,

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

        ty_i32,
        ty_i64,
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
    show_node_index: bool = false,
};
pub fn getTokenSlice(self: *Ast, token_i: Token.Index) []const u8 {
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
    try self.format_inner(writer, node, 0, options);
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
        // .number_literal => {
        //     try tw.green_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.number_literal.token)}, color_options);
        // },
        // .true_literal, .false_literal => {
        //     try tw.pink_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.boolean_literal.token)}, color_options);
        // },
        // .string_literal => {
        //     try tw.blue_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.string_literal.token)}, color_options);
        // },
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

test "writeJSON" {
    const test_allocator = std.testing.allocator;
    var errors = try ErrorManager.init(test_allocator);
    defer errors.deinit();
    const source =
        \\pub const a:Option<int, Option<boolean>> = b * 1;
        \\pub const a:Option<int, Option<boolean>> = b * 1
    ;
    const stderr = std.io.getStdErr().writer();
    var ast = try parse(test_allocator, &errors, source);
    defer ast.deinit();
    const writer = stderr.any();
    try writer.writeAll("[\n");
    for (0..ast.nodes.len) |i| {
        if (i != 0) try writer.writeAll(",\n");
        try serializer.writeJSON(Node, writer, ast.nodes.get(i), .{
            .lists = &ast.node_lists,
        });
    }
    try writer.writeAll("\n]");
    try writer.writeAll("\n");
    try serializer.writeTsType(Node, "Node", writer);
}
