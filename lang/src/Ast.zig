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

nodes: MultiArray(Node).Slice,
node_lists: PackedLists(Node.Index, 0),
allocator: Allocator,
tokens: Array(Token),
source: []const u8,

pub const Node = struct {
    tag: Tag,
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
    const UnaryExpression = struct {
        operand: Index,
    };

    pub const Data = union {
        binary_expression: BinaryExpression,
        unary_expression: Index,
        literal: Token.Index,
        children_list: NodeListsIndex,
        if_expression: struct {
            condition: Index,
            then_branch: Index,
            else_branch: Index,
        },
        ret_expression: Index,
        increment_expression: Index,
        decrement_expression: Index,
        fn_decl: struct {
            proto: Index,
            body: Index,
        },
        group: Index,
        fn_proto: struct {
            name: Index,
            params_list: NodeListsIndex,
            ret_ty: Index,
        },
        fn_param: FnParam,
        // var_decl: struct {
        //     name: Index,
        //     value: Index,
        // },
        fn_call: struct {
            callee: Index,
            args_list: NodeListsIndex,
        },
        ty_generic: struct {
            name: Token.Index,
            args_list: NodeListsIndex,
        },
        while_loop: WhileLoop,

        pub const FnParam = struct {
            name: Token.Index,
            ty: Index,
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
        sub,
        mul,
        div,
        mod,
        pow,

        // - Comparison
        eq,
        ne,
        lt,
        gt,
        lte,
        gte,

        // - Logical
        @"and",
        @"or",
        xor,

        // - Property access
        prop_access, // a.b

        // Bitwise operators
        band,
        bor,
        bxor,
        bshl,
        bshr,

        // Unary operators
        bnot,
        neg,
        not,
        // Postfix unary operators
        increment,
        decrement,

        @"pub",
        @"extern",
        @"export",
        assign,
        ty_assign,

        // Literals
        number_literal,
        string_literal,
        char_literal,
        hex_literal,
        true_literal,
        false_literal,
        identifier,

        ty_number,
        ty_string,
        ty_boolean,
        ty_option,
        ty_void,
        ty_generic,

        var_decl,
        const_decl,

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
    return self.nodes.items(.tag)[node_i];
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
    const tag: Node.Tag = self.nodes.items(.tag)[node];
    const data: Node.Data = self.nodes.items(.data)[node];
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

    switch (tag) {
        .block, .root => {
            _ = try writer.write("\n");
            var iter = self.node_lists.iterList(data.children_list);
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
        .lte,
        .gt,
        .gte,
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
        => {
            _ = try writer.write("\n");
            try self.format_inner(writer, data.binary_expression.lhs, indent + 1, options);
            try self.format_inner(writer, data.binary_expression.rhs, indent + 1, options);
        },
        .prop_access => {
            _ = try writer.write("\n");
            try self.format_inner(writer, data.binary_expression.lhs, indent + 1, options);
            try self.format_inner(writer, data.binary_expression.rhs, indent + 1, options);
        },
        .@"extern", .@"pub", .@"export", .const_decl, .var_decl => {
            _ = try writer.write("\n");
            try self.format_inner(writer, data.unary_expression, indent + 1, options);
        },
        .increment, .decrement => {
            _ = try writer.write("\n");
            try self.format_inner(writer, data.unary_expression, indent + 1, options);
        },
        .if_expr => {
            _ = try writer.write("\n");
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[condition]:\n", color_options);
            try self.format_inner(writer, data.if_expression.condition, indent + 2, options);
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[then]:\n", color_options);
            try self.format_inner(writer, data.if_expression.then_branch, indent + 2, options);
            if (data.if_expression.else_branch != 0) {
                try format_utils.writeIndent(writer, indent + 1, indentOptions);
                try tw.gray_400.write(writer, "[else]:\n", color_options);
                try self.format_inner(writer, data.if_expression.else_branch, indent + 2, options);
            }
        },
        .ret_expression => {
            _ = try writer.write("\n");
            try self.format_inner(writer, data.ret_expression, indent + 1, options);
        },
        .fn_call => {
            _ = try writer.write("\n");
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[callee]:\n", color_options);
            try self.format_inner(writer, data.fn_call.callee, indent + 2, options);
            // _ = try writer.write("\n");
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            var iter = self.node_lists.iterList(data.fn_call.args_list);
            var i: usize = 0;
            while (iter.next()) |arg| {
                if (i == 0) try tw.gray_400.write(writer, "[args]:\n", color_options);
                try self.format_inner(writer, arg, indent + 2, options);
                i += 1;
            }
        },
        .group => {
            _ = try writer.write("\n");
            try self.format_inner(writer, data.group, indent + 1, options);
        },
        .identifier => {
            try tw.yellow_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.literal)}, color_options);
        },
        .number_literal => {
            try tw.green_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.literal)}, color_options);
        },
        .true_literal, .false_literal => {
            try tw.pink_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.literal)}, color_options);
        },
        .string_literal => {
            try tw.blue_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.literal)}, color_options);
        },

        .ty_boolean,
        .ty_number,
        .ty_string,
        .ty_void,
        => {
            try tw.cyan_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.literal)}, color_options);
        },
        .ty_generic => {
            // try tw.cyan_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.ty_generic.name)});
            _ = try writer.write("\n");

            try self.format_inner(writer, data.ty_generic.name, indent + 1, options);
            var iter = self.node_lists.iterList(data.ty_generic.args_list);
            while (iter.next()) |arg| {
                if (iter.index == 1) {
                    try format_utils.writeIndent(writer, indent + 1, indentOptions);
                    try tw.gray_400.write(writer, "[args]:\n", color_options);
                }
                try self.format_inner(writer, arg, indent + 2, options);
            }
        },

        .fn_decl => {
            _ = try writer.write("\n");

            try self.format_inner(writer, data.fn_decl.proto, indent + 1, options);
            if (data.fn_decl.body != 0) {
                try format_utils.writeIndent(writer, indent + 1, indentOptions);
                try tw.gray_400.write(writer, "[body]:\n", color_options);
                try self.format_inner(writer, data.fn_decl.body, indent + 2, options);
            }
        },
        .fn_proto => {
            _ = try writer.write("\n");
            try self.format_inner(writer, data.fn_proto.name, indent + 1, options);
            if (data.fn_proto.params_list != 0) {
                var iter = self.node_lists.iterList(data.fn_proto.params_list);
                while (iter.next()) |param| {
                    if (iter.index == 1) {
                        try format_utils.writeIndent(writer, indent + 1, indentOptions);
                        try tw.gray_400.write(writer, "[params]:\n", color_options);
                    }
                    try self.format_inner(writer, param, indent + 2, options);
                }
            }
            if (data.fn_proto.ret_ty != 0) {
                try format_utils.writeIndent(writer, indent + 1, indentOptions);
                try tw.gray_400.write(writer, "[ret]:\n", color_options);
                try self.format_inner(writer, data.fn_proto.ret_ty, indent + 2, options);
            }
        },
        .fn_param => {
            _ = try writer.write("\n");
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[name]:\n", color_options);
            try self.format_inner(writer, data.fn_param.name, indent + 2, options);
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[type]:\n", color_options);
            try self.format_inner(writer, data.fn_param.ty, indent + 2, options);
        },
        // .const_decl, .var_decl => {
        //     _ = try writer.write("\n");
        //     // if (data.var_decl.ty != 0) {
        //     //     try writeIndent(writer, indent + 1, .{});
        //     //     try tw.gray_400.print(writer, "[type]:\n", .{});
        //     //     try self.format_inner(writer, data.var_decl.ty, indent + 2, options);
        //     // }
        //     if (data.var_decl.value != 0) {
        //         try writeIndent(writer, indent + 1, .{});
        //         try tw.gray_400.print(writer, "[value]:\n", .{});
        //         try self.format_inner(writer, data.var_decl.value, indent + 2, options);
        //     }
        //     // try writeIndent(writer, indent + 1, .{});
        //     // try tw.gray_400.print(writer, "[name]:\n", .{});
        //     // try self.format_inner(writer, data.var_decl.name, indent + 2, options);
        //     // try writeIndent(writer, indent + 1, .{});
        //     // try tw.gray_400.print(writer, "[type]:\n", .{});
        //     // try self.format_inner(writer, data.var_decl.ty, indent + 2, options);
        //     // try writeIndent(writer, indent + 1, .{});
        //     // try tw.gray_400.print(writer, "[value]:\n", .{});
        // },
        .while_loop => {
            _ = try writer.write("\n");
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[condition]:\n", color_options);
            try self.format_inner(writer, data.while_loop.condition, indent + 2, options);
            try format_utils.writeIndent(writer, indent + 1, indentOptions);
            try tw.gray_400.write(writer, "[body]:\n", color_options);
            try self.format_inner(writer, data.while_loop.body, indent + 2, options);
        },
        .ty_i32,
        .ty_i64,
        .ty_f32,
        .ty_f64,
        => {
            try tw.cyan_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.literal)}, color_options);
        },
        else => {
            _ = try writer.write("\n");
        },
    }
}

// fn writeIndent(writer: std.io.AnyWriter, indent: usize, options: struct {
//     rainbow: bool = true,
//     size: usize = 2,
// }) !void {
//     if (options.rainbow) {
//         for (0..indent) |color| {
//             try RAINBOW[color % RAINBOW.len].brighter(-0.3).write(writer, "┆");
//             try writer.writeByteNTimes(' ', options.size - 1);
//         }
//     } else {
//         for (0..indent) |_| {
//             _ = try writer.write("┆");
//             try writer.writeByteNTimes(' ', options.size - 1);
//         }
//     }
// }

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
test "parse" {
    const test_allocator = std.testing.allocator;
    // const source = "1 + math.add(2,3,4 + 3) * 3";
    // const source = "{if (1) 2 else 3;if (1) 2 else 3;}";
    // const source = "fn a(b: boolean,c:number):Option<number, boolean,Option<boolean>> { 1 + 2 + 3;}";
    const source = "pub const a:Option<int, Option<boolean>> = b * 1;pub const a:Option<int, Option<boolean>> = b * 1";
    // const source =
    //     \\export fn add(a: number, b: number): number {
    //     \\  const c:Option<number> = 1;
    //     \\  return a + b + c;
    //     \\}
    //     \\
    //     \\pub fn test(a: number): number {
    //     \\  return a + 1;
    //     \\}
    //     \\
    // ;
    // const source = "a * (if (1) 2 else 3) + 4 * a.b()";
    // const source = "3 + if (true == false or true) 2 + 2 else if (2) 3 + 3 else 4 + 4";
    var errors = try ErrorManager.init(test_allocator);
    defer errors.deinit();
    var ast = try parse(test_allocator, &errors, source);
    defer ast.deinit();

    // try ast.format(std.io.getStdErr().writer().any(), 0, .{
    //     .color = false,
    //     .indent_size = 2,
    // });

    // std.debug.print("\n", .{});
    // var ast = try Ast.parse(test_allocator, &errors, source);
    // defer ast.deinit();
    // std.debug.print("{any}\n", .{ast.nodes.items});
    // std.debug.print("\n\n", .{});
    // try ast.print(std.io.getStdErr().writer().any(), 0, .{});
    // std.debug.print("\n\n", .{});
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

    pub fn acceptData(self: *Navigator, tag: Node.Tag) ?*Node.Data {
        if (self.is(tag)) {
            return self.data;
        }
        return null;
    }

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
