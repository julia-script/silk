const std = @import("std");
const MultiArray = std.MultiArrayList;
const Token = @import("Lexer.zig").Token;
const ErrorManager = @import("ErrorManager.zig");
const Color = @import("Color.zig");
const tw = Color.tw;
const Lexer = @import("Lexer.zig");
const PackedLists = @import("PackedLists.zig").new;
const Allocator = std.mem.Allocator;
const Array = std.ArrayListUnmanaged;

const assert = std.debug.assert;
const AstGen = @This();

errors: *ErrorManager,
nodes: MultiArray(Node) = .{},
node_lists: PackedLists(Node.Index, 0) = .{},
tokens: Array(Token) = .{},
allocator: Allocator,
token_index: Token.Index = 0,

const AstGenError = error{
    OutOfMemory,
};
const Ast = struct {
    nodes: MultiArray(Node).Slice,
    node_lists: PackedLists(Node.Index, 0),
    allocator: Allocator,
    tokens: Array(Token),
    source: []const u8,

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
    pub fn format(self: *Ast, writer: std.io.AnyWriter, node: Node.Index, options: FormatOptions) !void {
        try self.format_inner(writer, node, 0, options);
    }
    fn format_inner(self: *Ast, writer: std.io.AnyWriter, node: Node.Index, indent: usize, options: FormatOptions) !void {
        if (node == 0 and indent > 0) return;
        const tag = self.nodes.items(.tag)[node];
        const data = self.nodes.items(.data)[node];
        // _ = try writer.writeBytesNTimes("| ", indent);
        try writeIndent(writer, indent, .{});
        if (options.show_node_index) try tw.blue_400.brighter(-0.3).print(writer, "[{d}]", .{node});
        try tw.blue_400.bold().print(writer, "{s}: ", .{@tagName(tag)});
        switch (tag) {
            .root => {
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
            .increment, .decrement => {
                _ = try writer.write("\n");
                try self.format_inner(writer, data.unary_expression, indent + 1, options);
            },
            .if_expr => {
                _ = try writer.write("\n");
                try writeIndent(writer, indent + 1, .{});
                try tw.gray_400.print(writer, "[condition]:\n", .{});
                try self.format_inner(writer, data.if_expression.condition, indent + 2, options);
                try writeIndent(writer, indent + 1, .{});
                try tw.gray_400.print(writer, "[then]:\n", .{});
                try self.format_inner(writer, data.if_expression.then_branch, indent + 2, options);
                if (data.if_expression.else_branch != 0) {
                    try writeIndent(writer, indent + 1, .{});
                    try tw.gray_400.print(writer, "[else]:\n", .{});
                    try self.format_inner(writer, data.if_expression.else_branch, indent + 2, options);
                }
            },
            .ret => {
                _ = try writer.write("\n");
                try self.format_inner(writer, data.ret_expression, indent + 1, options);
            },
            .fn_call => {
                _ = try writer.write("\n");
                try writeIndent(writer, indent + 1, .{});
                try tw.gray_400.print(writer, "[callee]:\n", .{});
                try self.format_inner(writer, data.fn_call.callee, indent + 2, options);
                // _ = try writer.write("\n");
                try writeIndent(writer, indent + 1, .{});
                var iter = self.node_lists.iterList(data.fn_call.args_list);
                var i: usize = 0;
                while (iter.next()) |arg| {
                    if (i == 0) try tw.gray_400.print(writer, "[args]:\n", .{});
                    try self.format_inner(writer, arg, indent + 2, options);
                    i += 1;
                }
            },
            .group => {
                _ = try writer.write("\n");
                try self.format_inner(writer, data.group, indent + 1, options);
            },
            .identifier => {
                try tw.yellow_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.literal)});
            },
            .number_literal => {
                try tw.green_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.literal)});
            },
            .true_literal, .false_literal => {
                try tw.pink_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.literal)});
            },
            .string_literal => {
                try tw.blue_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(data.literal)});
            },

            else => {
                _ = try writer.write("\n");
            },
        }
    }

    pub fn writeIndent(writer: std.io.AnyWriter, indent: usize, options: struct {
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
};

const Node = struct {
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
    const Data = union {
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
        fn_def: struct {
            name: Index,
            proto: Index,
            body: Index,
        },
        group: Index,
        fn_proto: struct {
            params_list: NodeListsIndex,
            ret_ty: Index,
        },
        fn_param: struct {
            name: Index,
            ty: Index,
        },
        fn_call: struct {
            callee: Index,
            args_list: NodeListsIndex,
        },
    };
    const Tag = enum {
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

        // Literals
        number_literal,
        string_literal,
        char_literal,
        hex_literal,
        true_literal,
        false_literal,
        identifier,

        // fn
        fn_def,
        fn_proto,
        fn_param,
        fn_call,

        // Statements
        expr,
        group,
        if_expr,
        ret,
    };
};

pub fn parse(allocator: Allocator, errors: *ErrorManager, source: []const u8) AstGenError!Ast {
    var ast_gen = AstGen{
        .allocator = allocator,
        .errors = errors,
        .tokens = .{},
        .node_lists = .{},
        .nodes = .{},
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
fn pushNode(self: *AstGen, node: Node) AstGenError!Node.Index {
    const index = try self.nodes.addOne(self.allocator);
    self.nodes.set(index, node);
    return @intCast(index);
}
fn reserveNodeIndex(self: *AstGen) AstGenError!Node.Index {
    const index = try self.nodes.addOne(self.allocator);
    return @intCast(index);
}
fn setNode(self: *AstGen, index: Node.Index, node: Node) void {
    self.nodes.set(index, node);
}

/// Token helpers
pub fn nextToken(self: *AstGen) ?Token {
    if (self.token_index >= self.tokens.items.len) return null;
    const token = self.tokens.items[self.token_index];
    self.token_index += 1;
    return token;
}

pub fn consumeToken(self: *AstGen) void {
    self.token_index += 1;
}

pub fn peekToken(self: *AstGen) ?Token {
    if (self.token_index >= self.tokens.items.len) return null;
    return self.tokens.items[self.token_index];
}

pub fn tokenIs(self: *AstGen, tag: Token.Tag) bool {
    if (self.token_index >= self.tokens.items.len) return false;
    return self.tokens.items[self.token_index].tag == tag;
}

pub fn getTokenSlice(self: *AstGen, token_i: Token.Index) []const u8 {
    const token = self.tokens.items[token_i];
    return self.source[token.start..token.end];
}
pub fn getNode(self: *AstGen, index: Node.Index) Node {
    return self.nodes.get(index);
}

fn parseRoot(self: *AstGen) AstGenError!void {
    const index = try self.reserveNodeIndex();
    var children = self.node_lists.new(self.allocator);
    while (self.token_index < self.tokens.items.len) {
        const index_before = self.token_index;
        const node = try self.parseExpression();
        const index_after = self.token_index;
        assert(index_after > index_before);
        if (node == 0) break;
        try children.append(node);
    }
    const children_index = try children.commit();
    std.debug.print("root children: {d}\n", .{children_index});
    self.setNode(index, .{
        .tag = .root,
        .start_token = 0,
        .end_token = @intCast(self.tokens.items.len),
        .data = .{
            .children_list = @intCast(children_index),
        },
    });
}
pub fn parseExpression(self: *AstGen) AstGenError!Node.Index {
    const lhs = try self.parseUnary();
    if (lhs == 0) return 0;
    return self.parseBinaryRhs(0, lhs);
}
fn parseUnary(self: *AstGen) AstGenError!Node.Index {
    const token = self.peekToken() orelse return 0;
    // If the current token is not an operator, it must be a primary expr.
    switch (token.tag) {
        .plus,
        .minus,
        .star,
        .slash,
        .percent,
        .caret,
        .ampersand,
        .pipe,
        .tilde,
        .bang,
        .keyword_or,
        .keyword_and,
        => {},
        else => {
            return try self.parsePrimary();
        },
    }

    const op_token = self.peekToken() orelse return 0;
    self.consumeToken();

    const start_token = self.token_index;
    switch (op_token.tag) {
        .keyword_return => {
            return try self.pushNode(.{
                .tag = .ret,
                .data = .{ .ret_expression = try self.parseExpression() },
                .start_token = start_token,
                .end_token = self.token_index,
            });
        },
        else => {
            return try self.pushNode(.{
                .tag = switch (op_token.tag) {
                    .minus => .neg,
                    .bang => .not,
                    .tilde => .bnot,
                    else => return 0,
                },
                .data = .{ .unary_expression = try self.parseUnary() },
                .start_token = start_token,
                .end_token = self.token_index,
            });
        },
        // .minus => {
        //     return try self.pushNode(.{
        //         .data = .{ .neg = .{
        //             .operand = try self.parseUnary() orelse return null,
        //         } },
        //         .start_token = start_token,
        //         .end_token = self.token_index,
        //     });
        // },
        // .bang => {
        //     return try self.appendNode(.{
        //         .data = .{ .not = .{
        //             .operand = try self.parseUnary() orelse return null,
        //         } },
        //         .start_token = start_token,
        //         .end_token = self.token_index,
        //     });
        // },
        // .tilde => {
        //     return try self.appendNode(.{
        //         .data = .{ .bnot = .{
        //             .operand = try self.parseUnary() orelse return null,
        //         } },
        //         .start_token = start_token,
        //         .end_token = self.token_index,
        //     });
        // },
        // else => {},
    }
}
pub fn parsePrimary(self: *AstGen) AstGenError!Node.Index {
    if (self.peekToken()) |token| {
        switch (token.tag) {
            .eof => {
                self.consumeToken();
                return 0;
            },
            inline .string_literal,
            .number_literal,
            .identifier,
            .keyword_true,
            .keyword_false,
            => |tag| {
                defer self.consumeToken();
                return try self.pushNode(.{
                    .tag = switch (tag) {
                        .string_literal => .string_literal,
                        .number_literal => .number_literal,
                        .identifier => .identifier,
                        .keyword_false => .false_literal,
                        .keyword_true => .true_literal,
                        else => unreachable,
                    },
                    .data = .{ .literal = self.token_index },
                    .start_token = self.token_index,
                    .end_token = self.token_index,
                });
            },
            .l_parenthesis => {
                return try self.parseGroup();
            },
            .keyword_if => {
                return try self.parseIfExpression();
            },

            else => {
                std.debug.print("unexpected {s}", .{@tagName(token.tag)});
            },
        }
    }

    try self.errors.addError(.{
        .tag = .unexpected_token,
        .start = self.token_index,
        .end = self.token_index,
    });
    self.consumeToken();
    return 0;
}

fn getTokenPrecedence(tag: Token.Tag) i8 {
    return switch (tag) {
        // logical
        .keyword_or => 1,
        .keyword_and => 2,
        // bitwise
        .pipe => 3,
        .caret => 4,
        .ampersand => 5,
        // equality
        .double_equal => 6,
        .bang_equal => 6,
        // relational
        .l_angle_bracket => 7,
        .l_angle_bracket_equal => 7,
        .r_angle_bracket => 7,
        .r_angle_bracket_equal => 7,
        // bitwise shift
        .double_l_angle_bracket => 8,
        .double_r_angle_bracket => 8,
        // additive
        .plus => 9,
        .minus => 9,
        // multiplicative
        .star => 10,
        .slash => 10,
        .percent => 10,

        .double_star => 11,

        // unary
        .bang => 12,
        .tilde => 12,
        .dot => 12,

        // .l_parenthesis => 13,
        else => -1,
    };
}
fn parseBinaryRhs(self: *AstGen, expression_precedence: i8, lhs_: Node.Index) AstGenError!Node.Index {
    var lhs = lhs_;
    while (true) {
        const token = self.peekToken() orelse return lhs;

        switch (token.tag) {
            .double_plus, .double_minus => {
                const _lhs = try self.parsePostfixUnary(lhs);
                if (_lhs == 0) return _lhs;
                lhs = _lhs;
                continue;
            },
            .l_parenthesis => {
                const _lhs = try self.parseFnCall(lhs);
                if (_lhs == 0) return _lhs;
                lhs = _lhs;
                continue;
            },
            else => {},
        }

        const tok_prec = getTokenPrecedence(token.tag);

        if (tok_prec < expression_precedence) return lhs;

        const bin_op = self.nextToken() orelse return lhs;

        var rhs = try self.parseUnary();
        if (rhs == 0) return rhs;
        const next_tok = self.peekToken() orelse return lhs;

        const next_precedence = getTokenPrecedence(
            next_tok.tag,
        );
        if (tok_prec < next_precedence) {
            rhs = try self.parseBinaryRhs(tok_prec + 1, rhs);
            if (rhs == 0) return lhs;
        }

        // if (token.tag == .l_parenthesis) {
        //     lhs = try self.parseFnCall(lhs) orelse return null;
        //     std.debug.print("tok: {s}, {s}\n", .{
        //         @tagName(token.tag),
        //         @tagName(self.nodes.items[lhs].data),
        //     });
        //     continue;
        // }
        lhs = try self.makeBinaryExpression(bin_op, lhs, rhs);
        if (lhs == 0) return lhs;
    }
    return lhs;
}
pub fn makeBinaryExpression(self: *AstGen, bin_op_token: Token, lhs: Node.Index, rhs: Node.Index) !Node.Index {
    const start_token = self.nodes.get(lhs).start_token;
    const end_token = self.nodes.get(rhs).end_token;
    return try self.pushNode(.{
        .tag = switch (bin_op_token.tag) {
            .plus => .add,
            .minus => .sub,
            .star => .mul,
            .slash => .div,
            .percent => .mod,
            .double_star => .pow,
            .double_equal => .eq,
            .bang_equal => .ne,
            .l_angle_bracket => .lt,
            .l_angle_bracket_equal => .lte,
            .r_angle_bracket => .gt,
            .r_angle_bracket_equal => .gte,
            .double_l_angle_bracket => .bshl,
            .double_r_angle_bracket => .bshr,
            .ampersand => .band,
            .pipe => .bor,
            .dot => .prop_access,
            .keyword_or => .@"or",
            .keyword_and => .@"and",
            else => unreachable,
        },
        .data = .{ .binary_expression = .{
            .lhs = lhs,
            .rhs = rhs,
        } },
        .start_token = start_token,
        .end_token = end_token,
    });
    // return try self.appendNode(.{
    //     .data = switch (bin_op_token.tag) {
    //         .plus => .{ .add = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .minus => .{ .sub = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .star => .{ .mul = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .slash => .{ .div = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .percent => .{ .mod = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .double_star => .{ .pow = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .double_equal => .{ .eq = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .bang_equal => .{ .ne = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .l_angle_bracket => .{ .lt = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .l_angle_bracket_equal => .{ .lte = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .r_angle_bracket => .{ .gt = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .r_angle_bracket_equal => .{ .gte = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .double_l_angle_bracket => .{ .bshl = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .double_r_angle_bracket => .{ .bshr = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .ampersand => .{ .band = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .pipe => .{ .bor = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .dot => .{ .prop_access = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .keyword_or => .{ .@"or" = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .keyword_and => .{ .@"and" = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         else => {
    //             unreachable;
    //         },
    //     },
    //     .start_token = start_token,
    //     .end_token = end_token,
    // });
}
pub fn parseGroup(self: *AstGen) AstGenError!Node.Index {
    const start_token = self.token_index;
    assert(self.tokenIs(.l_parenthesis));
    self.consumeToken();
    const expr = try self.parseExpression();
    if (expr != 0) {
        const index = try self.pushNode(.{
            .tag = .group,
            .data = .{ .group = expr },
            .start_token = start_token,
            .end_token = self.token_index,
        });
        if (self.tokenIs(.r_parenthesis)) {
            self.consumeToken();
        } else {
            try self.errors.addError(.{
                .tag = .expected_token,
                .start = self.token_index,
                .end = self.token_index,
                .payload = @intFromEnum(Token.Tag.r_parenthesis),
            });
        }
        return index;
    }
    try self.errors.addError(.{
        .tag = .expected_expression,
        .start = start_token + 1,
        .end = self.token_index,
    });
    if (self.tokenIs(.r_parenthesis)) {
        self.consumeToken();
    }

    return 0;
}
pub fn parseIfExpression(self: *AstGen) AstGenError!Node.Index {
    std.debug.print("[{d}] parseIfExpression: {s}\n", .{ self.token_index, @tagName(self.peekToken().?.tag) });
    const start_token = self.token_index;
    assert(self.tokenIs(.keyword_if));
    self.consumeToken();

    const condition: Node.Index = try self.parseExpression();
    if (condition == 0) {
        try self.errors.addError(.{
            .tag = .expected_expression,
            .start = self.token_index,
            .end = self.token_index,
        });
        return 0;
    }
    if (self.getNode(condition).tag != .group) {
        try self.errors.addError(.{
            .tag = .expression_not_parenthesized,
            .start = self.getNode(condition).start_token,
            .end = self.getNode(condition).end_token,
        });
    }
    const then_expr = try self.parseExpression();
    if (then_expr == 0) {
        try self.errors.addError(.{
            .tag = .expected_expression,
            .start = self.token_index,
            .end = self.token_index,
        });
        return 0;
    }
    const else_expr = blk: {
        if (self.tokenIs(.keyword_else)) {
            self.consumeToken();
            const expr = try self.parseExpression();
            if (expr == 0) {
                try self.errors.addError(.{
                    .tag = .expected_expression,
                    .start = self.token_index,
                    .end = self.token_index,
                });
            }
            break :blk expr;
        }
        break :blk 0;
    };
    return try self.pushNode(.{
        .tag = .if_expr,
        .data = .{ .if_expression = .{
            .condition = condition,
            .then_branch = then_expr,
            .else_branch = else_expr,
        } },
        .start_token = start_token,
        .end_token = self.token_index,
    });
}
pub fn parsePostfixUnary(self: *AstGen, lhs: Node.Index) AstGenError!Node.Index {
    const token = self.peekToken() orelse return lhs;
    self.consumeToken();
    return try self.pushNode(.{
        .tag = switch (token.tag) {
            .double_plus => .increment,
            .double_minus => .decrement,
            else => unreachable,
        },
        .data = .{ .unary_expression = lhs },
        .start_token = self.getNode(lhs).start_token,
        .end_token = self.token_index,
    });
}
pub fn parseFnDef(self: *AstGen) AstGenError!Node.Index {
    _ = self; // autofix
    @panic("unimplemented");
}
pub fn parseFnProto(self: *AstGen) AstGenError!Node.Index {
    _ = self; // autofix
    @panic("unimplemented");
}
pub fn parseFnParam(self: *AstGen) AstGenError!Node.Index {
    _ = self; // autofix
    @panic("unimplemented");
}
pub fn parseFnCall(self: *AstGen, callee: Node.Index) AstGenError!Node.Index {
    var args = self.node_lists.new(self.allocator);
    self.consumeToken();
    const index = try self.reserveNodeIndex();

    while (self.token_index < self.tokens.items.len) {
        const token = self.peekToken() orelse break;
        switch (token.tag) {
            .r_parenthesis => {
                self.consumeToken();
                break;
            },
            .comma => {
                self.consumeToken();
            },
            else => {
                const current_index = self.token_index;
                const expr = try self.parseExpression();
                assert(expr != 0);
                try args.append(expr);
                assert(self.token_index != current_index);
            },
        }
    }
    self.setNode(index, .{
        .tag = .fn_call,
        .data = .{ .fn_call = .{
            .callee = callee,
            .args_list = @intCast(try args.commit()),
        } },
        .start_token = index,
        .end_token = self.token_index,
    });

    return index;
}

test "parse" {
    const test_allocator = std.testing.allocator;
    // const source = "1 + 2 * 3";
    // const source = "a * (if (1) 2 else 3) + 4 * a.b()";
    const source = "3 + if (true == false or true) 2 + 2 else if (2) 3 + 3 else 4 + 4";
    var errors = try ErrorManager.init(test_allocator);
    defer errors.deinit();
    var ast = try parse(test_allocator, &errors, source);
    defer ast.deinit();

    try ast.format(std.io.getStdOut().writer().any(), 0, .{});
    // var ast = try Ast.parse(test_allocator, &errors, source);
    // defer ast.deinit();
    // std.debug.print("{any}\n", .{ast.nodes.items});
    // std.debug.print("\n\n", .{});
    // try ast.print(std.io.getStdOut().writer().any(), 0, .{});
    // std.debug.print("\n\n", .{});
}
