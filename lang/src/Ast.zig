const std = @import("std");
const Array = std.ArrayListUnmanaged;
const Lexer = @import("Lexer.zig");
const Token = Lexer.Token;
const Ast = @This();
const assert = std.debug.assert;
const ErrorManager = @import("ErrorManager.zig");
const Color = @import("Color.zig");
const tw = Color.tw;
nodes: Array(Node),
allocator: std.mem.Allocator,

errors: *ErrorManager,
tokens: Array(Token),
token_index: Token.Index = 0,
source: []const u8,

node_lists: Array(Node.Index),
const NodeListsIndex = u32;
const Error = struct {};
pub const Node = struct {
    data: Data,

    start_token: Token.Index,
    end_token: Token.Index,

    // data: Data,
    pub const Index = u32;

    const BinaryExpression = struct {
        lhs: Index,
        rhs: Index,
    };
    const UnaryExpression = struct {
        operand: Index,
    };
    pub const Data = union(enum) {
        root: struct {
            children_list: NodeListsIndex,
        },

        // Binary operators
        // - Math
        add: BinaryExpression,
        sub: BinaryExpression,
        mul: BinaryExpression,
        div: BinaryExpression,
        mod: BinaryExpression,
        pow: BinaryExpression,

        // - Comparison
        eq: BinaryExpression,
        ne: BinaryExpression,
        lt: BinaryExpression,
        gt: BinaryExpression,
        lte: BinaryExpression,
        gte: BinaryExpression,

        // - Logical
        @"and": BinaryExpression,
        @"or": BinaryExpression,
        xor: BinaryExpression,
        // - Property access
        prop_access: BinaryExpression, // a.b

        // Bitwise operators
        band: BinaryExpression, // &
        bor: BinaryExpression, // |
        bxor: BinaryExpression, // ^
        bshl: BinaryExpression, // <<
        bshr: BinaryExpression, // >>

        // Unary operators
        bnot: UnaryExpression, // ~

        neg: UnaryExpression, // -
        not: UnaryExpression, // !
        @"pub": UnaryExpression, // pub
        @"extern": UnaryExpression, // extern

        // Literals
        number_literal: void,
        string_literal: void,
        char_literal: void,
        hex_literal: void,
        true_literal: void,
        false_literal: void,

        // Identifiers
        ident: void,

        // fn
        fn_def: struct {
            name: ?Node.Index,
            proto: ?Node.Index,
            body: ?Node.Index,
        },
        fn_proto: struct {
            params_list: NodeListsIndex,
            ret_type: ?Node.Index,
        },
        fn_param: struct {
            name: Node.Index,
            type: ?Node.Index,
        },
        fn_call: struct {
            fn_identifier: Node.Index,
            args_list: NodeListsIndex,
        },
        // Statements
        expr: UnaryExpression,
        group: struct {
            expr: Node.Index,
        },
        if_expr: struct {
            condition: Node.Index,
            then_branch: Node.Index,
            else_branch: ?Node.Index,
        },
        // @"for": struct {
        //   ident: Node.Index,
        //   expr: Node.Index,
        //   body: Node.Index,
        // },
        ret: struct {
            expr: ?Node.Index,
        },
        increment: UnaryExpression,
        decrement: UnaryExpression,

        // label,
    };
};

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
pub fn init(allocator: std.mem.Allocator, errors: *ErrorManager, source: []const u8) !Ast {
    return Ast{
        .nodes = try Array(Node).initCapacity(allocator, 1),
        .tokens = try Array(Token).initCapacity(allocator, 1),
        .node_lists = try Array(Node.Index).initCapacity(allocator, 1),
        .source = source,
        .errors = errors,
        .allocator = allocator,
    };
}

/// Token helpers
pub fn nextToken(self: *Ast) ?Token {
    if (self.token_index >= self.tokens.items.len) return null;
    const token = self.tokens.items[self.token_index];
    self.token_index += 1;
    return token;
}

pub fn consumeToken(self: *Ast) void {
    self.token_index += 1;
}

pub fn peekToken(self: *Ast) ?Token {
    if (self.token_index >= self.tokens.items.len) return null;
    return self.tokens.items[self.token_index];
}

pub fn tokenIs(self: *Ast, tag: Token.Tag) bool {
    if (self.token_index >= self.tokens.items.len) return false;
    return self.tokens.items[self.token_index].tag == tag;
}

pub fn getTokenSlice(self: *Ast, token_i: Token.Index) []const u8 {
    const token = self.tokens.items[token_i];
    return self.source[token.start..token.end];
}
/// Parsers
pub fn parse(allocator: std.mem.Allocator, errors: *ErrorManager, source: []const u8) !Ast {
    var ast = try Ast.init(allocator, errors, source);
    var lexer = Lexer.init(source);

    while (lexer.next()) |token| {
        try ast.tokens.append(ast.allocator, token);
    }
    try ast.parseRoot();
    return ast;
}
fn appendNodesList(self: *Ast, nodes: []const Node.Index) !NodeListsIndex {
    const index: NodeListsIndex = @intCast(self.node_lists.items.len);
    try self.node_lists.ensureUnusedCapacity(self.allocator, nodes.len + 1);
    std.debug.print("n {any}, {d}\n", .{ nodes, nodes.len + 1 });
    if (nodes.len > 0) {
        self.node_lists.appendSliceAssumeCapacity(
            nodes,
        );
        self.allocator.free(nodes);
    }
    self.node_lists.appendAssumeCapacity(0);
    return index;
}
// fn getNodeList(self: *Ast, index: NodeListsIndex) []const Node.Index {
//     return self.node_lists.items[index..];
// }
const NodeListIter = struct {
    slice: []const Node.Index,
    index: usize = 0,
    pub fn next(self: *NodeListIter) ?Node.Index {
        if (self.index >= self.slice.len) return null;
        const index = self.slice[self.index];
        if (index == 0) return null;
        self.index += 1;
        return index;
    }
};
fn iterNodeList(self: *Ast, index: NodeListsIndex) NodeListIter {
    return .{
        .slice = self.node_lists.items[index..],
    };
}
pub fn parseRoot(self: *Ast) !void {
    const index = try self.reserveNodeIndex();
    var children = try Array(Node.Index).initCapacity(self.allocator, 1);
    while (self.token_index < self.tokens.items.len) {
        const node = try self.parseExpression();
        if (node) |node_index| {
            try children.append(self.allocator, node_index);
        }
    }

    self.setNode(index, .{
        .data = .{ .root = .{
            .children_list = try self.appendNodesList(
                try children.toOwnedSlice(self.allocator),
            ),
        } },
        .start_token = 0,
        .end_token = @intCast(self.tokens.items.len),
    });
}
const AstError = error{
    OutOfMemory,
};
pub fn parseFnCall(self: *Ast, identifier: Node.Index) AstError!?Node.Index {
    std.debug.print("[{d}] parseFnCall: {s}\n", .{ self.token_index, @tagName(self.peekToken().?.tag) });
    // assert(self.tokenIs(.l_parenthesis));
    var args = try Array(Node.Index).initCapacity(self.allocator, 1);
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
                if (expr) |expr_index| {
                    try args.append(self.allocator, expr_index);
                }
                assert(self.token_index != current_index);
            },
        }
    }
    self.setNode(index, .{
        .data = .{ .fn_call = .{
            .fn_identifier = identifier,
            .args_list = try self.appendNodesList(
                try args.toOwnedSlice(self.allocator),
            ),
        } },
        .start_token = index,
        .end_token = self.token_index,
    });

    return index;
}
pub fn parseIdentifier(self: *Ast) !?Node.Index {
    std.debug.print("[{d}] parseIdentifier: {s}\n", .{ self.token_index, @tagName(self.peekToken().?.tag) });
    const start_token = self.token_index;
    self.consumeToken();

    const identifier = try self.appendNode(.{
        .data = .{ .ident = {} },
        .start_token = start_token,
        .end_token = start_token,
    });

    return identifier;
}
pub fn parsePrimary(self: *Ast) !?Node.Index {
    std.debug.print("[{d}] parsePrimary: {s}\n", .{ self.token_index, @tagName(self.peekToken().?.tag) });
    // handles fn decl, literals, identifiers, groups, if, loops...
    if (self.peekToken()) |token| {
        switch (token.tag) {
            .eof => {
                self.consumeToken();
                return null;
            },
            .string_literal => {
                defer self.consumeToken();
                return try self.appendNode(.{
                    .data = .{ .string_literal = {} },
                    .start_token = self.token_index,
                    .end_token = self.token_index,
                });
            },
            .number_literal => {
                defer self.consumeToken();
                return try self.appendNode(.{
                    .data = .{ .number_literal = {} },
                    .start_token = self.token_index,
                    .end_token = self.token_index,
                });
            },
            .identifier => {
                return try self.parseIdentifier();
            },
            .l_parenthesis => {
                return try self.parseGroup();
            },
            .keyword_true => {
                defer self.consumeToken();
                return try self.appendNode(.{
                    .data = .{ .true_literal = {} },
                    .start_token = self.token_index,
                    .end_token = self.token_index,
                });
            },
            .keyword_false => {
                defer self.consumeToken();
                return try self.appendNode(.{
                    .data = .{ .false_literal = {} },
                    .start_token = self.token_index,
                    .end_token = self.token_index,
                });
            },

            .keyword_if => {
                return try self.parseIfExpression();
            },
            // .ident => {},

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
    return null;
}
fn parseUnary(self: *Ast) !?Node.Index {
    std.debug.print("[{d}] parseUnary: {s}\n", .{ self.token_index, @tagName(self.peekToken().?.tag) });
    const token = self.peekToken() orelse return null;
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
        // .l_parenthesis,
        => {},
        else => {
            return try self.parsePrimary();
        },
    }
    if (token.tag == .l_parenthesis) {

        // return try self.parseFnCall();
    }
    for (self.nodes.items) |node| {
        std.debug.print("n {s}\n", .{@tagName(node.data)});
    }

    const op_token = self.peekToken() orelse return null;

    // if (op_token.tag == .l_parenthesis) {
    //     return try self.parseFnCall();
    // }
    self.consumeToken();

    const start_token = self.token_index;
    switch (op_token.tag) {
        // .l_parenthesis => {
        //     return try self.parseFnCall();
        // },
        .keyword_return => {
            return try self.appendNode(.{
                .data = .{ .ret = .{
                    .expr = try self.parseUnary(),
                } },
                .start_token = start_token,
                .end_token = self.token_index,
            });
        },
        .minus => {
            return try self.appendNode(.{
                .data = .{ .neg = .{
                    .operand = try self.parseUnary() orelse return null,
                } },
                .start_token = start_token,
                .end_token = self.token_index,
            });
        },
        .bang => {
            return try self.appendNode(.{
                .data = .{ .not = .{
                    .operand = try self.parseUnary() orelse return null,
                } },
                .start_token = start_token,
                .end_token = self.token_index,
            });
        },
        .tilde => {
            return try self.appendNode(.{
                .data = .{ .bnot = .{
                    .operand = try self.parseUnary() orelse return null,
                } },
                .start_token = start_token,
                .end_token = self.token_index,
            });
        },
        else => {},
    }
    return null;
}
fn parseExpression(self: *Ast) !?Node.Index {
    std.debug.print("[{d}] parseExpression: {s}\n", .{ self.token_index, @tagName(self.peekToken().?.tag) });
    const lhs = try self.parseUnary() orelse return null;
    // std.debug.print("peeks {any}\n", .{self.peekToken()});

    return self.parseBinaryRhs(0, lhs);
}
fn parsePostfixUnary(self: *Ast, lhs: Node.Index) !Node.Index {
    std.debug.print("[{d}] parsePostfixUnary: {s}\n", .{ self.token_index, @tagName(self.peekToken().?.tag) });
    const token = self.peekToken() orelse return lhs;

    switch (token.tag) {
        .double_plus => {
            self.consumeToken();
            return try self.appendNode(.{
                .data = .{ .increment = .{
                    .operand = lhs,
                } },
                .start_token = self.nodes.items[lhs].start_token,
                .end_token = self.token_index,
            });
        },
        .double_minus => {
            self.consumeToken();
            return try self.appendNode(.{
                .data = .{ .decrement = .{
                    .operand = lhs,
                } },
                .start_token = self.nodes.items[lhs].start_token,
                .end_token = self.token_index,
            });
        },
        else => {
            unreachable;
        },
    }
}
fn parseBinaryRhs(self: *Ast, expression_precedence: i8, lhs_: Node.Index) !?Node.Index {
    var lhs = lhs_;
    while (true) {
        const token = self.peekToken() orelse return lhs;

        switch (token.tag) {
            .double_plus, .double_minus => {
                lhs = try self.parsePostfixUnary(lhs);
                continue;
            },
            .l_parenthesis => {
                lhs = try self.parseFnCall(lhs) orelse return lhs;
                continue;
            },
            else => {},
        }

        const tok_prec = getTokenPrecedence(token.tag);

        if (tok_prec < expression_precedence) return lhs;

        const bin_op = self.nextToken() orelse return lhs;

        var rhs = try self.parseUnary() orelse return null;

        const next_tok = self.peekToken() orelse return null;

        const next_precedence = getTokenPrecedence(
            next_tok.tag,
        );
        if (tok_prec < next_precedence) {
            rhs = try self.parseBinaryRhs(tok_prec + 1, rhs) orelse return null;
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
    }
    return null;
}
pub fn parseGroup(self: *Ast) AstError!?Node.Index {
    std.debug.print("[{d}] parseGroup: {s}\n", .{ self.token_index, @tagName(self.peekToken().?.tag) });
    const start_token = self.token_index;
    assert(self.tokenIs(.l_parenthesis));
    self.consumeToken();
    const expr = try self.parseExpression();
    if (expr) |expr_index| {
        const index = try self.appendNode(.{
            .data = .{ .group = .{
                .expr = expr_index,
            } },
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

    // try    self.node
    //         .data = .{ .group = .{
    //             .expr = expr,
    //         } },
    //         .start_token = start_token,
    //         .end_token = self.token_index,
    //     });

    return null;
}
pub fn makeBinaryExpression(self: *Ast, bin_op_token: Token, lhs: Node.Index, rhs: Node.Index) !Node.Index {
    const start_token = self.nodes.items[lhs].start_token;
    const end_token = self.nodes.items[rhs].end_token;
    return try self.appendNode(.{
        .data = switch (bin_op_token.tag) {
            .plus => .{ .add = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .minus => .{ .sub = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .star => .{ .mul = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .slash => .{ .div = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .percent => .{ .mod = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .double_star => .{ .pow = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .double_equal => .{ .eq = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .bang_equal => .{ .ne = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .l_angle_bracket => .{ .lt = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .l_angle_bracket_equal => .{ .lte = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .r_angle_bracket => .{ .gt = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .r_angle_bracket_equal => .{ .gte = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .double_l_angle_bracket => .{ .bshl = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .double_r_angle_bracket => .{ .bshr = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .ampersand => .{ .band = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .pipe => .{ .bor = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .dot => .{ .prop_access = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .keyword_or => .{ .@"or" = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            .keyword_and => .{ .@"and" = .{
                .lhs = lhs,
                .rhs = rhs,
            } },
            else => {
                unreachable;
            },
        },
        .start_token = start_token,
        .end_token = end_token,
    });
}
pub fn getNode(self: *Ast, index: Node.Index) Node {
    return self.nodes.items[index];
}
pub fn parseIfExpression(self: *Ast) AstError!?Node.Index {
    std.debug.print("[{d}] parseIfExpression: {s}\n", .{ self.token_index, @tagName(self.peekToken().?.tag) });
    const start_token = self.token_index;
    assert(self.tokenIs(.keyword_if));
    self.consumeToken();
    // if (self.tokenIs(.l_parenthesis)) {
    //     self.consumeToken();
    // } else {
    //     try self.errors.addError(.{
    //         .tag = .expected_token,
    //         .start = self.token_index,
    //         .end = self.token_index,
    //         .payload = @intFromEnum(Token.Tag.l_parenthesis),
    //     });
    // }
    const condition: Node.Index = try self.parseExpression() orelse {
        try self.errors.addError(.{
            .tag = .expected_expression,
            .start = self.token_index,
            .end = self.token_index,
        });
        return null;
    };
    if (std.meta.activeTag(self.getNode(condition).data) != .group) {
        try self.errors.addError(.{
            .tag = .expression_not_parenthesized,
            .start = self.getNode(condition).start_token,
            .end = self.getNode(condition).end_token,
        });
    }
    const then_expr = try self.parseExpression() orelse {
        try self.errors.addError(.{
            .tag = .expected_expression,
            .start = self.token_index,
            .end = self.token_index,
        });
        return null;
    };
    const else_expr = blk: {
        if (self.tokenIs(.keyword_else)) {
            self.consumeToken();
            const expr = try self.parseExpression();
            if (expr == null) {
                try self.errors.addError(.{
                    .tag = .expected_expression,
                    .start = self.token_index,
                    .end = self.token_index,
                });
            }
            break :blk expr;
        }
        break :blk null;
    };
    // if (self.tokenIs(.keyword_else)) {
    //     self.consumeToken();
    //     const else_expr = try self.parseExpression() orelse {
    //         try self.errors.addError(.{
    //             .tag = .expected_expression,
    //             .start = self.token_index,
    //             .end = self.token_index,
    //         });
    //         // return try self.appendNode(.{
    //         //     .data = .{ .@"if" = .{
    //         //         .condition = condition,
    //         //         .then_branch = then_expr,
    //         //         .else_branch = null,
    //         //     } },
    //         //     .start_token = start_token,
    //         //     .end_token = self.token_index,
    //         // });
    //     };
    //     return try self.appendNode(.{
    //         .data = .{ .@"if" = .{
    //             .condition = condition,
    //             .then_branch = then_expr,
    //             .else_branch = else_expr,
    //         } },
    //         .start_token = start_token,
    //         .end_token = self.token_index,
    //     });
    // }
    return try self.appendNode(.{
        .data = .{ .if_expr = .{
            .condition = condition,
            .then_branch = then_expr,
            .else_branch = else_expr,
        } },
        .start_token = start_token,
        .end_token = self.token_index,
    });
    // const else_expr =
    // if (condition) 1 + 2 else 2 + 3
}
pub fn deinit(self: *Ast) void {
    self.nodes.deinit(self.allocator);
    self.tokens.deinit(self.allocator);
    self.node_lists.deinit(self.allocator);
}
pub fn reserveNodeIndex(self: *Ast) !Node.Index {
    const index: Node.Index = @intCast(self.nodes.items.len);
    try self.nodes.append(self.allocator, undefined);
    return index;
}
pub fn setNode(self: *Ast, index: Node.Index, node: Node) void {
    self.nodes.items[index] = node;
}
pub fn appendNode(self: *Ast, node: Node) !Node.Index {
    const index: Node.Index = @intCast(self.nodes.items.len);
    try self.nodes.append(self.allocator, node);
    return index;
}

//|`Block name`|(a(map (i in x) to { i + 1 }))

const RAINBOW = [7]Color{
    tw.red_200,
    tw.orange_200,
    tw.yellow_200,
    tw.green_200,
    tw.blue_200,
    tw.indigo_200,
    tw.violet_200,
};

pub fn writeIndent(writer: std.io.AnyWriter, indent: usize, options: struct {
    rainbow: bool = true,
    size: usize = 2,
}) !void {
    if (options.rainbow) {
        for (0..indent) |color| {
            try RAINBOW[color % RAINBOW.len].brighter(-0.5).write(writer, "|");
            try writer.writeByteNTimes(' ', options.size - 1);
        }
    } else {
        for (0..indent) |_| {
            _ = try writer.write("|");
            try writer.writeByteNTimes(' ', options.size - 1);
        }
    }
}
pub fn a(n: i32) i32 {
    if (n > 10) {
        return 1;
    }
    return 0;
}
const FormatOptions = struct {
    color: bool = true,
    indent_size: usize = 2,
    show_node_index: bool = false,
};
pub fn print(self: *Ast, writer: std.io.AnyWriter, node: Node.Index, options: FormatOptions) !void {
    try self.format(writer, node, 0, options);
}

pub fn format(self: *Ast, writer: std.io.AnyWriter, node: Node.Index, indent: usize, options: FormatOptions) !void {
    const n = self.nodes.items[node];
    // _ = try writer.writeBytesNTimes("| ", indent);
    try writeIndent(writer, indent, .{});
    if (options.show_node_index) try tw.blue_400.brighter(-0.3).print(writer, "[{d}]", .{node});
    try tw.blue_400.bold().print(writer, "{s}: ", .{@tagName(n.data)});
    // try writer.print("[{d}]{s}: ", .{ node, @tagName(n.data) });
    switch (n.data) {
        .add,
        .sub,
        .mul,
        .div,
        .mod,
        .pow,
        .eq,
        .ne,
        .lt,
        .gt,
        .lte,
        .gte,
        .band,
        .bor,
        .bxor,
        .bshl,
        .bshr,
        .prop_access,
        .@"or",
        .@"and",
        => |data| {
            _ = try writer.write("\n");
            try self.format(writer, data.lhs, indent + 1, options);
            try self.format(writer, data.rhs, indent + 1, options);
        },
        .root => |data| {
            _ = try writer.write("\n");
            var iter = self.iterNodeList(data.children_list);
            while (iter.next()) |child| {
                try self.format(writer, child, indent + 1, options);
            }
        },
        .ident => try tw.yellow_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(n.start_token)}),
        .true_literal, .false_literal => try tw.pink_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(n.start_token)}),
        .number_literal => try tw.green_400.bold().print(writer, "{s}\n", .{self.getTokenSlice(n.start_token)}),
        .fn_call => |data| {
            _ = try writer.write("\n");
            try writeIndent(writer, indent + 1, .{});
            try tw.blue_400.print(writer, "identifier:\n", .{});
            try self.format(writer, data.fn_identifier, indent + 2, options);
            // _ = try writer.write("\n");
            try writeIndent(writer, indent + 1, .{});
            try tw.blue_400.print(writer, "args:\n", .{});
            var iter = self.iterNodeList(data.args_list);
            while (iter.next()) |arg| {
                try self.format(writer, arg, indent + 2, options);
            }
            // try writer.print("{s}()\n", .{self.getTokenSlice(data.fn_identifier)});
            // try self.format(data.args_list, writer, indent + 1);
        },
        .group => |data| {
            _ = try writer.write("\n");
            try self.format(writer, data.expr, indent + 1, options);
        },
        .increment, .decrement => |data| {
            _ = try writer.write("\n");
            // try writer.print("{s}\n", .{self.getTokenSlice(n.start_token)});
            try self.format(writer, data.operand, indent + 1, options);
        },
        .if_expr => |data| {
            _ = try writer.write("\n");
            try writeIndent(writer, indent + 1, .{});
            try tw.blue_400.print(writer, "condition:\n", .{});
            try self.format(writer, data.condition, indent + 2, options);
            try writeIndent(writer, indent + 1, .{});
            try tw.blue_400.print(writer, "then:\n", .{});
            try self.format(writer, data.then_branch, indent + 2, options);
            if (data.else_branch) |else_branch| {
                try writeIndent(writer, indent + 1, .{});
                try tw.blue_400.print(writer, "else:\n", .{});
                try self.format(writer, else_branch, indent + 2, options);
            }
        },
        else => {
            _ = try writer.write("\n");
        },
    }
}
const test_allocator = std.testing.allocator;
test "parse" {
    // const source = "a.b(1)(2,3 + 2,4)";
    // const source = "1 * (2 + 3) + 4";
    const source = "3 + if (true == false or true) 2 + 2 else if (2) 3 + 3 else 4 + 4";
    var errors = try ErrorManager.init(test_allocator);
    defer errors.deinit();
    var ast = try Ast.parse(test_allocator, &errors, source);
    defer ast.deinit();
    std.debug.print("{any}\n", .{ast.nodes.items});
    std.debug.print("\n\n", .{});
    try ast.print(std.io.getStdOut().writer().any(), 0, .{});
    std.debug.print("\n\n", .{});
}
