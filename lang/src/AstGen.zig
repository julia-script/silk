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
const Ast = @import("Ast.zig");
const Node = Ast.Node;
const Logger = @import("IndentedWriter.zig");
const log = Logger.new(.ast_gen);

const assert = std.debug.assert;
const AstGen = @This();

errors: *ErrorManager,
nodes: MultiArray(Node) = .{},
node_lists: PackedLists(Node.Index, 0) = .{},
tokens: Array(Token) = .{},
allocator: Allocator,
token_index: Token.Index = 0,
source: []const u8,
logger: Logger = Logger.init(std.io.getStdErr().writer().any()),

const AstGenError = error{
    OutOfMemory,
} || Logger.Error;

fn pushNode(self: *AstGen, node: Node) AstGenError!Node.Index {
    try self.logger.printLnIndented(comptime "[PUSH_NODE]: {s}", .{@tagName(node.tag)});
    const index = try self.nodes.addOne(self.allocator);
    self.nodes.set(index, node);
    return @intCast(index);
}
fn reserveNodeIndex(self: *AstGen) AstGenError!Node.Index {
    const index = try self.nodes.addOne(self.allocator);
    return @intCast(index);
}
fn setNode(self: *AstGen, index: Node.Index, node: Node) void {
    self.logger.printLnIndented(comptime "[SET_NODE]: {s}", .{@tagName(node.tag)}) catch {};
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

pub fn getNode(self: *AstGen, index: Node.Index) Node {
    return self.nodes.get(index);
}

pub fn debugToken(self: *AstGen, index: Token.Index) void {
    std.debug.print("{s} [{s}] \n", .{
        @tagName(self.tokens.items[index].tag),
        self.source[self.tokens.items[index].start..self.tokens.items[index].end],
    });
    // while (index < self.tokens.items.len) {
    //     std.debug.print("{s} ", .{@tagName(self.tokens.items[index].tag)});
    //     index += 1;
    // }
    // std.debug.print("\n", .{});
}
pub fn parseRoot(self: *AstGen) AstGenError!void {
    try self.loggerOpen("parseRoot");
    defer self.logger.close();
    const index = try self.reserveNodeIndex();
    var children = self.node_lists.new(self.allocator);
    while (self.token_index < self.tokens.items.len) {
        const index_before = self.token_index;
        const node = try self.parseExpression();
        // const node_tag = self.getNode(node).tag;
        // assert(
        //     node_tag == .@"export" or
        //         node_tag == .fn_decl or
        //         node_tag == .const_decl or
        //         node_tag == .var_decl,
        // );
        const index_after = self.token_index;
        assert(index_after > index_before);
        if (node == 0) break;
        try children.append(node);
    }
    const children_index = try children.commit();
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
    try self.loggerOpen("parseExpression");
    defer self.logger.close();
    const lhs = try self.parseUnary();
    if (lhs == 0) return 0;
    const expr = try self.parseBinaryRhs(0, lhs);

    if (self.tokenIs(.semicolon)) {
        self.consumeToken();
    }
    return expr;
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
                .tag = .ret_expression,
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
pub fn parseIdentifier(self: *AstGen) AstGenError!Node.Index {
    const token = self.peekToken() orelse return 0;
    assert(token.tag == .identifier);
    defer self.consumeToken();
    return try self.pushNode(.{
        .tag = .identifier,
        .data = .{ .literal = self.token_index },
        .start_token = self.token_index,
        .end_token = self.token_index,
    });
}
pub fn parsePrimary(self: *AstGen) AstGenError!Node.Index {
    try self.loggerOpen("parsePrimary");
    defer self.logger.close();
    if (self.peekToken()) |token| {
        switch (token.tag) {
            .eof => {
                self.consumeToken();
                return 0;
            },
            inline .string_literal,
            .number_literal,
            .keyword_true,
            .keyword_false,
            => |tag| {
                defer self.consumeToken();
                return try self.pushNode(.{
                    .tag = switch (tag) {
                        .string_literal => .string_literal,
                        .number_literal => .number_literal,
                        .keyword_false => .false_literal,
                        .keyword_true => .true_literal,
                        else => unreachable,
                    },
                    .data = .{ .literal = self.token_index },
                    .start_token = self.token_index,
                    .end_token = self.token_index,
                });
            },
            .identifier => return try self.parseIdentifier(),
            inline .keyword_number,
            .keyword_boolean,
            .keyword_string,
            .keyword_void,
            .keyword_i32,
            .keyword_i64,
            .keyword_f32,
            .keyword_f64,
            => |tag| {
                defer self.consumeToken();
                return try self.pushNode(.{
                    .tag = switch (tag) {
                        .keyword_number => .ty_number,
                        .keyword_boolean => .ty_boolean,
                        .keyword_string => .ty_string,
                        .keyword_void => .ty_void,
                        .keyword_i32 => .ty_i32,
                        .keyword_i64 => .ty_i64,
                        .keyword_f32 => .ty_f32,
                        .keyword_f64 => .ty_f64,
                        else => unreachable,
                    },
                    .data = .{ .literal = self.token_index },
                    .start_token = self.token_index,
                    .end_token = self.token_index,
                });
            },
            .keyword_return => {
                const start_token = self.token_index;
                self.consumeToken();
                const expr = try self.parseExpression();

                return try self.pushNode(.{
                    .tag = .ret_expression,
                    .data = .{ .ret_expression = expr },
                    .start_token = start_token,
                    .end_token = self.token_index,
                });
            },
            .l_brace => {
                return try self.parseBlock();
            },
            .keyword_fn => {
                return try self.parseFnDecl();
            },
            // => {
            //     return try self.parseDeclaration();
            // },
            inline .keyword_export,
            .keyword_const,
            .keyword_var,
            .keyword_extern,
            .keyword_pub,
            => |tag| {
                const start_token = self.token_index;
                self.consumeToken();
                const expr = try self.parseExpression();
                return try self.pushNode(.{
                    .tag = switch (tag) {
                        .keyword_export => .@"export",
                        .keyword_extern => .@"extern",
                        .keyword_pub => .@"pub",
                        .keyword_const => .const_decl,
                        .keyword_var => .var_decl,
                        else => unreachable,
                    },
                    .data = .{ .unary_expression = expr },
                    .start_token = start_token,
                    .end_token = self.token_index,
                });
            },
            .l_parenthesis => {
                return try self.parseGroup();
            },
            .keyword_if => {
                return try self.parseIfExpression();
            },
            .keyword_while => {
                return try self.parseWhileLoop();
            },
            .comment => {
                self.consumeToken();
                // TODO: add it to ast
                return try self.parsePrimary();
            },
            else => {
                self.logger.fail("unexpected {s}", .{@tagName(token.tag)});
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
        .equal => 0,
        .colon => 0,

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
            .colon => {
                self.consumeToken();
                const rhs = try self.parseTy();
                if (rhs == 0) return lhs;
                lhs = try self.makeBinaryExpression(token, lhs, rhs);
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

        lhs = try self.makeBinaryExpression(bin_op, lhs, rhs);
        if (lhs == 0) return lhs;
    }
    return lhs;
}
pub fn makeBinaryExpression(self: *AstGen, bin_op_token: Token, lhs: Node.Index, rhs: Node.Index) !Node.Index {
    try self.loggerOpen("makeBinaryExpression");
    defer self.logger.close();
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
            .equal => .assign,
            .colon => .ty_assign,
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
    try self.loggerOpen("parsePostfixUnary");
    defer self.logger.close();
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

pub fn parseFnDecl(self: *AstGen) AstGenError!Node.Index {
    try self.loggerOpen("parseFnDecl");
    defer self.logger.close();
    const start_token = self.token_index;
    if (!self.tokenIs(.keyword_fn)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.keyword_fn),
        });
        return 0;
    }
    const proto = try self.parseFnProto();
    if (proto == 0) {
        return 0;
    }

    const body = if (self.tokenIs(.l_brace))
        try self.parseBlock()
    else
        0;
    if (body == 0) {
        return proto;
    }
    return try self.pushNode(.{
        .tag = .fn_decl,
        .data = .{ .fn_decl = .{
            .proto = proto,
            .body = body,
        } },
        .start_token = start_token,
        .end_token = self.token_index,
    });
}
pub fn parseFnProto(self: *AstGen) AstGenError!Node.Index {
    assert(self.tokenIs(.keyword_fn));
    self.consumeToken();
    if (!self.tokenIs(.identifier)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.identifier),
        });
        return 0;
    }
    const start_token = self.token_index;
    const name = try self.parseIdentifier();
    assert(name != 0);

    if (!self.tokenIs(.l_parenthesis)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.l_parenthesis),
        });
        return name;
    } else {
        self.consumeToken();
    }
    var params = self.node_lists.new(self.allocator);
    while (true) {
        if (self.tokenIs(.r_parenthesis)) {
            break;
        }

        const param = try self.parseFnParam();
        if (param == 0) {
            break;
        }
        try params.append(param);
        if (self.tokenIs(.comma)) {
            self.consumeToken();
            continue;
        }

        break;
    }
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
    if (self.tokenIs(.colon)) {
        self.consumeToken();
    } else {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.colon),
        });
    }
    const ret_ty = try self.parseTy();
    return try self.pushNode(.{
        .tag = .fn_proto,
        .data = .{ .fn_proto = .{
            .name = name,
            .params_list = @intCast(try params.commit()),
            .ret_ty = ret_ty,
        } },
        .start_token = start_token,
        .end_token = self.token_index,
    });
}
pub fn parseFnParam(self: *AstGen) AstGenError!Node.Index {
    if (!self.tokenIs(.identifier)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.identifier),
        });
        return 0;
    }
    const index = try self.reserveNodeIndex();
    const start_token = self.token_index;
    const name = try self.parseIdentifier();
    var data: Node.Data.FnParam = .{
        .name = name,
        .ty = 0,
    };

    // if (!self.isToken())

    if (!self.tokenIs(.colon)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.colon),
        });
        self.setNode(index, .{
            .tag = .fn_param,
            .data = .{
                .fn_param = data,
            },
            .start_token = start_token,
            .end_token = self.token_index,
        });
        return index;
    }
    self.consumeToken();

    data.ty = try self.parseTy();
    self.setNode(index, .{
        .tag = .fn_param,
        .data = .{ .fn_param = data },
        .start_token = start_token,
        .end_token = self.token_index,
    });
    return index;
}
pub fn parseTy(self: *AstGen) AstGenError!Node.Index {
    try self.loggerOpen("parseTy");
    defer self.logger.close();
    return try self.parseTyInner(0);
}
pub fn parseTyInner(self: *AstGen, depth_arg: usize) AstGenError!Node.Index {
    try self.loggerOpen("parseTyInner");
    defer self.logger.close();
    var depth = depth_arg;
    const token = self.peekToken() orelse return 0;
    switch (token.tag) {
        .keyword_boolean,
        .keyword_number,
        .keyword_string,
        .keyword_void,
        .keyword_i32,
        .keyword_i64,
        .keyword_f32,
        .keyword_f64,
        => {
            return try self.parsePrimary();
        },

        .identifier => {
            const start_token = self.token_index;
            const name = try self.parsePrimary();

            if (name == 0) {
                return 0;
            }

            if (self.tokenIs(.l_angle_bracket)) {
                self.consumeToken();
                var args = self.node_lists.new(self.allocator);
                while (true) {
                    const ty = try self.parseTyInner(depth + 1);

                    if (ty == 0) break;
                    try args.append(ty);
                    if (self.tokenIs(.comma)) {
                        self.consumeToken();
                        continue;
                    }
                    break;
                }

                if (self.tokenIs(.r_angle_bracket)) {
                    self.consumeToken();
                } else if (self.tokenIs(.double_r_angle_bracket)) {
                    if (depth % 2 == 0) self.consumeToken();
                    depth += 2;
                } else {
                    try self.errors.addError(.{
                        .tag = .expected_token,
                        .start = start_token,
                        .end = self.token_index,
                        .payload = @intFromEnum(Token.Tag.r_angle_bracket),
                    });
                }
                return try self.pushNode(.{
                    .tag = .ty_generic,
                    .data = .{
                        .ty_generic = .{
                            .name = name,
                            .args_list = @intCast(try args.commit()),
                        },
                    },
                    .start_token = start_token,
                    .end_token = self.token_index,
                });
                // self.consumeToken();
                // return try self.pushNode(.{
                //     .tag = .generic_ty,
                //     .data = .{
                //         .generic_ty = .{
                //             .name = name,
                //             // .args_list = @intCast(try args.commit()),
                //         },
                //     },
                //     .start_token = start_token,
                //     .end_token = self.token_index,
                // });
            }
            return name;
        },
        else => {
            try self.errors.addError(.{
                .tag = .expected_token,
                .start = self.token_index,
                .end = self.token_index,
                .payload = @intFromEnum(Token.Tag.identifier),
            });

            return 0;
            // self.setNode(index, .{
            //     .tag = .fn_param,
            //     .data = data,
            //     .start_token = start_token,
            //     .end_token = self.token_index,
            // });
        },
    }

    @panic("unimplemented");
}
pub fn parseBlock(self: *AstGen) AstGenError!Node.Index {
    try self.loggerOpen("parseBlock");
    defer self.logger.close();
    const start_token = self.token_index;
    assert(self.tokenIs(.l_brace));
    self.consumeToken();
    var nodes = self.node_lists.new(self.allocator);
    while (!self.tokenIs(.r_brace)) {
        const index = self.token_index;
        const node = try self.parseExpression();
        assert(index != self.token_index);
        if (node == 0) {
            break;
        }
        try nodes.append(node);
        if (!self.tokenIs(.semicolon)) {
            try self.errors.addError(.{
                .tag = .expected_token,
                .start = self.token_index,
                .end = self.token_index,
                .payload = @intFromEnum(Token.Tag.semicolon),
            });
        } else {
            self.consumeToken();
        }
        if (!self.tokenIs(.r_brace)) {
            continue;
        }
        break;
    }
    if (!self.tokenIs(.r_brace)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.r_brace),
        });
    } else {
        self.consumeToken();
    }
    return try self.pushNode(.{
        .tag = .block,
        .data = .{ .children_list = @intCast(try nodes.commit()) },
        .start_token = start_token,
        .end_token = self.token_index,
    });
}
pub fn parseFnCall(self: *AstGen, callee: Node.Index) AstGenError!Node.Index {
    try self.loggerOpen("parseFnCall");
    defer self.logger.close();
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

pub fn parseWhileLoop(self: *AstGen) AstGenError!Node.Index {
    try self.loggerOpen("parseWhileLoop");
    defer self.logger.close();
    const start_token = self.token_index;
    assert(self.tokenIs(.keyword_while));
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

    const body = try self.parseExpression();
    if (body == 0) {
        try self.errors.addError(.{
            .tag = .expected_expression,
            .start = self.token_index,
            .end = self.token_index,
        });
        return 0;
    }

    return try self.pushNode(.{
        .tag = .while_loop,
        .data = .{ .while_loop = .{
            .condition = condition,
            .body = body,
        } },
        .start_token = start_token,
        .end_token = self.token_index,
    });
}

pub fn loggerOpen(self: *AstGen, label: []const u8) AstGenError!void {
    try self.logger.open("#token({d}:.{s}) {s}", .{
        self.token_index,
        if (self.peekToken()) |t| @tagName(t.tag) else "eof",
        label,
    });
}
