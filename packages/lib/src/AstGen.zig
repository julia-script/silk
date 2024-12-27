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
const Logger = @import("Logger.zig");
const builtin = @import("builtin");
const host = @import("host.zig");
const log = Logger.new(.ast_gen);

const compile_options = @import("options");

const assert = std.debug.assert;
const AstGen = @This();
const Tracer = @import("Tracer.zig");

ast: *Ast,
errors: *ErrorManager,
// nodes: MultiArray(Node) = .{},
// node_lists: PackedLists(Node.Index, 0) = .{},
// allocator: Allocator,
token_index: Token.Index = 0,
// source: []const u8,
logger: Logger = Logger.init(host.getStdErrWriter(), "AstGen"),
tracer: Tracer,

const AstGenError = error{
    OutOfMemory,
} || Logger.Error;

pub const Options = struct {
    trace_dir: ?[]const u8 = null,
    trace_name: ?[]const u8 = null,
    unique_trace_name: bool = true,
};
pub fn init(ast: *Ast, errors: *ErrorManager, options: Options) !AstGen {
    var tracer = try Tracer.init(
        options.trace_dir orelse "./.tmp/trace",
        options.trace_name orelse "ast-gen",
        options.unique_trace_name,
    );
    tracer.logEvent(
        "AstGen.init",
        .{},
    );
    return AstGen{
        .ast = ast,
        .errors = errors,
        .tracer = tracer,
    };
}
pub fn deinit(self: *AstGen) void {
    self.tracer.deinit();
}
fn pushNode(self: *AstGen, node: Node) AstGenError!Node.Index {
    const index = self.ast.nodes.items.len;
    self.logger.printLnIndented(comptime "[PUSH_NODE]: {s}", .{@tagName(node.data)}) catch {};

    for (self.ast.tokens.items[node.start_token..node.end_token]) |token| {
        self.logger.log(comptime " - tok .{s}", .{@tagName(token.tag)}, null);
    }

    try self.ast.nodes.append(self.ast.allocator, node);
    self.tracer.logEvent(
        "AstGen.pushNode",
        .{
            .snapshot = self.ast.nodes.items,
            .node_index = index,
            .node = node,
            // .node_tag = @tagName(node.data),
            .token_index = self.token_index,
            .tokens_in_node = self.ast.tokens.items[node.start_token..node.end_token],
        },
    );
    return @intCast(index);
}
pub fn reserveNodeIndex(self: *AstGen) AstGenError!Node.Index {
    self.tracer.logEvent(
        "AstGen.reserveNodeIndex",
        .{
            .index = self.ast.nodes.items.len,
        },
    );
    const index = self.ast.nodes.items.len;
    try self.ast.nodes.append(self.ast.allocator, .{
        .data = .{ .reserved = .{} },
        .start_token = 0,
        .end_token = 0,
    });
    return @intCast(index);
}
pub fn setNode(self: *AstGen, index: Node.Index, node: Node) void {
    self.tracer.logEvent(
        "AstGen.setNode",
        .{
            .index = index,
            .node = node,
        },
    );
    self.logger.printLnIndented(comptime "[SET_NODE]: {s}", .{@tagName(node.data)}) catch {};
    for (self.ast.tokens.items[node.start_token..node.end_token]) |token| {
        self.logger.log(comptime " - tok .{s}", .{@tagName(token.tag)}, null);
    }
    self.ast.nodes.items[index] = node;
}

/// Token helpers
pub fn nextToken(self: *AstGen) ?Token {
    const next_token = if (self.token_index < self.ast.tokens.items.len) self.ast.tokens.items[self.token_index] else null;
    self.tracer.logEvent(
        "AstGen.nextToken",
        .{
            .snapshot = self.ast.nodes.items,
            .token_index = self.token_index,
            .token = next_token,
        },
    );
    // if (self.token_index >= self.ast.tokens.items.len) return null;
    // const token = self.ast.tokens.items[self.token_index];
    if (next_token) |token| {
        self.consumeToken();
        return token;
    }
    return null;
}
pub fn accept(self: *AstGen, tag: Token.Tag) bool {
    const is_token = self.tokenIs(tag);
    self.tracer.printEvent(
        "AstGen.accept(%{d}.{s}) = %{}",
        .{
            self.token_index,
            @tagName(tag),
            is_token,
        },
        .{
            .token_index = self.token_index,
            .tag = tag,
            .accepted = is_token,
        },
    );
    if (is_token) {
        self.consumeToken();
        return true;
    }
    return false;
}

pub fn getToken(self: *AstGen) ?Token {
    if (self.token_index >= self.ast.tokens.items.len) return null;
    return self.ast.tokens.items[self.token_index];
}
pub fn consumeToken(self: *AstGen) void {
    const token = self.getToken();
    self.tracer.printEvent(
        "AstGen.consumeToken(%{d}.{s})",
        .{
            self.token_index,
            if (token) |tok| @tagName(tok.tag) else "NONE",
        },
        .{
            .token_index = self.token_index,
            .token = token,
        },
    );

    if (token) |tok| {
        const slice = self.ast.source[tok.start..tok.end];
        self.logger.log(comptime " consumed [{d}].{s} \"{s}\"", .{ self.token_index, @tagName(tok.tag), slice }, null);
        self.token_index += 1;
    }
}

pub fn peekToken(self: *AstGen) ?Token {
    if (self.token_index >= self.ast.tokens.items.len) return null;
    return self.ast.tokens.items[self.token_index];
}

pub fn tokenIs(self: *AstGen, tag: Token.Tag) bool {
    if (self.token_index >= self.ast.tokens.items.len) return false;
    return self.ast.tokens.items[self.token_index].tag == tag;
}
pub fn nextTokensAre(self: *AstGen, a: Token.Tag, b: Token.Tag) bool {
    if (self.tokenIs(a)) {
        const b_index = self.token_index + 1;
        if (b_index >= self.ast.tokens.items.len) return false;
        return self.ast.tokens.items[b_index].tag == b;
    }
    return false;
}

pub fn tokenOffsetIs(self: *AstGen, offset: usize, tag: Token.Tag) bool {
    if (self.token_index + offset >= self.ast.tokens.items.len) return false;
    return self.ast.tokens.items[self.token_index + offset].tag == tag;
}
pub fn getNode(self: *AstGen, index: Node.Index) Node {
    return self.ast.nodes.items[index];
}

pub fn nodeIs(self: *AstGen, index: Node.Index, tag: Node.Tag) bool {
    return std.meta.activeTag(self.getNode(index).data) == tag;
}
// pub const ParseMode = enum {
//     module,
//     expression,
// };
pub fn parse(self: *AstGen) AstGenError!void {
    var lexer = Lexer.init(self.ast.source);

    while (lexer.next()) |token| {
        try self.ast.tokens.append(self.ast.allocator, token);
    }
    const parse_id = self.tracer.beginEvent("AstGen.parse", .{
        .snapshot = self.ast.nodes.items,
        .tokens = self.ast.tokens.items,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parse", .{});
    self.loggerOpen("parse");
    defer self.logger.close();

    // if (mode == .expression) {
    //     const root = try self.reserveNodeIndex();
    //     var first_expr = try self.parseExpression();
    //     // skip comment lines
    //     while (self.nodeIs(first_expr, .comment_line)) {
    //         first_expr = try self.parseExpression();
    //     }
    //     self.setNode(root, .{
    //         .data = .{ .group = .{ .node = first_expr } },
    //         .start_token = 0,
    //         .end_token = @intCast(self.ast.tokens.items.len),
    //     });
    // } else {
    const root = try self.parseStructDecl();
    _ = root; // autofix
    // }
    // const index = try self.reserveNodeIndex();
    // var children = self.node_lists.new(self.allocator);
    // while (self.token_index < self.tokens.items.len) {
    //     const index_before = self.token_index;
    //     const node = try self.parseExpression();

    //     const index_after = self.token_index;
    //     // assert(index_after > index_before);
    //     if (index_after == index_before) self.consumeToken();
    //     if (node == 0) break;
    //     try children.append(node);
    // }
    // const children_index = try children.commit();
    // self.setNode(index, .{
    //     .start_token = 0,
    //     .end_token = @intCast(self.tokens.items.len - 1),
    //     .data = .{
    //         .root = .{
    //             .list = @intCast(children_index),
    //         },
    //     },
    // });
}

pub fn parseExpression(self: *AstGen) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseExpression", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseExpression", .{
        .current_token = self.token_index,
    });
    self.loggerOpen("parseExpression");
    defer self.logger.close();
    self.logger.log("parse LHS", .{}, null);
    const lhs = try self.parseUnary();
    const lhs_data = self.getNode(lhs).data;

    switch (lhs_data) {
        .comment_line => {
            return lhs;
        },
        else => {},
    }

    if (lhs == 0) return 0;

    self.logger.log("parse RHS", .{}, null);
    const expr = try self.parseBinaryRhs(0, lhs);
    return expr;
}

fn parsePropertyAccess(self: *AstGen, lhs: Node.Index) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parsePropertyAccess", .{
        .snapshot = self.ast.nodes.items,
        .lhs = lhs,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parsePropertyAccess", .{
        .lhs = lhs,
    });
    const is_builtin = self.tokenIs(.colon);
    self.consumeToken();
    const start_token = self.ast.nodes.items[lhs].start_token;
    const rhs = try self.parsePrimary();
    return try self.pushNode(.{
        .data = if (is_builtin) .{ .builtin_prop_access = .{
            .lhs = lhs,
            .rhs = rhs,
        } } else .{ .prop_access = .{
            .lhs = lhs,
            .rhs = rhs,
        } },
        .start_token = start_token,
        .end_token = self.token_index - 1,
    });
}

fn parseArrayPropertyAccess(self: *AstGen, lhs: Node.Index) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseArrayPropertyAccess", .{
        .snapshot = self.ast.nodes.items,
        .lhs = lhs,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseArrayPropertyAccess", .{
        .lhs = lhs,
    });
    self.logger.log("parse array like property access", .{}, null);
    self.consumeToken();
    const index = try self.parseExpression();
    if (index == 0) {
        try self.errors.addError(.{
            .tag = .expected_expression,
            .start = self.token_index,
            .end = self.token_index,
        });
        return lhs;
    }
    if (!self.accept(.r_bracket)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = Token.Tag.r_bracket.toInt(),
        });
    }
    const start_token = self.ast.nodes.items[lhs].start_token;
    return try self.pushNode(.{
        .data = .{
            .array_prop_access = .{
                .lhs = lhs,
                .rhs = index,
            },
        },
        .start_token = start_token,
        .end_token = self.token_index - 1,
    });
}

fn parseFunctionCall(self: *AstGen, callee: Node.Index) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseFunctionCall", .{
        .snapshot = self.ast.nodes.items,
        .callee = callee,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseFunctionCall", .{
        .callee = callee,
    });
    self.logger.log("parse fn call", .{}, null);
    self.consumeToken();
    var args_list = self.ast.interned_lists.new();
    const first_expr = try self.parseExpression();
    if (first_expr != 0) {
        try args_list.append(first_expr);
    }
    while (self.tokenIs(.comma)) {
        self.consumeToken();
        const expr = try self.parseExpression();
        if (expr == 0) break;
        try args_list.append(expr);
    }
    const args_list_index = try args_list.commit();
    if (!self.accept(.r_parenthesis)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = Token.Tag.r_parenthesis.toInt(),
        });
    }
    const lhs_start = self.ast.nodes.items[callee].start_token;
    return try self.pushNode(.{
        .data = .{
            .fn_call = .{
                .callee = callee,
                .args_list = args_list_index,
            },
        },
        .start_token = lhs_start,
        .end_token = self.token_index - 1,
    });
}

fn parseUnary(self: *AstGen) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseUnary", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseUnary", .{
        .current_token = self.token_index,
    });
    self.loggerOpen("parseUnary");
    defer self.logger.close();
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
        // .keyword_return,
        => {},
        else => {
            return try self.parsePrimary();
        },
    }

    const op_token = self.peekToken() orelse return 0;
    self.consumeToken();

    const start_token = self.token_index;
    switch (op_token.tag) {
        // .keyword_return => {
        //     return try self.pushNode(.{
        //         .data = .{
        //             .ret_expression = .{
        //                 .node = try self.parseExpression(),
        //             },
        //         },
        //         .start_token = start_token,
        //         .end_token = self.token_index,
        //     });
        // },
        inline .minus, .bang, .tilde => |token_tag| {
            const tag = comptime switch (token_tag) {
                .minus => .neg,
                .bang => .not,
                .tilde => .bnot,
                else => unreachable,
            };
            const node_index = try self.parseUnary();
            const data: Node.Data = @unionInit(Node.Data, @tagName(tag), .{ .node = node_index });
            return try self.pushNode(.{
                // .tag = switch (op_token.tag) {
                //     .minus => .neg,
                //     .bang => .not,
                //     .tilde => .bnot,
                //     else => return 0,
                // },
                .data = data,
                .start_token = start_token,
                .end_token = self.token_index,
            });
        },
        else => unreachable,
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
    const parse_id = self.tracer.beginEvent("AstGen.parseIdentifier", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseIdentifier", .{
        .current_token = self.token_index,
    });
    const token = self.peekToken() orelse return 0;
    if (token.tag != .identifier) return 0;
    // assert(token.tag == .identifier);
    defer self.consumeToken();
    return try self.pushNode(.{
        .data = .{ .identifier = .{ .token = self.token_index } },
        .start_token = self.token_index,
        .end_token = self.token_index,
    });
}

pub fn parsePrimary(self: *AstGen) AstGenError!Node.Index {
    const parse_id = self.tracer.begin("AstGen.parsePrimary", .{}, .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.end(parse_id);
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
            .keyword_number,
            .keyword_boolean,
            .keyword_string,
            .keyword_void,

            .keyword_i8,
            .keyword_i16,
            .keyword_i32,
            .keyword_i64,
            .keyword_i128,
            .keyword_i256,

            .keyword_u8,
            .keyword_u16,
            .keyword_u32,
            .keyword_u64,
            .keyword_u128,
            .keyword_u256,

            .keyword_f32,
            .keyword_f64,
            .keyword_usize,
            => |token_tag| {
                const tag: Node.Tag = comptime switch (token_tag) {
                    .string_literal => .string_literal,
                    .number_literal => .number_literal,
                    .keyword_false => .false_literal,
                    .keyword_true => .true_literal,
                    .keyword_number => .ty_number,
                    .keyword_boolean => .ty_boolean,
                    .keyword_string => .ty_string,
                    .keyword_void => .ty_void,

                    .keyword_i8 => .ty_i8,
                    .keyword_i16 => .ty_i16,
                    .keyword_i32 => .ty_i32,
                    .keyword_i64 => .ty_i64,
                    .keyword_i128 => .ty_i128,
                    .keyword_i256 => .ty_i256,

                    .keyword_u8 => .ty_u8,
                    .keyword_u16 => .ty_u16,
                    .keyword_u32 => .ty_u32,
                    .keyword_u64 => .ty_u64,
                    .keyword_u128 => .ty_u128,
                    .keyword_u256 => .ty_u256,
                    .keyword_usize => .ty_usize,

                    .keyword_f32 => .ty_f32,
                    .keyword_f64 => .ty_f64,

                    else => unreachable,
                };
                defer self.consumeToken();
                return try self.pushNode(.{
                    .data = @unionInit(Node.Data, @tagName(tag), .{ .token = self.token_index }),
                    .start_token = self.token_index,
                    .end_token = self.token_index,
                });
            },

            .identifier => return try self.parseIdentifier(),
            .l_bracket => {
                const start_token = self.token_index;
                self.consumeToken();
                const size_expr = try self.parseExpression();
                if (!self.accept(.r_bracket)) {
                    try self.errors.addError(.{
                        .tag = .expected_token,
                        .start = self.token_index,
                        .end = self.token_index,
                        .payload = Token.Tag.r_bracket.toInt(),
                    });
                }
                const ty = try self.parsePrimary();
                if (ty == 0) return 0;
                return try self.pushNode(.{
                    .data = .{
                        .ty_array = .{
                            .size_expr = size_expr,
                            .type = ty,
                        },
                    },
                    .start_token = start_token,
                    .end_token = self.token_index,
                });
            },
            .keyword_return => {
                const start_token = self.token_index;
                var end_token = self.token_index;
                self.consumeToken();

                const expr = try self.parseExpression();
                // std.debug.panic("expr: {d}", .{expr});
                if (expr != 0) end_token = self.token_index - 1;
                return try self.pushNode(.{
                    .data = .{
                        .ret_expression = .{
                            .node = expr,
                        },
                    },
                    .start_token = start_token,
                    .end_token = end_token,
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
            .keyword_extern,
            .keyword_pub,
            .keyword_comp,
            => |token_tag| {
                const start_token = self.token_index;
                const tag = comptime switch (token_tag) {
                    .keyword_export => .@"export",
                    .keyword_extern => .@"extern",
                    .keyword_pub => .@"pub",
                    .keyword_comp => .comp,
                    else => unreachable,
                };

                self.consumeToken();
                const expr = try self.parseExpression();
                return try self.pushNode(.{
                    .data = @unionInit(Node.Data, @tagName(tag), .{ .node = expr }),
                    .start_token = start_token,
                    .end_token = self.token_index,
                });
            },
            inline .keyword_const,
            .keyword_var,
            .keyword_type,
            => |token_tag| {
                const start_token = self.token_index;
                self.consumeToken();
                const name = try self.parseIdentifier();
                const ty: Node.Index = blk: {
                    if (self.accept(.colon)) {
                        break :blk try self.parsePrimary();
                    }
                    break :blk 0;
                };
                const value = blk: {
                    if (self.tokenIs(.equal)) {
                        self.consumeToken();
                        break :blk try self.parseExpression();
                    }
                    break :blk 0;
                };
                const tag = comptime switch (token_tag) {
                    .keyword_const => .const_decl,
                    .keyword_var => .var_decl,
                    .keyword_type => .type_decl,
                    else => unreachable,
                };
                return try self.pushNode(.{
                    .data = @unionInit(
                        Node.Data,
                        @tagName(tag),
                        .{ .name = name, .type = ty, .value = value },
                    ),

                    .start_token = start_token,
                    .end_token = self.token_index - 1,
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
                const start_token = self.token_index;
                self.consumeToken();
                // TODO: add it to ast
                return try self.pushNode(.{
                    .data = .{ .comment_line = .{ .token = start_token } },
                    .start_token = start_token,
                    .end_token = self.token_index,
                });
            },
            .keyword_struct => {
                return try self.parseStructDecl();
            },
            else => {
                // self.logger.panic("unexpected {s}", .{@tagName(token.tag)});
            },
        }
    }

    try self.errors.addError(.{
        .tag = .unexpected_token,
        .start = self.token_index,
        .end = self.token_index,
    });
    // self.consumeToken();
    return 0;
}
// pub fn parseExpressionList(self: *AstGen) AstGenError!Node.Index {
//     const start_token = self.token_index;
//     const expr = try self.parseExpression();
//     return try self.pushNode(.{
//         .data = .{ .group = .{ .node = expr } },
//         .start_token = start_token,
//         .end_token = self.token_index,
//     });
// }

fn getTokenPrecedence(tag: Token.Tag) i32 {
    return switch (tag) {
        .equal => 0,
        // .colon => 0,

        // logical
        .keyword_or => 10,
        .keyword_and => 20,
        // bitwise
        .pipe => 30,
        .caret => 40,
        .ampersand => 50,
        // equality
        .double_equal => 60,
        .bang_equal => 60,
        // relational
        .l_angle_bracket => 70,
        .l_angle_bracket_equal => 70,
        .r_angle_bracket => 70,
        .r_angle_bracket_equal => 70,
        // bitwise shift
        .double_l_angle_bracket => 80,
        .double_r_angle_bracket => 80,
        // additive
        .plus => 90,
        .minus => 90,
        // multiplicative
        .star => 100,
        .slash => 100,
        .percent => 100,

        .double_star => 110,

        // unary
        .bang => 120,
        .tilde => 120,
        // .dot => 12,

        // .l_parenthesis => 13,

        .l_parenthesis,
        .l_brace,
        => 130,

        .l_bracket, .dot, .colon => 140,
        else => -1,
    };
}
fn parseBinaryRhs(self: *AstGen, expression_precedence: i32, lhs_: Node.Index) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseBinaryRhs", .{
        .snapshot = self.ast.nodes.items,
        .expression_precedence = expression_precedence,
        .lhs = lhs_,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseBinaryRhs", .{
        .expression_precedence = expression_precedence,
        .lhs = lhs_,
    });
    var lhs = lhs_;
    while (true) {
        const token = self.peekToken() orelse return lhs;

        var tag = token.tag;
        if (tag == .l_angle_bracket) {
            if (self.ast.tokens.items[self.token_index + 1].tag == .l_angle_bracket) {
                tag = .double_l_angle_bracket;
                self.consumeToken();
            }
        } else if (tag == .r_angle_bracket) {
            if (self.ast.tokens.items[self.token_index + 1].tag == .r_angle_bracket) {
                tag = .double_r_angle_bracket;
                self.consumeToken();
            }
        }

        const tok_prec = getTokenPrecedence(token.tag);

        if (tok_prec < expression_precedence) return lhs;
        self.logger.log("{s} tok_prec: {d} expression_precedence: {d}", .{
            @tagName(token.tag),
            tok_prec,
            expression_precedence,
        }, null);

        switch (token.tag) {
            .dot, .colon => {
                lhs = try self.parsePropertyAccess(lhs);
                continue;
            },
            .l_parenthesis => {
                lhs = try self.parseFunctionCall(lhs);
                continue;
            },
            .l_brace => {
                lhs = try self.parseTypeInit(lhs);
                continue;
            },
            .l_bracket => {
                lhs = try self.parseArrayPropertyAccess(lhs);
                continue;
            },
            else => {},
        }
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
    const make_id = self.tracer.beginEvent("AstGen.makeBinaryExpression", .{
        .snapshot = self.ast.nodes.items,
        .bin_op_token = bin_op_token,
        .lhs = lhs,
        .rhs = rhs,
    });
    defer self.tracer.endEvent(make_id, "AstGen.makeBinaryExpression", .{
        .bin_op_token = bin_op_token,
        .lhs = lhs,
        .rhs = rhs,
    });
    const start_token = self.ast.nodes.items[lhs].start_token;
    const end_token = self.ast.nodes.items[rhs].end_token;

    // const tag: Node.Tag = switch (bin_op_token.tag) {
    // inline .plus => .add,
    // inline .minus => .sub,
    // inline .star => .mul,
    // inline .slash => .div,
    // inline .percent => .mod,
    // inline .double_star => .pow,
    // inline .double_equal => .eq,
    // inline .bang_equal => .ne,
    // inline .l_angle_bracket => .lt,
    // inline .l_angle_bracket_equal => .le,
    // inline .r_angle_bracket => .gt,
    // inline .r_angle_bracket_equal => .ge,
    // inline .double_l_angle_bracket => .bshl,
    // inline .double_r_angle_bracket => .bshr,
    // inline .ampersand => .band,
    // inline .pipe => .bor,
    // inline .dot => .prop_access,
    // inline .keyword_or => .@"or",
    // inline .keyword_and => .@"and",
    // inline .equal => .assign,
    // inline .colon => .ty_assign,
    //     inline else => unreachable,
    // };
    return try self.pushNode(.{
        // .data = @unionInit(Node.Data, @tagName(tag), .{
        //     .lhs = lhs,
        //     .rhs = rhs,
        // }),
        .data = switch (bin_op_token.tag) {
            .plus => .{ .add = .{ .lhs = lhs, .rhs = rhs } },
            .minus => .{ .sub = .{ .lhs = lhs, .rhs = rhs } },
            .star => .{ .mul = .{ .lhs = lhs, .rhs = rhs } },
            .slash => .{ .div = .{ .lhs = lhs, .rhs = rhs } },
            .percent => .{ .mod = .{ .lhs = lhs, .rhs = rhs } },
            .double_star => .{ .pow = .{ .lhs = lhs, .rhs = rhs } },
            .double_equal => .{ .eq = .{ .lhs = lhs, .rhs = rhs } },
            .bang_equal => .{ .ne = .{ .lhs = lhs, .rhs = rhs } },
            .l_angle_bracket => .{ .lt = .{ .lhs = lhs, .rhs = rhs } },
            .l_angle_bracket_equal => .{ .le = .{ .lhs = lhs, .rhs = rhs } },
            .r_angle_bracket => .{ .gt = .{ .lhs = lhs, .rhs = rhs } },
            .r_angle_bracket_equal => .{ .ge = .{ .lhs = lhs, .rhs = rhs } },
            .double_l_angle_bracket => .{ .bshl = .{ .lhs = lhs, .rhs = rhs } },
            .double_r_angle_bracket => .{ .bshr = .{ .lhs = lhs, .rhs = rhs } },
            .ampersand => .{ .band = .{ .lhs = lhs, .rhs = rhs } },
            .pipe => .{ .bor = .{ .lhs = lhs, .rhs = rhs } },
            .dot => .{ .prop_access = .{ .lhs = lhs, .rhs = rhs } },
            .keyword_or => .{ .@"or" = .{ .lhs = lhs, .rhs = rhs } },
            .keyword_and => .{ .@"and" = .{ .lhs = lhs, .rhs = rhs } },
            .equal => .{ .assign = .{ .lhs = lhs, .rhs = rhs } },
            .colon => .{ .ty_assign = .{ .lhs = lhs, .rhs = rhs } },
            else => unreachable,
        },
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
    //         .l_angle_bracket_equal => .{ .le = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .r_angle_bracket => .{ .gt = .{
    //             .lhs = lhs,
    //             .rhs = rhs,
    //         } },
    //         .r_angle_bracket_equal => .{ .ge = .{
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
    const parse_id = self.tracer.beginEvent("AstGen.parseGroup", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseGroup", .{
        .current_token = self.token_index,
    });
    const start_token = self.token_index;
    assert(self.tokenIs(.l_parenthesis));
    self.consumeToken();
    const expr = try self.parseExpression();
    if (expr != 0) {
        const index = try self.pushNode(.{
            .data = .{ .group = .{ .node = expr } },
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
                .payload = Token.Tag.r_parenthesis.toInt(),
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
pub inline fn mapTag(tag: Token.Tag, comptime options: []const struct { Token.Tag, Node.Tag }) Node.Tag {
    inline for (options) |option| {
        if (option[0] == tag) return comptime option[1];
    }
    unreachable;
}
pub inline fn mapTagName(tag: Token.Tag, comptime options: []const struct { Token.Tag, Node.Tag }) []const u8 {
    inline for (options) |option| {
        if (option[0] == tag) return comptime @tagName(option[1]);
    }
    unreachable;
}
pub fn parsePostfixUnary(self: *AstGen, lhs: Node.Index) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parsePostfixUnary", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parsePostfixUnary", .{
        .current_token = self.token_index,
    });
    self.loggerOpen("parsePostfixUnary");
    defer self.logger.close();
    const token = self.peekToken() orelse return lhs;
    self.consumeToken();

    // const tag: Node.Tag = switch (token.tag) {
    //     inline .double_plus => .increment,
    //     inline .double_minus => .decrement,
    //     inline else => unreachable,
    // };
    // const tag = mapTagName(token.tag, &.{
    //     .{ .double_plus, .increment },
    //     .{ .double_minus, .decrement },
    // });
    return try self.pushNode(.{
        .data = switch (token.tag) {
            .double_plus => .{ .increment = .{ .node = lhs } },
            .double_minus => .{ .decrement = .{ .node = lhs } },
            else => unreachable,
        },
        .start_token = self.getNode(lhs).start_token,
        .end_token = self.token_index,
    });
}

pub fn parseFnDecl(self: *AstGen) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseFnDecl", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseFnDecl", .{
        .current_token = self.token_index,
    });
    self.loggerOpen("parseFnDecl");
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
        .data = .{ .fn_decl = .{
            .proto = proto,
            .body = body,
        } },
        .start_token = start_token,
        .end_token = self.token_index,
    });
}
pub fn parseFnProto(self: *AstGen) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseFnProto", .{
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseFnProto", .{
        .current_token = self.token_index,
    });
    self.loggerOpen("parseFnProto");
    defer self.logger.close();
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

    if (self.accept(.l_parenthesis)) {} else {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.l_parenthesis),
        });
        return name;
    }
    var params = self.ast.interned_lists.new();
    while (true) {
        if (self.tokenIs(.r_parenthesis)) {
            break;
        }

        const param = try self.parseFnParam();
        if (param == 0) {
            break;
        }
        try params.append(param);
        if (self.accept(.comma)) {
            continue;
        }
        break;
    }
    if (self.accept(.r_parenthesis)) {} else {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.r_parenthesis),
        });
    }
    if (self.accept(.colon)) {} else {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.colon),
        });
    }
    const ret_ty = try self.parsePrimary();

    return try self.pushNode(.{
        .data = .{ .fn_proto = .{
            .name = name,
            .params_list = try params.commit(),
            .ret_type = ret_ty,
        } },
        .start_token = start_token,
        .end_token = self.token_index - 1,
    });
}
pub fn parseFnParam(self: *AstGen) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseFnParam", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseFnParam", .{
        .current_token = self.token_index,
    });
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
        .type = 0,
    };

    // if (!self.isToken())

    if (!self.accept(.colon)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.colon),
        });
        self.setNode(index, .{
            .data = .{
                .fn_param = data,
            },
            .start_token = start_token,
            .end_token = self.token_index,
        });
        return index;
    }
    // self.consumeToken();

    data.type = try self.parseExpression();
    self.setNode(index, .{
        .data = .{ .fn_param = data },
        .start_token = start_token,
        .end_token = self.token_index - 1,
    });
    return index;
}
// pub fn parseTy(self: *AstGen) AstGenError!Node.Index {
//     self.loggerOpen("parseTy");
//     defer self.logger.close();
//     return try self.parseTyInner(0);
// }
pub fn parseTyInner(self: *AstGen, depth_arg: usize) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseTyInner", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseTyInner", .{
        .current_token = self.token_index,
    });
    self.loggerOpen("parseTyInner");
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
        .l_bracket => {
            const start_token = self.token_index;
            self.consumeToken();
            const expr = try self.parseExpression();

            if (!self.accept(.r_bracket)) {
                try self.errors.addError(.{
                    .tag = .expected_token,
                    .start = self.token_index,
                    .end = self.token_index,
                    .payload = @intFromEnum(Token.Tag.r_bracket),
                });
                return 0;
            }
            const ty = try self.parseTy();
            return try self.pushNode(.{
                .data = .{ .ty_list = .{ .size_expr = expr, .ty = ty } },
                .start_token = start_token,
                .end_token = self.token_index - 1,
            });
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
    const parse_id = self.tracer.beginEvent("AstGen.parseBlock", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseBlock", .{
        .current_token = self.token_index,
    });
    self.loggerOpen("parseBlock");
    defer self.logger.close();
    const start_token = self.token_index;
    assert(self.tokenIs(.l_brace));
    self.consumeToken();
    var nodes = self.ast.interned_lists.new();
    while (!self.tokenIs(.r_brace)) {
        const index = self.token_index;
        _ = index; // autofix
        const node = try self.parseExpression();
        // assert(index != self.token_index);
        if (node == 0) {
            break;
        }
        try nodes.append(node);
        self.logger.log("push statement {d}", .{node}, null);
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
    if (!self.accept(.r_brace)) {} else {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.r_brace),
        });
    }
    // self.consumeToken();
    return try self.pushNode(.{
        .data = .{ .block = .{ .list = try nodes.commit() } },
        .start_token = start_token,
        .end_token = self.token_index - 1,
    });
}
pub fn parseFnCall(self: *AstGen, callee: Node.Index) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseFnCall", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseFnCall", .{
        .current_token = self.token_index,
    });
    self.loggerOpen("parseFnCall");
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
        .data = .{ .fn_call = .{
            .callee = callee,
            .args_list = @intCast(try args.commit()),
        } },
        .start_token = index,
        .end_token = self.token_index,
    });

    return index;
}
pub fn parseTypeInit(self: *AstGen, ty: Node.Index) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseTypeInit", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseTypeInit", .{
        .current_token = self.token_index,
    });
    self.loggerOpen("parseTypeInit");
    defer self.logger.close();
    self.consumeToken();
    const start_token = self.ast.nodes.items[ty].start_token;
    if (!self.nextTokensAre(.identifier, .equal)) {
        // array init
        var items = self.ast.interned_lists.new();
        while (true) {
            const expr = try self.parseExpression();
            if (expr == 0) break;
            try items.append(expr);
            if (self.accept(.comma)) continue;
            break;
        }
        if (!self.accept(.r_brace)) {
            try self.errors.addError(.{
                .tag = .expected_token,
                .start = self.token_index,
                .end = self.token_index,
                .payload = Token.Tag.r_brace.toInt(),
            });
        }
        return try self.pushNode(.{
            .data = .{
                .array_init = .{
                    .type = ty,
                    .items_list = try items.commit(),
                },
            },
            .start_token = start_token,
            .end_token = self.token_index,
        });
    }
    var initializers = self.ast.interned_lists.new();
    while (!self.tokenIs(.r_brace)) {
        if (self.accept(.comment)) {
            continue;
        }
        const field_start_token = self.token_index;

        const name = try self.parseIdentifier();

        if (!self.accept(.equal)) {
            try self.errors.addError(.{
                .tag = .expected_token,
                .start = self.token_index,
                .end = self.token_index,
                .payload = @intFromEnum(Token.Tag.equal),
            });
        }
        const expr = try self.parseExpression();

        const field_init = try self.pushNode(.{
            .data = .{ .field_init = .{ .name = name, .value = expr } },
            .start_token = field_start_token,
            .end_token = self.token_index - 1,
        });

        try initializers.append(field_init);

        if (!self.accept(.comma) and !self.tokenIs(.r_brace)) {
            try self.errors.addError(.{
                .tag = .expected_token,
                .start = self.token_index,
                .end = self.token_index,
                .payload = @intFromEnum(Token.Tag.comma),
            });
        }
    }

    if (!self.accept(.r_brace)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.r_brace),
        });
    }
    return try self.pushNode(.{
        .data = .{ .type_init = .{
            .type = ty,
            .field_init_list = try initializers.commit(),
        } },
        .start_token = start_token,
        .end_token = self.token_index,
    });

    // return id;
}
// const a = [] i32;
pub fn parseArrayInit(self: *AstGen) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseArrayInit", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseArrayInit", .{
        .current_token = self.token_index,
    });
    const start_token = self.token_index;
    self.consumeToken();
    const first_expression = try self.parseExpression();

    array_init: {
        if (self.accept(.r_bracket)) {
            // It could be the end of an one item array initialization OR a fixed size type declaration
            const following_token = self.peekToken() orelse break :array_init;

            // if (following_token.tag == .l_bracket) {
            //     // Probably multidimensional array type
            // }
            const maybe_multidimension_array_type = following_token.tag == .l_bracket;
            const maybe_type_identifier = following_token.tag.isTypeIdentifierLike();

            if (!maybe_type_identifier and !maybe_multidimension_array_type) break :array_init;

            // Probably a type identifier
            const ty = try self.parsePrimary();

            const array_ty_init = try self.pushNode(.{
                .data = .{ .ty_array = .{
                    .size_expr = first_expression,
                    .ty = ty,
                } },
                .start_token = start_token,
                .end_token = self.token_index,
            });
            return array_ty_init;
        }
    }
    var args_list = self.node_lists.new(self.allocator);
    try args_list.append(first_expression);
    if (self.accept(.comma)) {
        while (true) {
            const expr = try self.parseExpression();
            if (expr == 0) break;
            try args_list.append(expr);
            if (self.accept(.comma)) continue;
            break;
        }
    }

    if (!self.accept(.r_bracket)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = @intFromEnum(Token.Tag.r_bracket),
        });
    }

    return try self.pushNode(.{
        .data = .{
            .array_init = .{ .list = @intCast(try args_list.commit()) },
        },
        .start_token = start_token,
        .end_token = self.token_index,
    });
}

pub fn parseIfExpression(self: *AstGen) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseIfExpression", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseIfExpression", .{
        .current_token = self.token_index,
    });
    self.loggerOpen("parseIfExpression");
    defer self.logger.close();
    const start_token = self.token_index;
    assert(self.tokenIs(.keyword_if));
    self.consumeToken();

    if (!self.accept(.l_parenthesis)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = Token.Tag.l_parenthesis.toInt(),
        });
        return 0;
    }
    const condition: Node.Index = try self.parseExpression();
    if (!self.accept(.r_parenthesis)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = Token.Tag.r_parenthesis.toInt(),
        });
        return 0;
    }
    if (condition == 0) {
        try self.errors.addError(.{
            .tag = .expected_expression,
            .start = self.token_index,
            .end = self.token_index,
        });
        return 0;
    }
    const then_expr = if (self.tokenIs(.l_brace))
        try self.parseBlock()
    else
        try self.parseExpression();

    if (then_expr == 0) {
        try self.errors.addError(.{
            .tag = .expected_expression,
            .start = self.token_index,
            .end = self.token_index,
        });
        return 0;
    }
    const else_expr = blk: {
        if (self.accept(.keyword_else)) {
            if (self.tokenIs(.l_brace)) {
                const expr = try self.parseBlock();
                if (expr == 0) {
                    try self.errors.addError(.{
                        .tag = .expected_expression,
                        .start = self.token_index,
                        .end = self.token_index,
                    });
                }
                break :blk expr;
            } else {
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
        }
        break :blk 0;
    };
    return try self.pushNode(.{
        .data = .{ .if_expr = .{
            .condition = condition,
            .then_branch = then_expr,
            .else_branch = else_expr,
        } },
        .start_token = start_token,
        .end_token = self.token_index - 1,
    });
}

pub fn parseWhileLoop(self: *AstGen) AstGenError!Node.Index {
    const parse_id = self.tracer.begin("AstGen.parseWhileLoop", .{}, .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.end(parse_id);
    self.loggerOpen("parseWhileLoop");
    defer self.logger.close();
    const start_token = self.token_index;
    assert(self.tokenIs(.keyword_while));
    self.consumeToken();

    if (!self.accept(.l_parenthesis)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = Token.Tag.l_parenthesis.toInt(),
        });
        return 0;
    }
    const condition: Node.Index = try self.parseExpression();
    if (!self.accept(.r_parenthesis)) {
        try self.errors.addError(.{
            .tag = .expected_token,
            .start = self.token_index,
            .end = self.token_index,
            .payload = Token.Tag.r_parenthesis.toInt(),
        });
        return 0;
    }
    if (condition == 0) {
        try self.errors.addError(.{
            .tag = .expected_expression,
            .start = self.token_index,
            .end = self.token_index,
        });
        return 0;
    }
    const then_expr = if (self.tokenIs(.l_brace))
        try self.parseBlock()
    else
        try self.parseExpression();

    if (then_expr == 0) {
        try self.errors.addError(.{
            .tag = .expected_expression,
            .start = self.token_index,
            .end = self.token_index,
        });
        return 0;
    }
    return try self.pushNode(.{
        .data = .{ .while_loop = .{
            .condition = condition,
            .body = then_expr,
        } },
        .start_token = start_token,
        .end_token = self.token_index - 1,
    });
}
pub fn parseStructDecl(self: *AstGen) AstGenError!Node.Index {
    const parse_id = self.tracer.beginEvent("AstGen.parseStructDecl", .{
        .snapshot = self.ast.nodes.items,
        .current_token = self.token_index,
    });
    defer self.tracer.endEvent(parse_id, "AstGen.parseStructDecl", .{
        .current_token = self.token_index,
    });
    self.loggerOpen("parseStructDecl");

    const index = try self.reserveNodeIndex();
    const is_root = index == 0;
    defer self.logger.close();
    const start_token = self.token_index;
    if (!is_root) {
        assert(self.tokenIs(.keyword_struct));
        self.consumeToken();
        if (!self.accept(.l_brace)) {
            try self.errors.addError(.{
                .tag = .expected_token,
                .start = self.token_index,
                .end = self.token_index,
                .payload = Token.Tag.l_brace.toInt(),
            });
            return 0;
        }
    }

    var members_list = self.ast.interned_lists.new();
    while (!self.tokenIs(.r_brace) and !self.tokenIs(.eof)) {
        const before_parsing_token_index = self.token_index;
        if (!self.nextTokensAre(.identifier, .colon) and !self.nextTokensAre(.identifier, .equal)) {
            const parse_event_id = self.tracer.begin("[PARSE_DECLARATION]", .{}, .{
                .current_token = self.token_index,
            });
            defer self.tracer.end(parse_event_id);
            // Probably a declaration
            const declaration = try self.parsePrimary();
            if (declaration != 0)
                try members_list.append(declaration);
        } else {
            const parse_event_id = self.tracer.begin("[PARSE_FIELD]", .{}, .{
                .current_token = self.token_index,
            });
            defer self.tracer.end(parse_event_id);
            const identifier = try self.parsePrimary();
            const field_start_token = self.token_index;
            if (!self.nodeIs(identifier, .identifier)) {
                try self.errors.addError(.{
                    .tag = .expected_node,
                    .start = self.token_index,
                    .end = self.token_index,
                    .payload = Node.Tag.identifier.toInt(),
                });
                return 0;
            }
            if (!self.accept(.colon)) {
                try self.errors.addError(.{
                    .tag = .expected_token,
                    .start = self.token_index,
                    .end = self.token_index,
                    .payload = Token.Tag.colon.toInt(),
                });
            }
            const ty = try self.parsePrimary();

            const default_value = blk: {
                if (self.accept(.equal)) {
                    break :blk try self.parseExpression();
                }
                break :blk 0;
            };
            try members_list.append(try self.pushNode(.{
                .data = .{ .struct_field = .{
                    .name = identifier,
                    .type = ty,
                    .default_value = default_value,
                } },
                .start_token = field_start_token,
                .end_token = self.token_index,
            }));
            if (!self.accept(.comma)) {
                try self.errors.addError(.{
                    .tag = .expected_token,
                    .start = self.token_index,
                    .end = self.token_index,
                    .payload = Token.Tag.comma.toInt(),
                });
            }
            // if (self.accept(.comma)) continue;
            if (self.accept(.semicolon)) {
                try self.errors.addError(.{
                    .tag = .expected_token,
                    .start = self.token_index,
                    .end = self.token_index,
                    .payload = Token.Tag.comma.toInt(),
                });
            }
            // break;
        }

        // to be sure we don't get stuck in infinite loops
        // assert(self.token_index > before_parsing_token_index);
        if (self.accept(.eof)) break;
        if (self.token_index == before_parsing_token_index) {
            self.consumeToken();
        }
    }

    self.setNode(index, .{
        .data = .{ .struct_decl = .{
            .members_list = try members_list.commit(),
        } },
        .start_token = start_token,
        .end_token = self.token_index,
    });
    if (!is_root) {
        if (!self.accept(.r_brace)) {
            try self.errors.addError(.{
                .tag = .expected_token,
                .start = self.token_index,
                .end = self.token_index,
                .payload = Token.Tag.r_brace.toInt(),
            });
        }
    }
    return index;
}

const fmt = @import("./format_utils.zig");
pub fn loggerOpen(self: *AstGen, label: []const u8) void {
    _ = label; // autofix
    self.logger.writeIndent() catch {};

    const token = self.ast.tokens.items[@min(self.token_index, self.ast.tokens.items.len - 1)];
    _ = token; // autofix

    // self.logger.print("#token({d}):", .{self.token_index}) catch {};
    // const color = fmt.pickColor(@intFromEnum(token.tag));
    // color.print(self.logger.writer, ".{s}", .{@tagName(token.tag)}, .{}) catch {};
    // self.logger.print(" {s} {{\n", .{label}) catch {};
    // self.logger.indent();
    // self.logger.print("\n", .{}, null) catch {};
    // self.logger.log("#token({d}:)", .{ self.token_index }, null);
    // self.logger.open("#token({d}:.{s}) {s}", .{
    //     self.token_index,
    //     if (self.peekToken()) |t| @tagName(t.tag) else "eof",
    //     label,
    // });
}
