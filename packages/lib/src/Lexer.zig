const std = @import("std");
const Lexer = @This();
const patience = @import("patience_diff.zig");

pub const Token = struct {
    tag: Tag,
    start: usize,
    end: usize,
    pub const Index = u32;
    pub const Tag = enum {
        eof,
        identifier,
        number_literal,
        string_literal,
        char_literal,
        template_string_literal,
        comment,

        double_pipe,
        double_ampersand,

        equal,
        double_equal,

        bang_equal,
        r_angle_bracket_equal,
        l_angle_bracket_equal,
        double_r_angle_bracket,
        double_l_angle_bracket,
        r_angle_bracket,
        l_angle_bracket,
        l_parenthesis,
        r_parenthesis,
        l_brace,
        r_brace,
        l_bracket,
        r_bracket,
        comma,
        semicolon,
        colon,
        fat_arrow,

        newline,
        tab,

        double_plus,
        double_minus,
        double_star,
        slash_equal,
        star_equal,
        plus_equal,
        minus_equal,

        dot,
        double_dot,
        double_dot_equal,
        ellipsis,

        symbol,
        bang,
        plus,
        minus,
        star,
        slash,
        percent,
        ampersand,
        caret,
        pipe,
        backslash,
        tilde,

        // keywords
        keyword_if,
        keyword_else,
        keyword_for,
        keyword_while,
        keyword_do,
        keyword_inline,
        keyword_switch,
        keyword_case,
        keyword_default,
        keyword_break,
        keyword_continue,
        keyword_return,
        keyword_import,
        keyword_export,
        keyword_module,
        keyword_type,
        keyword_struct,
        keyword_enum,
        keyword_union,
        keyword_const,
        keyword_let,
        keyword_var,
        keyword_fn,
        keyword_function,
        keyword_func,
        keyword_gen,
        keyword_impl,
        keyword_trait,
        keyword_interface,
        keyword_extends,
        keyword_implements,
        // keyword_as,
        keyword_in,
        keyword_of,
        keyword_is,
        keyword_match,
        keyword_where,
        keyword_when,
        keyword_then,
        keyword_with,
        keyword_using,
        keyword_defer,
        keyword_async,
        keyword_await,
        keyword_yield,
        keyword_throw,
        keyword_try,
        keyword_catch,
        keyword_finally,
        keyword_or,
        keyword_and,
        keyword_not,
        keyword_orelse,
        keyword_class,
        keyword_true,
        keyword_false,
        keyword_null,
        keyword_undefined,
        keyword_void,
        keyword_bool,
        keyword_boolean,
        keyword_pub,
        keyword_extern,
        keyword_number,
        keyword_string,

        keyword_i8,
        keyword_i16,
        keyword_i32,
        keyword_i64,
        keyword_i128,
        keyword_i256,

        keyword_u8,
        keyword_u16,
        keyword_u32,
        keyword_u64,
        keyword_u128,
        keyword_u256,
        keyword_usize,
        keyword_f32,
        keyword_f64,
        pub fn isTypeIdentifierLike(self: Tag) bool {
            switch (self) {
                .identifier,

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
                => return true,
                else => return false,
            }
        }
        pub fn toInt(self: Tag) u32 {
            return @intFromEnum(self);
        }
    };
};

cursor: usize = 0,
source: []const u8,

hit_eof: bool = false,

pub fn init(source: []const u8) Lexer {
    return Lexer{
        .source = source,
    };
}

pub fn keywordOrIdentifier(self: *Lexer, start: usize, end: usize) Token {
    @setEvalBranchQuota(2000);
    const keyword_start = "keyword_";
    inline for (std.meta.fields(Token.Tag)) |field| {
        const keyword = comptime blk: {
            if (!std.mem.startsWith(u8, field.name, keyword_start)) continue;
            break :blk field.name[keyword_start.len..];
        };

        if (std.mem.eql(
            u8,
            keyword,
            self.source[start..end],
        )) {
            return .{
                .tag = @enumFromInt(field.value),
                .start = start,
                .end = end,
            };
        }
    }
    return Token{
        .tag = .identifier,
        .start = start,
        .end = end,
    };
}

pub fn next(self: *Lexer) ?Token {
    if (self.hit_eof) {
        return null;
    }
    const token = self.lex();
    self.hit_eof = token.tag == .eof;
    self.cursor = token.end;
    return token;
}

pub fn charAt(self: *Lexer, pos: usize) ?u8 {
    if (pos < self.source.len)
        return self.source[pos];
    return null;
}

pub fn startsWith(haystack: []const u8, needle: []const u8) bool {
    return std.mem.startsWith(u8, haystack, needle);
}

pub fn lex(self: *Lexer) Token {
    var cursor = self.cursor;
    const src = self.source;
    while (self.charAt(cursor)) |c| {
        switch (c) {
            '!' => {
                const start = cursor;
                if (startsWith(src[cursor..], "!=")) {
                    return .{
                        .tag = .bang_equal,
                        .start = start,
                        .end = start + 2,
                    };
                }
                return .{
                    .tag = .bang,

                    .start = start,
                    .end = start + 1,
                };
            },
            '\\' => {
                if (startsWith(src[cursor..], "\n")) {
                    return .{
                        .tag = .newline,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }

                if (startsWith(src[cursor..], "\t")) {
                    return .{
                        .tag = .tab,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }
                return .{
                    .tag = .backslash,
                    .start = cursor,
                    .end = cursor + 1,
                };
            },
            '*' => {
                if (startsWith(src[cursor..], "**")) {
                    return .{
                        .tag = .double_star,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }
                if (startsWith(src[cursor..], "*=")) {
                    return .{
                        .tag = .star_equal,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }
                return .{
                    .tag = .star,
                    .start = cursor,
                    .end = cursor + 1,
                };
            },
            '+' => {
                if (startsWith(src[cursor..], "+=")) {
                    return .{
                        .tag = .plus_equal,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }
                if (startsWith(src[cursor..], "++")) {
                    return .{
                        .tag = .double_plus,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }

                return .{ .tag = .plus, .start = cursor, .end = cursor + 1 };
            },
            '-', '0'...'9' => {
                if (std.ascii.isDigit(c) or (c == '-' and blk: {
                    if (self.charAt(cursor + 1)) |next_c| {
                        break :blk std.ascii.isDigit(next_c);
                    }
                    break :blk false;
                })) {
                    const start = cursor;
                    cursor += 1;

                    while (self.charAt(cursor)) |c_| {
                        if (std.ascii.isDigit(c_) or c_ == '_' or c_ == '.') {
                            cursor += 1;
                        } else {
                            break;
                        }
                    }

                    return .{
                        .tag = .number_literal,
                        .start = start,
                        .end = cursor,
                    };
                }
                if (startsWith(src[cursor..], "-=")) {
                    return .{
                        .tag = .minus_equal,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }
                return .{ .tag = .minus, .start = cursor, .end = cursor + 1 };
            },
            '=' => {
                if (startsWith(src[cursor..], "=>")) {
                    return .{
                        .tag = .fat_arrow,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }

                if (startsWith(src[cursor..], "==")) {
                    return .{
                        .tag = .double_equal,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }
                return .{ .tag = .equal, .start = cursor, .end = cursor + 1 };
            },
            '>' => {
                if (startsWith(src[cursor..], ">=")) {
                    return .{
                        .tag = .r_angle_bracket_equal,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }
                // if (startsWith(src[cursor..], ">>")) {
                //     return .{
                //         .tag = .double_r_angle_bracket,
                //         .start = cursor,
                //         .end = cursor + 2,
                //     };
                // }

                return .{ .tag = .r_angle_bracket, .start = cursor, .end = cursor + 1 };
            },
            '<' => {
                if (startsWith(src[cursor..], "<=")) {
                    return .{
                        .tag = .l_angle_bracket_equal,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }
                // if (startsWith(src[cursor..], "<<")) {
                //     return .{
                //         .tag = .double_l_angle_bracket,
                //         .start = cursor,
                //         .end = cursor + 2,
                //     };
                // }
                return .{ .tag = .l_angle_bracket, .start = cursor, .end = cursor + 1 };
            },
            '|' => {
                if (startsWith(src[cursor..], "||")) {
                    return .{
                        .tag = .double_pipe,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }
                return .{ .tag = .pipe, .start = cursor, .end = cursor + 1 };
            },

            '.' => {
                if (startsWith(src[cursor..], "...")) {
                    return .{
                        .tag = .ellipsis,
                        .start = cursor,
                        .end = cursor + 3,
                    };
                }
                if (startsWith(src[cursor..], "..")) {
                    return .{
                        .tag = .double_dot,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }

                if (startsWith(src[cursor..], "..=")) {
                    return .{
                        .tag = .double_dot_equal,
                        .start = cursor,
                        .end = cursor + 3,
                    };
                }
                return .{ .tag = .dot, .start = cursor, .end = cursor + 1 };
            },
            '&' => {
                if (startsWith(src[cursor..], "&&")) {
                    return .{
                        .tag = .double_ampersand,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }
                return .{ .tag = .ampersand, .start = cursor, .end = cursor + 1 };
            },
            '^' => return .{ .tag = .caret, .start = cursor, .end = cursor + 1 },
            '~' => return .{ .tag = .tilde, .start = cursor, .end = cursor + 1 },
            '%' => return .{ .tag = .percent, .start = cursor, .end = cursor + 1 },

            '/' => {
                if (startsWith(src[cursor..], "/=")) {
                    return .{
                        .tag = .slash_equal,
                        .start = cursor,
                        .end = cursor + 2,
                    };
                }
                if (startsWith(src[cursor..], "//")) {
                    const start = cursor;
                    cursor += 2;
                    while (self.charAt(cursor)) |c_| {
                        if (c_ == '\n') {
                            break;
                        }
                        cursor += 1;
                    }

                    return .{
                        .tag = .comment,
                        .start = start,
                        .end = cursor,
                    };
                }
                return .{ .tag = .slash, .start = cursor, .end = cursor + 1 };
            },

            inline '\'', '"', '`' => |c_pair| {
                const start = cursor;
                cursor += 1;
                while (self.charAt(cursor)) |c_| {
                    switch (c_) {
                        '\\' => {
                            cursor += 2;
                        },
                        c_pair => {
                            cursor += 1;
                            break;
                        },
                        else => {
                            cursor += 1;
                        },
                    }
                }
                const tag = switch (c_pair) {
                    '\'' => .char_literal,
                    '"' => .string_literal,
                    '`' => .template_string_literal,
                    else => unreachable,
                };
                return .{
                    .tag = tag,
                    .start = start,
                    .end = cursor,
                };
            },

            'a'...'z', 'A'...'Z', '$', '_' => {
                const start = cursor;
                cursor += 1;
                if (self.charAt(start) == '$') {
                    if (self.charAt(cursor)) |c_| {
                        if (c_ >= '0' and c_ <= '9' or c_ == '-' or c_ == '+' or c_ == '_') {
                            return self.keywordOrIdentifier(start, cursor);
                        }
                    }
                }
                while (self.charAt(cursor)) |c_| {
                    if (std.ascii.isAlphanumeric(c_) or c_ == '_' or c_ == '-' or c_ == '$') {
                        cursor += 1;
                    } else {
                        break;
                    }
                }

                return self.keywordOrIdentifier(start, cursor);
            },

            '[' => return .{ .tag = .l_bracket, .start = cursor, .end = cursor + 1 },
            ']' => return .{ .tag = .r_bracket, .start = cursor, .end = cursor + 1 },
            '(' => return .{ .tag = .l_parenthesis, .start = cursor, .end = cursor + 1 },
            ')' => return .{ .tag = .r_parenthesis, .start = cursor, .end = cursor + 1 },
            '{' => return .{ .tag = .l_brace, .start = cursor, .end = cursor + 1 },
            '}' => return .{ .tag = .r_brace, .start = cursor, .end = cursor + 1 },
            ',' => return .{ .tag = .comma, .start = cursor, .end = cursor + 1 },
            ';' => return .{ .tag = .semicolon, .start = cursor, .end = cursor + 1 },
            ':' => return .{ .tag = .colon, .start = cursor, .end = cursor + 1 },

            else => {
                cursor += 1;
            },
        }
    }
    return .{ .tag = .eof, .start = src.len, .end = src.len };
}

const testing_allocator = std.testing.allocator;

fn assertLex(str: []const u8, expected: []const Token) !void {
    var lexer = Lexer.init(str);
    for (expected) |expected_token| {
        const actual_token = lexer.next().?;
        try std.testing.expectEqual(expected_token, actual_token);
    }
}
fn assertLexParts(str: []const u8, expected: []const struct { []const u8, Token.Tag }) !void {
    var expected_str = std.ArrayList(u8).init(testing_allocator);
    var expected_writer = expected_str.writer();
    defer expected_str.deinit();
    for (expected) |expected_part| {
        const expected_s, const expected_tag = expected_part;
        if (expected_tag == .eof) {
            try expected_writer.print("[{s}]\t\n", .{@tagName(expected_tag)});
            continue;
        }
        try expected_writer.print("[{s}]:\t\"{s}\"\n", .{ @tagName(expected_tag), expected_s });
    }
    var actual_str = std.ArrayList(u8).init(testing_allocator);
    var actual_writer = actual_str.writer();
    defer actual_str.deinit();
    var lexer = Lexer.init(str);
    while (lexer.next()) |token| {
        if (token.tag == .eof) {
            try actual_writer.print("[{s}]\t\n", .{@tagName(token.tag)});
            continue;
        }
        try actual_writer.print("[{s}]:\t\"{s}\"\n", .{ @tagName(token.tag), lexer.source[token.start..token.end] });
    }
    if (std.mem.eql(u8, expected_str.items, actual_str.items)) return;
    var diff = try patience.diff(std.testing.allocator, expected_str.items, actual_str.items);
    defer diff.deinit();
    try diff.format(std.io.getStdErr().writer().any(), .{});
    return error.TestExpectedEqual;
}
test "Lexer" {
    try assertLexParts("add(1, sub($a))", &.{
        .{ "add", Token.Tag.identifier },
        .{ "(", Token.Tag.l_parenthesis },
        .{ "1", Token.Tag.number_literal },
        .{ ",", Token.Tag.comma },
        .{ "sub", Token.Tag.identifier },
        .{ "(", Token.Tag.l_parenthesis },
        .{ "$a", Token.Tag.identifier },
        .{ ")", Token.Tag.r_parenthesis },
        .{ ")", Token.Tag.r_parenthesis },
        .{ "", Token.Tag.eof },
    });

    try assertLexParts("add($1.4)", &.{
        .{ "add", Token.Tag.identifier },
        .{ "(", Token.Tag.l_parenthesis },
        .{ "$", Token.Tag.identifier },
        .{ "1.4", Token.Tag.number_literal },
        .{ ")", Token.Tag.r_parenthesis },
        .{ "", Token.Tag.eof },
    });

    try assertLexParts("math.add(1, 2)", &.{
        .{ "math", Token.Tag.identifier },
        .{ ".", Token.Tag.dot },
        .{ "add", Token.Tag.identifier },
        .{ "(", Token.Tag.l_parenthesis },
        .{ "1", Token.Tag.number_literal },
        .{ ",", Token.Tag.comma },
        .{ "2", Token.Tag.number_literal },
        .{ ")", Token.Tag.r_parenthesis },
        .{ "", Token.Tag.eof },
    });

    // binary operators

    try assertLexParts("1 + 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "+", Token.Tag.plus },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });
    try assertLexParts("1 - 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "-", Token.Tag.minus },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });
    try assertLexParts("1 * 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "*", Token.Tag.star },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });

    try assertLexParts("1 - 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "-", Token.Tag.minus },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });

    try assertLexParts("1 / 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "/", Token.Tag.slash },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });
    try assertLexParts("1 % 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "%", Token.Tag.percent },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });
    try assertLexParts("1 ** 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "**", Token.Tag.double_star },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });
    try assertLexParts("1 == 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "==", Token.Tag.double_equal },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });
    try assertLexParts("1 != 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "!=", Token.Tag.bang_equal },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });
    try assertLexParts("1 > 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ ">", Token.Tag.r_angle_bracket },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });
    try assertLexParts("1 < 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "<", Token.Tag.l_angle_bracket },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });
    try assertLexParts("1 >= 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ ">=", Token.Tag.r_angle_bracket_equal },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });
    try assertLexParts("1 <= 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "<=", Token.Tag.l_angle_bracket_equal },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });

    try assertLexParts("1 & 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "&", Token.Tag.ampersand },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });

    try assertLexParts("1 | 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "|", Token.Tag.pipe },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });

    try assertLexParts("1 ^ 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "^", Token.Tag.caret },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });

    // Unary operators
    try assertLexParts("~2", &.{
        .{ "~", Token.Tag.tilde },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });

    try assertLexParts("!2", &.{
        .{ "!", Token.Tag.bang },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });

    try assertLexParts("-a", &.{
        .{ "-", Token.Tag.minus },
        .{ "a", Token.Tag.identifier },
        .{ "", Token.Tag.eof },
    });
    try assertLexParts("+a", &.{
        .{ "+", Token.Tag.plus },
        .{ "a", Token.Tag.identifier },
        .{ "", Token.Tag.eof },
    });
    // shift operators
    try assertLexParts("1 << 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ "<<", Token.Tag.double_l_angle_bracket },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });
    try assertLexParts("1 >> 2", &.{
        .{ "1", Token.Tag.number_literal },
        .{ ">>", Token.Tag.double_r_angle_bracket },
        .{ "2", Token.Tag.number_literal },
        .{ "", Token.Tag.eof },
    });
    try assertLexParts("const c = 1;", &.{
        .{ "const", Token.Tag.keyword_const },
        .{ "c", Token.Tag.identifier },
        .{ "=", Token.Tag.equal },
        .{ "1", Token.Tag.number_literal },
        .{ ";", Token.Tag.semicolon },
        .{ "", Token.Tag.eof },
    });
}
