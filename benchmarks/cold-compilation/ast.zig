// Matched in-memory benchmark: see ast.silk, ast.rs, and README.md.
const std = @import("std");
const allocator = std.heap.page_allocator;
const Token = struct { kind: i32, value: i32, start: usize, end: usize };
const Diagnostic = struct { code: i32, start: usize, end: usize };
const Kind = union(enum) {
    number: i32,
    name: usize,
    negate: usize,
    binary: struct { operator: i32, left: usize, right: usize },
    assign: struct { index: usize, child: usize },
    invalid,
};
const Node = struct { kind: Kind, start: usize, end: usize };
const Parser = struct {
    tokens: std.ArrayList(Token) = .empty,
    nodes: std.ArrayList(Node) = .empty,
    roots: std.ArrayList(usize) = .empty,
    diagnostics: std.ArrayList(Diagnostic) = .empty,
    position: usize = 0,
};

// Token kinds: EOF=0, number=1, name=2, + =3, - =4, * =5,
// ( =6, ) =7, ; =8, = =9, invalid=10. Names are single ASCII letters.
fn digit(byte: u8) bool {
    return byte >= '0' and byte <= '9';
}
fn punctuation(byte: u8) i32 {
    return switch (byte) {
        '+' => 3,
        '-' => 4,
        '*' => 5,
        '(' => 6,
        ')' => 7,
        ';' => 8,
        '=' => 9,
        else => 10,
    };
}
fn report(parser: *Parser, code: i32, start: usize, end: usize) error{OutOfMemory}!void {
    try parser.diagnostics.append(allocator, .{ .code = code, .start = start, .end = end });
}
fn deinit(parser: *Parser) void {
    parser.tokens.deinit(allocator);
    parser.nodes.deinit(allocator);
    parser.roots.deinit(allocator);
    parser.diagnostics.deinit(allocator);
}
fn lex(source: []const u8) error{OutOfMemory}!Parser {
    var parser: Parser = .{};
    errdefer deinit(&parser);
    var position: usize = 0;
    while (position < source.len) {
        const byte = source[position];
        if (byte == ' ' or byte == '\t' or byte == '\n' or byte == '\r') {
            position += 1;
        } else if (byte == '#') {
            while (position < source.len and source[position] != '\n') {
                position += 1;
            }
        } else {
            const start = position;
            var kind = punctuation(byte);
            var value: i32 = 0;
            if (digit(byte)) {
                kind = 1;
                while (position < source.len and digit(source[position])) {
                    value = value * 10 + @as(i32, source[position]) - 48;
                    position += 1;
                }
            } else {
                position += 1;
                if (byte >= 'a' and byte <= 'z') {
                    kind = 2;
                    value = @as(i32, byte) - 97;
                }
                if (kind == 10) {
                    try report(&parser, 1, start, position);
                }
            }
            try parser.tokens.append(allocator, .{ .kind = kind, .value = value, .start = start, .end = position });
        }
    }
    try parser.tokens.append(allocator, .{ .kind = 0, .value = 0, .start = position, .end = position });
    return parser;
}
fn current(parser: *const Parser) Token {
    return parser.tokens.items[parser.position];
}
fn advance(parser: *Parser) Token {
    const token = current(parser);
    if (token.kind != 0) {
        parser.position += 1;
    }
    return token;
}
fn append(parser: *Parser, kind: Kind, start: usize, end: usize) error{OutOfMemory}!usize {
    const id = parser.nodes.items.len;
    try parser.nodes.append(allocator, .{ .kind = kind, .start = start, .end = end });
    return id;
}
fn primary(parser: *Parser) error{OutOfMemory}!usize {
    const token = current(parser);
    if (token.kind == 1) {
        _ = advance(parser);
        return append(parser, .{ .number = token.value }, token.start, token.end);
    }
    if (token.kind == 2) {
        _ = advance(parser);
        return append(parser, .{ .name = @intCast(token.value) }, token.start, token.end);
    }
    if (token.kind == 4) {
        _ = advance(parser);
        const child = try primary(parser);
        return append(parser, .{ .negate = child }, token.start, parser.nodes.items[child].end);
    }
    if (token.kind == 6) {
        _ = advance(parser);
        const child = try expression(parser);
        const closing = current(parser);
        if (closing.kind == 7) {
            _ = advance(parser);
        } else {
            try report(parser, 3, closing.start, closing.end);
        }
        return child;
    }
    try report(parser, 2, token.start, token.end);
    if (token.kind != 0 and token.kind != 8 and token.kind != 7) {
        _ = advance(parser);
    }
    return append(parser, .invalid, token.start, token.end);
}
fn binary(parser: *Parser, operator: i32, left: usize, right: usize) error{OutOfMemory}!usize {
    return append(parser, .{ .binary = .{ .operator = operator, .left = left, .right = right } }, parser.nodes.items[left].start, parser.nodes.items[right].end);
}
fn term(parser: *Parser) error{OutOfMemory}!usize {
    var left = try primary(parser);
    while (current(parser).kind == 5) {
        const operator = advance(parser).kind;
        const right = try primary(parser);
        left = try binary(parser, operator, left, right);
    }
    return left;
}
fn expression(parser: *Parser) error{OutOfMemory}!usize {
    var left = try term(parser);
    while (current(parser).kind == 3 or current(parser).kind == 4) {
        const operator = advance(parser).kind;
        const right = try term(parser);
        left = try binary(parser, operator, left, right);
    }
    return left;
}
fn statement(parser: *Parser) error{OutOfMemory}!usize {
    const token = current(parser);
    if (token.kind == 2 and parser.tokens.items[parser.position + 1].kind == 9) {
        _ = advance(parser);
        _ = advance(parser);
        const child = try expression(parser);
        return append(parser, .{ .assign = .{ .index = @intCast(token.value), .child = child } }, token.start, parser.nodes.items[child].end);
    }
    return expression(parser);
}
fn parse(source: []const u8) error{OutOfMemory}!Parser {
    var parser = try lex(source);
    errdefer deinit(&parser);
    while (current(&parser).kind != 0) {
        const root = try statement(&parser);
        try parser.roots.append(allocator, root);
        const token = current(&parser);
        if (token.kind != 8) {
            try report(&parser, 4, token.start, token.end);
            while (current(&parser).kind != 0 and current(&parser).kind != 8) {
                _ = advance(&parser);
            }
        }
        if (current(&parser).kind == 8) {
            _ = advance(&parser);
        }
    }
    return parser;
}
fn operation(operator: i32, left: i32, right: i32) i32 {
    if (operator == 3) return left + right;
    if (operator == 4) return left - right;
    return left * right;
}
// Postorder evaluation also checks that every child ID precedes its parent.
fn evaluate(parser: *const Parser) error{OutOfMemory}!i32 {
    var values: std.ArrayList(i32) = .empty;
    defer values.deinit(allocator);
    var bindings: std.ArrayList(i32) = .empty;
    defer bindings.deinit(allocator);
    for (0..26) |_| {
        try bindings.append(allocator, 0);
    }
    for (parser.nodes.items) |node| {
        const value = switch (node.kind) {
            .number => |value| value,
            .name => |index| bindings.items[index],
            .negate => |child| -values.items[child],
            .binary => |data| operation(data.operator, values.items[data.left], values.items[data.right]),
            .assign => |data| blk: {
                bindings.items[data.index] = values.items[data.child];
                break :blk values.items[data.child];
            },
            .invalid => 0,
        };
        try values.append(allocator, value);
    }
    var total: i32 = 0;
    for (parser.roots.items) |id| {
        total += values.items[id];
    }
    return total;
}
fn check(source: []const u8, total: i32, nodes: usize, roots: usize, diagnostics: usize, locations: usize) error{OutOfMemory}!bool {
    var parser = try parse(source);
    defer deinit(&parser);
    if (parser.nodes.items.len != nodes or parser.roots.items.len != roots or parser.diagnostics.items.len != diagnostics) return false;
    var positions: usize = 0;
    for (parser.diagnostics.items) |diagnostic| {
        positions += diagnostic.start + diagnostic.end + @as(usize, @intCast(diagnostic.code));
    }
    if (positions != locations) return false;
    for (parser.nodes.items) |node| {
        if (node.start > node.end or node.end > source.len) return false;
    }
    return try evaluate(&parser) == total;
}
fn suite() error{OutOfMemory}!bool {
    if (!try check("a = 12; b = (a + 3) * 9; b - a * 2; -b + 140;", 263, 17, 4, 0, 0)) return false;
    if (!try check("1 + ; 2 * 3; (4 + 5; 6;", 22, 10, 4, 2, 53)) return false;
    if (!try check("@; 7;", 7, 2, 2, 2, 5)) return false;
    if (!try check("1 2; 3;", 4, 2, 2, 1, 9)) return false;
    if (!try check("# comment\n", 0, 0, 0, 0, 0)) return false;
    var source: std.ArrayList(u8) = .empty;
    defer source.deinit(allocator);
    for (0..64) |_| {
        try source.appendSlice(allocator, "a = 12; b = (a + 3) * 9; b - a * 2; -b + 140; # loop\n");
    }
    return check(source.items, 16832, 1088, 256, 0, 0);
}
pub fn main() !void {
    if (!try suite()) @trap();
}
