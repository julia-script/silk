const Parser = struct {
    input: [16]i32,
    position: usize,
    current: i32,
    invalid: bool,
};

fn advance(parser: *Parser) void {
    parser.current = parser.input[parser.position];
    parser.position += 1;
    while (parser.current == 32 or parser.current == 9 or parser.current == 10 or parser.current == 13) {
        parser.current = parser.input[parser.position];
        parser.position += 1;
    }
}

fn factor(parser: *Parser) i32 {
    if (parser.current == 40) {
        advance(parser);
        const value = expression(parser);
        if (parser.current != 41) {
            parser.invalid = true;
            return 0;
        }
        advance(parser);
        return value;
    }
    if (parser.current < 48 or parser.current > 57) {
        parser.invalid = true;
        return 0;
    }
    var value: i32 = 0;
    while (parser.current >= 48 and parser.current <= 57) {
        value = value * 10 + parser.current - 48;
        advance(parser);
    }
    return value;
}

fn term(parser: *Parser) i32 {
    var value = factor(parser);
    while (parser.current == 42) {
        advance(parser);
        value *= factor(parser);
    }
    return value;
}

fn expression(parser: *Parser) i32 {
    var value = term(parser);
    while (parser.current == 43) {
        advance(parser);
        value += term(parser);
    }
    return value;
}

fn parse(input: [16]i32) i32 {
    var parser = Parser{ .input = input, .position = 0, .current = 0, .invalid = false };
    advance(&parser);
    const value = expression(&parser);
    return if (parser.invalid or parser.current != -1) 255 else @mod(value, 251);
}

pub fn main() void {
    // (12+3)*(4+5)
    if (parse([_]i32{ 40, 49, 50, 43, 51, 41, 42, 40, 52, 43, 53, 41, -1, -1, -1, -1 }) != 135) @trap();
    // 1+2*3
    if (parse([_]i32{ 49, 43, 50, 42, 51, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 }) != 7) @trap();
    //  (7+8)*2
    if (parse([_]i32{ 32, 40, 55, 43, 56, 41, 42, 50, 32, -1, -1, -1, -1, -1, -1, -1 }) != 30) @trap();
    // 1+
    if (parse([_]i32{ 49, 43, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 }) != 255) @trap();
    // (1+2
    if (parse([_]i32{ 40, 49, 43, 50, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 }) != 255) @trap();
    // 1x
    if (parse([_]i32{ 49, 120, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1 }) != 255) @trap();
}
