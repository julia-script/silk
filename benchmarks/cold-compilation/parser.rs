struct Parser {
    input: [i32; 16],
    position: usize,
    current: i32,
    invalid: bool,
}

fn advance(parser: &mut Parser) {
    parser.current = parser.input[parser.position];
    parser.position += 1;
    while parser.current == 32
        || parser.current == 9
        || parser.current == 10
        || parser.current == 13
    {
        parser.current = parser.input[parser.position];
        parser.position += 1;
    }
}

fn factor(parser: &mut Parser) -> i32 {
    if parser.current == 40 {
        advance(parser);
        let value = expression(parser);
        if parser.current != 41 {
            parser.invalid = true;
            return 0;
        }
        advance(parser);
        return value;
    }
    if parser.current < 48 || parser.current > 57 {
        parser.invalid = true;
        return 0;
    }
    let mut value = 0;
    while parser.current >= 48 && parser.current <= 57 {
        value = value * 10 + parser.current - 48;
        advance(parser);
    }
    value
}

fn term(parser: &mut Parser) -> i32 {
    let mut value = factor(parser);
    while parser.current == 42 {
        advance(parser);
        value *= factor(parser);
    }
    value
}

fn expression(parser: &mut Parser) -> i32 {
    let mut value = term(parser);
    while parser.current == 43 {
        advance(parser);
        value += term(parser);
    }
    value
}

fn parse(input: [i32; 16]) -> i32 {
    let mut parser = Parser {
        input,
        position: 0,
        current: 0,
        invalid: false,
    };
    advance(&mut parser);
    let value = expression(&mut parser);
    if parser.invalid || parser.current != -1 {
        255
    } else {
        value % 251
    }
}

fn main() -> std::process::ExitCode {
    // (12+3)*(4+5)
    if parse([
        40, 49, 50, 43, 51, 41, 42, 40, 52, 43, 53, 41, -1, -1, -1, -1,
    ]) != 135
    {
        return std::process::ExitCode::FAILURE;
    }
    // 1+2*3
    if parse([
        49, 43, 50, 42, 51, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    ]) != 7
    {
        return std::process::ExitCode::FAILURE;
    }
    //  (7+8)*2
    if parse([
        32, 40, 55, 43, 56, 41, 42, 50, 32, -1, -1, -1, -1, -1, -1, -1,
    ]) != 30
    {
        return std::process::ExitCode::FAILURE;
    }
    // 1+
    if parse([
        49, 43, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    ]) != 255
    {
        return std::process::ExitCode::FAILURE;
    }
    // (1+2
    if parse([
        40, 49, 43, 50, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    ]) != 255
    {
        return std::process::ExitCode::FAILURE;
    }
    // 1x
    if parse([
        49, 120, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
    ]) != 255
    {
        return std::process::ExitCode::FAILURE;
    }
    std::process::ExitCode::SUCCESS
}
