// Matched in-memory benchmark: see ast.silk, ast.zig, and README.md.
#[derive(Clone, Copy)]
struct Token {
    kind: i32,
    value: i32,
    start: usize,
    end: usize,
}
struct Diagnostic {
    code: i32,
    start: usize,
    end: usize,
}
#[derive(Clone, Copy)]
enum Kind {
    Number(i32),
    Name(usize),
    Negate(usize),
    Binary {
        operator: i32,
        left: usize,
        right: usize,
    },
    Assign {
        index: usize,
        child: usize,
    },
    Error,
}
#[derive(Clone, Copy)]
struct Node {
    kind: Kind,
    start: usize,
    end: usize,
}
struct Parser {
    tokens: Vec<Token>,
    nodes: Vec<Node>,
    roots: Vec<usize>,
    diagnostics: Vec<Diagnostic>,
    position: usize,
}

// Token kinds: EOF=0, number=1, name=2, + =3, - =4, * =5,
// ( =6, ) =7, ; =8, = =9, invalid=10. Names are single ASCII letters.
fn digit(byte: u8) -> bool {
    byte >= b'0' && byte <= b'9'
}
fn punctuation(byte: u8) -> i32 {
    match byte {
        b'+' => 3,
        b'-' => 4,
        b'*' => 5,
        b'(' => 6,
        b')' => 7,
        b';' => 8,
        b'=' => 9,
        _ => 10,
    }
}
fn report(parser: &mut Parser, code: i32, start: usize, end: usize) {
    parser.diagnostics.push(Diagnostic { code, start, end });
}
fn lex(source: &str) -> Parser {
    let bytes = source.as_bytes();
    let mut parser = Parser {
        tokens: Vec::new(),
        nodes: Vec::new(),
        roots: Vec::new(),
        diagnostics: Vec::new(),
        position: 0,
    };
    let mut position = 0;
    while position < bytes.len() {
        let byte = bytes[position];
        if matches!(byte, b' ' | b'\t' | b'\n' | b'\r') {
            position += 1;
        } else if byte == b'#' {
            while position < bytes.len() && bytes[position] != b'\n' {
                position += 1;
            }
        } else {
            let start = position;
            let mut kind = punctuation(byte);
            let mut value = 0;
            if digit(byte) {
                kind = 1;
                while position < bytes.len() && digit(bytes[position]) {
                    value = value * 10 + i32::from(bytes[position]) - 48;
                    position += 1;
                }
            } else {
                position += 1;
                if byte >= b'a' && byte <= b'z' {
                    kind = 2;
                    value = i32::from(byte) - 97;
                }
                if kind == 10 {
                    report(&mut parser, 1, start, position);
                }
            }
            parser.tokens.push(Token {
                kind,
                value,
                start,
                end: position,
            });
        }
    }
    parser.tokens.push(Token {
        kind: 0,
        value: 0,
        start: position,
        end: position,
    });
    parser
}
fn current(parser: &Parser) -> Token {
    parser.tokens[parser.position]
}
fn advance(parser: &mut Parser) -> Token {
    let token = current(parser);
    if token.kind != 0 {
        parser.position += 1;
    }
    token
}
fn append(parser: &mut Parser, kind: Kind, start: usize, end: usize) -> usize {
    let id = parser.nodes.len();
    parser.nodes.push(Node { kind, start, end });
    id
}
fn primary(parser: &mut Parser) -> usize {
    let token = current(parser);
    if token.kind == 1 {
        advance(parser);
        return append(parser, Kind::Number(token.value), token.start, token.end);
    }
    if token.kind == 2 {
        advance(parser);
        return append(
            parser,
            Kind::Name(token.value as usize),
            token.start,
            token.end,
        );
    }
    if token.kind == 4 {
        advance(parser);
        let child = primary(parser);
        return append(
            parser,
            Kind::Negate(child),
            token.start,
            parser.nodes[child].end,
        );
    }
    if token.kind == 6 {
        advance(parser);
        let child = expression(parser);
        let closing = current(parser);
        if closing.kind == 7 {
            advance(parser);
        } else {
            report(parser, 3, closing.start, closing.end);
        }
        return child;
    }
    report(parser, 2, token.start, token.end);
    if token.kind != 0 && token.kind != 8 && token.kind != 7 {
        advance(parser);
    }
    append(parser, Kind::Error, token.start, token.end)
}
fn binary(parser: &mut Parser, operator: i32, left: usize, right: usize) -> usize {
    append(
        parser,
        Kind::Binary {
            operator,
            left,
            right,
        },
        parser.nodes[left].start,
        parser.nodes[right].end,
    )
}
fn term(parser: &mut Parser) -> usize {
    let mut left = primary(parser);
    while current(parser).kind == 5 {
        let operator = advance(parser).kind;
        let right = primary(parser);
        left = binary(parser, operator, left, right);
    }
    left
}
fn expression(parser: &mut Parser) -> usize {
    let mut left = term(parser);
    while current(parser).kind == 3 || current(parser).kind == 4 {
        let operator = advance(parser).kind;
        let right = term(parser);
        left = binary(parser, operator, left, right);
    }
    left
}
fn statement(parser: &mut Parser) -> usize {
    let token = current(parser);
    if token.kind == 2 && parser.tokens[parser.position + 1].kind == 9 {
        advance(parser);
        advance(parser);
        let child = expression(parser);
        return append(
            parser,
            Kind::Assign {
                index: token.value as usize,
                child,
            },
            token.start,
            parser.nodes[child].end,
        );
    }
    expression(parser)
}
fn parse(source: &str) -> Parser {
    let mut parser = lex(source);
    while current(&parser).kind != 0 {
        let root = statement(&mut parser);
        parser.roots.push(root);
        let token = current(&parser);
        if token.kind != 8 {
            report(&mut parser, 4, token.start, token.end);
            while current(&parser).kind != 0 && current(&parser).kind != 8 {
                advance(&mut parser);
            }
        }
        if current(&parser).kind == 8 {
            advance(&mut parser);
        }
    }
    parser
}
fn operation(operator: i32, left: i32, right: i32) -> i32 {
    if operator == 3 {
        left + right
    } else if operator == 4 {
        left - right
    } else {
        left * right
    }
}
// Postorder evaluation also checks that every child ID precedes its parent.
fn evaluate(parser: &Parser) -> i32 {
    let mut values = Vec::<i32>::new();
    let mut bindings = Vec::<i32>::new();
    for _ in 0..26 {
        bindings.push(0);
    }
    for node in &parser.nodes {
        let value = match node.kind {
            Kind::Number(value) => value,
            Kind::Name(index) => bindings[index],
            Kind::Negate(child) => -values[child],
            Kind::Binary {
                operator,
                left,
                right,
            } => operation(operator, values[left], values[right]),
            Kind::Assign { index, child } => {
                bindings[index] = values[child];
                values[child]
            }
            Kind::Error => 0,
        };
        values.push(value);
    }
    parser.roots.iter().map(|&id| values[id]).sum()
}
fn check(
    source: &str,
    total: i32,
    nodes: usize,
    roots: usize,
    diagnostics: usize,
    locations: usize,
) -> bool {
    let parser = parse(source);
    if parser.nodes.len() != nodes
        || parser.roots.len() != roots
        || parser.diagnostics.len() != diagnostics
    {
        return false;
    }
    let positions: usize = parser
        .diagnostics
        .iter()
        .map(|d| d.start + d.end + d.code as usize)
        .sum();
    if positions != locations {
        return false;
    }
    for node in &parser.nodes {
        if node.start > node.end || node.end > source.len() {
            return false;
        }
    }
    evaluate(&parser) == total
}
fn suite() -> bool {
    if !check(
        "a = 12; b = (a + 3) * 9; b - a * 2; -b + 140;",
        263,
        17,
        4,
        0,
        0,
    ) {
        return false;
    }
    if !check("1 + ; 2 * 3; (4 + 5; 6;", 22, 10, 4, 2, 53) {
        return false;
    }
    if !check("@; 7;", 7, 2, 2, 2, 5) {
        return false;
    }
    if !check("1 2; 3;", 4, 2, 2, 1, 9) {
        return false;
    }
    if !check("# comment\n", 0, 0, 0, 0, 0) {
        return false;
    }
    let mut source = String::new();
    for _ in 0..64 {
        source.push_str("a = 12; b = (a + 3) * 9; b - a * 2; -b + 140; # loop\n");
    }
    check(&source, 16832, 1088, 256, 0, 0)
}
fn main() -> std::process::ExitCode {
    if suite() {
        std::process::ExitCode::SUCCESS
    } else {
        std::process::ExitCode::FAILURE
    }
}
