export const acceptedSource = 'pub fn main() -> i32 { return 42 }'
export const emptySource = new Uint8Array()

export const denseTriviaSource = `pub // visibility
fn // declaration
main // name
( // open parameters
) // close parameters
-> // return arrow
i32 // return type
{ // block
return // statement
42 // value
}`

export interface ExpectedNodeShape {
  readonly kind: string
  readonly children: ReadonlyArray<string | ExpectedNodeShape>
}

export const acceptedShape: ExpectedNodeShape = Object.freeze({
  kind: 'SourceFile',
  children: Object.freeze([
    Object.freeze({
      kind: 'FunctionDeclaration',
      children: Object.freeze([
        'PubKeyword',
        'Whitespace',
        'FnKeyword',
        'Whitespace',
        'Identifier',
        Object.freeze({
          kind: 'ParameterList',
          children: Object.freeze(['LeftParenthesis', 'RightParenthesis']),
        }),
        Object.freeze({
          kind: 'ReturnType',
          children: Object.freeze([
            'Whitespace',
            'Arrow',
            Object.freeze({
              kind: 'TypePath',
              children: Object.freeze(['Whitespace', 'Identifier']),
            }),
          ]),
        }),
        Object.freeze({
          kind: 'Block',
          children: Object.freeze([
            'Whitespace',
            'LeftBrace',
            Object.freeze({
              kind: 'ReturnStatement',
              children: Object.freeze([
                'Whitespace',
                'ReturnKeyword',
                Object.freeze({
                  kind: 'IntegerLiteralExpression',
                  children: Object.freeze(['Whitespace', 'DecimalInteger']),
                }),
              ]),
            }),
            'Whitespace',
            'RightBrace',
          ]),
        }),
      ]),
    }),
    'EndOfFile',
  ]),
})

export const missingNameSource = 'pub fn () -> i32 { return 42 }'
export const missingRightBraceSource = 'pub fn main() -> i32 { return 42'
export const unexpectedPunctuationSource = 'pub fn ^ main() -> i32 { return 42 }'
export const whollyUnrelatedSource = '^^^'
export const twoFunctionSource = `pub fn answer() -> i32 { return 42 }
pub fn main() -> i32 { return 0 }`
export const threeFunctionSource = `pub fn one() -> i32 { return 1 }
pub fn two() -> i32 { return 2 }
pub fn three() -> i32 { return 3 }`
export const missingFirstRightBraceSource = `pub fn answer() -> i32 { return 42
pub fn main() -> i32 { return 0 }`
export const interFunctionPunctuationSource = `pub fn answer() -> i32 { return 42 }
^^
pub fn main() -> i32 { return 0 }`
export const trailingTriviaSource = `pub fn main() -> i32 { return 42 }
// trailing source trivia
`
export const validCallSource = `pub fn answer() -> i32 { return 42 }
pub fn main() -> i32 { return answer() }`
export const triviaCallSource = `pub fn answer() -> i32 { return 42 }
pub fn main() -> i32 { return answer // callee
  ( // open call
  // empty call
  ) }`
export const missingCallCalleeSource = 'pub fn main() -> i32 { return () }'
export const missingCallRightParenthesisSource = 'pub fn main() -> i32 { return answer( }'
export const valueCallArgumentSource = 'pub fn main() -> i32 { return answer(42) }'
export const identifierCallArgumentSource =
  'pub fn main(value: i32) -> i32 { return answer(value) }'
export const identitySource = 'pub fn identity(value: i32) -> i32 { return value }'
export const twoParameterSource = 'pub fn choose(left: i32, right: i32) -> i32 { return left }'
export const missingParameterTypeSource = 'pub fn identity(value:) -> i32 { return value }'
export const missingParameterCommaSource =
  'pub fn choose(left: i32 right: i32) -> i32 { return left }'
export const malformedArgumentSource = 'pub fn main(value: i32) -> i32 { return answer(:, value) }'
export const damagedCallBeforeNextFunctionSource = `pub fn main() -> i32 { return answer(
pub fn after() -> i32 { return 0 }`
export const nestedCallSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)) }`
export const nestedSiblingCallSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn choose(left: i32, right: i32) -> i32 { return left }
pub fn main() -> i32 { return choose(identity(1), identity(2)) }`
export const damagedNestedSiblingSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn choose(left: i32, right: i32) -> i32 { return left }
pub fn main() -> i32 { return choose(identity(:), identity(2)) }`
export const missingNestedRightParenthesisSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42) }`
export const damagedNestedBeforeNextFunctionSource = `pub fn identity(value: i32) -> i32 { return value }
pub fn main() -> i32 { return identity(identity(42)
pub fn after() -> i32 { return 0 }`
export const validStructSource =
  'pub struct Token { pub kind: i32 lexeme: Text.Node }\n' +
  'struct Marker {}\n' +
  'pub fn main(value: Text.Node) -> Text.Node { return value }'
export const damagedStructSource =
  'pub struct Damaged { pub : i32 missing: next: bool }\n' +
  'struct Open { value: i32\n' +
  'pub fn after() -> i32 { return 1 }'

export const invalidUtf8Source = Uint8Array.of(
  ...Array.from('pub fn ', (character) => character.charCodeAt(0)),
  0xff,
  0xfe,
  ...Array.from(' main() -> i32 { return 42 }', (character) => character.charCodeAt(0)),
)
