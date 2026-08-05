export const acceptedSource = 'pub fn main() -> I32 { return 42 }'
export const emptySource = new Uint8Array()

export const denseTriviaSource = `pub // visibility
fn // declaration
main // name
( // open parameters
) // close parameters
-> // return arrow
I32 // return type
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
          children: Object.freeze(['Whitespace', 'Arrow', 'Whitespace', 'Identifier']),
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

export const missingNameSource = 'pub fn () -> I32 { return 42 }'
export const missingRightBraceSource = 'pub fn main() -> I32 { return 42'
export const unexpectedPunctuationSource = 'pub fn @ main() -> I32 { return 42 }'
export const whollyUnrelatedSource = '@@@'
export const twoFunctionSource = `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return 0 }`
export const threeFunctionSource = `pub fn one() -> I32 { return 1 }
pub fn two() -> I32 { return 2 }
pub fn three() -> I32 { return 3 }`
export const missingFirstRightBraceSource = `pub fn answer() -> I32 { return 42
pub fn main() -> I32 { return 0 }`
export const interFunctionPunctuationSource = `pub fn answer() -> I32 { return 42 }
@@
pub fn main() -> I32 { return 0 }`
export const trailingTriviaSource = `pub fn main() -> I32 { return 42 }
// trailing source trivia
`
export const validCallSource = `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return answer() }`
export const triviaCallSource = `pub fn answer() -> I32 { return 42 }
pub fn main() -> I32 { return answer // callee
  ( // open call
  // empty call
  ) }`
export const missingCallCalleeSource = 'pub fn main() -> I32 { return () }'
export const missingCallRightParenthesisSource = 'pub fn main() -> I32 { return answer( }'
export const unsupportedCallArgumentSource = 'pub fn main() -> I32 { return answer(42) }'
export const damagedCallBeforeNextFunctionSource = `pub fn main() -> I32 { return answer(
pub fn after() -> I32 { return 0 }`

export const invalidUtf8Source = Uint8Array.of(
  ...Array.from('pub fn ', (character) => character.charCodeAt(0)),
  0xff,
  0xfe,
  ...Array.from(' main() -> I32 { return 42 }', (character) => character.charCodeAt(0)),
)
