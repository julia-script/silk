export type UnaryOperator = '-'
export type BinaryOperator = '+' | '-' | '*' | '/' | '%' | '<' | '>'

interface Spanned {
  readonly start: number
  readonly end: number
}

export interface Integer extends Spanned {
  readonly _tag: 'Integer'
  readonly value: bigint
}

export interface Name extends Spanned {
  readonly _tag: 'Name'
  readonly name: string
}

export interface Unary extends Spanned {
  readonly _tag: 'Unary'
  readonly operator: UnaryOperator
  readonly operand: Expression
}

export interface Binary extends Spanned {
  readonly _tag: 'Binary'
  readonly operator: BinaryOperator
  readonly left: Expression
  readonly right: Expression
}

export interface Call extends Spanned {
  readonly _tag: 'Call'
  readonly callee: string
  readonly arguments: ReadonlyArray<Expression>
}

export interface If extends Spanned {
  readonly _tag: 'If'
  readonly condition: Expression
  readonly onTrue: Expression
  readonly onFalse: Expression
}

export type Expression = Integer | Name | Unary | Binary | Call | If

export const integer = (value: bigint, start: number, end: number): Integer =>
  Object.freeze({ _tag: 'Integer', value, start, end })

export const name = (value: string, start: number, end: number): Name =>
  Object.freeze({ _tag: 'Name', name: value, start, end })

export const unary = (
  operator: UnaryOperator,
  operand: Expression,
  start: number,
  end: number,
): Unary => Object.freeze({ _tag: 'Unary', operator, operand, start, end })

export const binary = (
  operator: BinaryOperator,
  left: Expression,
  right: Expression,
  start: number,
  end: number,
): Binary => Object.freeze({ _tag: 'Binary', operator, left, right, start, end })

export const call = (
  callee: string,
  arguments_: ReadonlyArray<Expression>,
  start: number,
  end: number,
): Call => Object.freeze({ _tag: 'Call', callee, arguments: Object.freeze(arguments_), start, end })

export const ifExpression = (
  condition: Expression,
  onTrue: Expression,
  onFalse: Expression,
  start: number,
  end: number,
): If => Object.freeze({ _tag: 'If', condition, onTrue, onFalse, start, end })

export const render = (self: Expression): string => {
  switch (self._tag) {
    case 'Integer':
      return self.value.toString()
    case 'Name':
      return self.name
    case 'Unary':
      return `(${self.operator} ${render(self.operand)})`
    case 'Binary':
      return `(${self.operator} ${render(self.left)} ${render(self.right)})`
    case 'Call':
      return `(call ${self.callee}${self.arguments.map((argument) => ` ${render(argument)}`).join('')})`
    case 'If':
      return `(if ${render(self.condition)} ${render(self.onTrue)} ${render(self.onFalse)})`
  }
}
