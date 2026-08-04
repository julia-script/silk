import * as Expression from './Expression.js'

export interface Parameter {
  readonly name: string
  readonly start: number
  readonly end: number
}

export interface FunctionDefinition {
  readonly name: string
  readonly parameters: ReadonlyArray<Parameter>
  readonly body: Expression.Expression
  readonly start: number
  readonly end: number
}

export interface Program {
  readonly functions: ReadonlyArray<FunctionDefinition>
  readonly start: number
  readonly end: number
}

export const parameter = (name: string, start: number, end: number): Parameter =>
  Object.freeze({ name, start, end })

export const functionDefinition = (
  name: string,
  parameters: ReadonlyArray<Parameter>,
  body: Expression.Expression,
  start: number,
  end: number,
): FunctionDefinition =>
  Object.freeze({ name, parameters: Object.freeze(parameters), body, start, end })

export const make = (
  functions: ReadonlyArray<FunctionDefinition>,
  start: number,
  end: number,
): Program => Object.freeze({ functions: Object.freeze(functions), start, end })

export const renderFunction = (self: FunctionDefinition): string =>
  `(fn ${self.name} (${self.parameters.map((parameter_) => parameter_.name).join(' ')}) ${Expression.render(self.body)})`

export const render = (self: Program): string =>
  `(program${self.functions.map((definition) => ` ${renderFunction(definition)}`).join('')})`
