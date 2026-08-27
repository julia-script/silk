import * as Bitcode from '@silklang/llvm/Bitcode'
import * as Block from '@silklang/llvm/Block'
import * as Builder from '@silklang/llvm/Builder'
import * as Constant from '@silklang/llvm/Constant'
import * as FunctionActor from '@silklang/llvm/Function'
import * as FunctionBody from '@silklang/llvm/FunctionBody'
import * as IrText from '@silklang/llvm/IrText'
import type * as LlvmError from '@silklang/llvm/LlvmError'
import * as Type from '@silklang/llvm/Type'
import * as Value from '@silklang/llvm/Value'
import * as Effect from 'effect/Effect'
import * as Diagnostic from './Diagnostic.js'
import type * as Expression from './Expression.js'
import * as Lexer from './Lexer.js'
import * as Parser from './Parser.js'
import type * as Program from './Program.js'
import type * as Token from './Token.js'

const unsupported = (program: Program.Program, message: string): Diagnostic.CompileError =>
  new Diagnostic.CompileError({
    operation: 'Compiler.compile',
    message,
    start: program.start,
    end: program.end,
    reason: { _tag: 'UnsupportedProgram' },
  })

const compileError = (
  operation: string,
  message: string,
  start: number,
  end: number,
  reason: Diagnostic.CompileReason,
): Diagnostic.CompileError =>
  new Diagnostic.CompileError({ operation, message, start, end, reason })

const resolutionError = (
  operation: string,
  message: string,
  start: number,
  end: number,
  reason: Diagnostic.ResolutionReason,
): Diagnostic.ResolutionError =>
  new Diagnostic.ResolutionError({ operation, message, start, end, reason })

interface FunctionEntry {
  readonly handle: FunctionActor.Function
  readonly arity: number
  readonly definition: Program.FunctionDefinition
}

interface LoweringContext {
  readonly builder: Builder.Builder
  readonly body: FunctionBody.FunctionBody
  readonly i32: Type.Type
  readonly functions: ReadonlyMap<string, FunctionEntry>
  readonly parameters: ReadonlyMap<string, Value.Value>
  currentBlock: Block.Block
  nextBlock: number
  nextValue: number
}

const freshName = (context: LoweringContext, base: string): string => {
  const name = `${base}_${context.nextValue}`
  context.nextValue += 1
  return name
}

const unsupportedExpression = (
  expression: Expression.Expression,
  message: string,
): Diagnostic.CompileError =>
  new Diagnostic.CompileError({
    operation: 'Compiler.lowerExpression',
    message,
    start: expression.start,
    end: expression.end,
    reason: { _tag: 'UnsupportedProgram' },
  })

const lowerExpression = Effect.fnUntraced(function* (
  context: LoweringContext,
  expression: Expression.Expression,
): Effect.fn.Return<
  Value.Input,
  Diagnostic.CompileError | Diagnostic.ResolutionError | LlvmError.LlvmError
> {
  switch (expression._tag) {
    case 'Integer':
      return yield* Constant.integerSigned(context.builder, context.i32, expression.value)
    case 'Name': {
      const value = context.parameters.get(expression.name)
      return value === undefined
        ? yield* resolutionError(
            'Compiler.lowerExpression',
            `Unknown parameter ${JSON.stringify(expression.name)}`,
            expression.start,
            expression.end,
            { _tag: 'UnknownName', name: expression.name },
          )
        : value
    }
    case 'Unary':
      return yield* FunctionBody.negate(
        context.body,
        yield* lowerExpression(context, expression.operand),
        freshName(context, 'negated'),
      )
    case 'Binary': {
      const left = yield* lowerExpression(context, expression.left)
      const right = yield* lowerExpression(context, expression.right)
      switch (expression.operator) {
        case '+':
          return yield* FunctionBody.binary(
            context.body,
            'add',
            left,
            right,
            freshName(context, 'added'),
          )
        case '-':
          return yield* FunctionBody.binary(
            context.body,
            'sub',
            left,
            right,
            freshName(context, 'subtracted'),
          )
        case '*':
          return yield* FunctionBody.binary(
            context.body,
            'mul',
            left,
            right,
            freshName(context, 'multiplied'),
          )
        case '/':
          return yield* FunctionBody.binary(
            context.body,
            'sdiv',
            left,
            right,
            freshName(context, 'divided'),
          )
        case '%':
          return yield* FunctionBody.binary(
            context.body,
            'srem',
            left,
            right,
            freshName(context, 'remainder'),
          )
        case '<':
        case '>': {
          const comparison = yield* FunctionBody.integerCompare(
            context.body,
            expression.operator === '<' ? 'slt' : 'sgt',
            left,
            right,
            freshName(context, 'comparison'),
          )
          return yield* FunctionBody.cast(
            context.body,
            'zext',
            comparison,
            context.i32,
            freshName(context, 'comparison_i32'),
          )
        }
      }
      return yield* unsupportedExpression(expression, 'Unknown Tiny binary operator')
    }
    case 'Call': {
      const target = context.functions.get(expression.callee)
      if (target === undefined) {
        return yield* resolutionError(
          'Compiler.lowerExpression',
          `Unknown function ${JSON.stringify(expression.callee)}`,
          expression.start,
          expression.end,
          { _tag: 'UnknownFunction', name: expression.callee },
        )
      }
      if (target.arity !== expression.arguments.length) {
        return yield* resolutionError(
          'Compiler.lowerExpression',
          `${expression.callee} expects ${target.arity} arguments, received ${expression.arguments.length}`,
          expression.start,
          expression.end,
          {
            _tag: 'WrongArity',
            name: expression.callee,
            expected: target.arity,
            actual: expression.arguments.length,
          },
        )
      }
      const arguments_: Array<Value.Input> = []
      for (const argument of expression.arguments) {
        arguments_.push(yield* lowerExpression(context, argument))
      }
      const result = yield* FunctionBody.callDirect(
        context.body,
        target.handle,
        arguments_,
        freshName(context, 'called'),
      )
      if (result !== undefined) return result
      return yield* compileError(
        'Compiler.lowerExpression',
        `Tiny function ${JSON.stringify(expression.callee)} unexpectedly returned void`,
        expression.start,
        expression.end,
        { _tag: 'MissingCallResult', name: expression.callee },
      )
    }
    case 'If': {
      const condition = yield* lowerExpression(context, expression.condition)
      const zero = yield* Constant.integerSigned(context.builder, context.i32, 0)
      const branchCondition = yield* FunctionBody.integerCompare(
        context.body,
        'ne',
        condition,
        zero,
        freshName(context, 'condition'),
      )
      const blockId = context.nextBlock
      context.nextBlock += 1
      const onTrueBlock = yield* Block.make(context.body, `if_true_${blockId}`)
      const onFalseBlock = yield* Block.make(context.body, `if_false_${blockId}`)
      const mergeBlock = yield* Block.make(context.body, `if_merge_${blockId}`)
      yield* FunctionBody.conditionalBranch(
        context.body,
        branchCondition,
        onTrueBlock,
        onFalseBlock,
      )

      yield* Block.setInsertionPoint(context.body, onTrueBlock)
      context.currentBlock = onTrueBlock
      const onTrue = yield* lowerExpression(context, expression.onTrue)
      const onTruePredecessor = context.currentBlock
      yield* FunctionBody.branch(context.body, mergeBlock)

      yield* Block.setInsertionPoint(context.body, onFalseBlock)
      context.currentBlock = onFalseBlock
      const onFalse = yield* lowerExpression(context, expression.onFalse)
      const onFalsePredecessor = context.currentBlock
      yield* FunctionBody.branch(context.body, mergeBlock)

      yield* Block.setInsertionPoint(context.body, mergeBlock)
      context.currentBlock = mergeBlock
      const phi = yield* FunctionBody.phi(
        context.body,
        context.i32,
        freshName(context, 'if_result'),
      )
      yield* FunctionBody.addPhiIncoming(context.body, phi, onTrue, onTruePredecessor)
      yield* FunctionBody.addPhiIncoming(context.body, phi, onFalse, onFalsePredecessor)
      return yield* FunctionBody.sealPhi(context.body, phi)
    }
  }
})

const buildProgram = Effect.fnUntraced(function* (
  program: Program.Program,
): Effect.fn.Return<
  Builder.Builder,
  Diagnostic.CompileError | Diagnostic.ResolutionError | LlvmError.LlvmError
> {
  const functionNames = new Set<string>()
  for (const definition of program.functions) {
    if (functionNames.has(definition.name)) {
      return yield* resolutionError(
        'Compiler.compile',
        `Duplicate function ${JSON.stringify(definition.name)}`,
        definition.start,
        definition.end,
        { _tag: 'DuplicateFunction', name: definition.name },
      )
    }
    functionNames.add(definition.name)
  }

  const mainDefinition = program.functions.find((definition) => definition.name === 'main')
  if (mainDefinition === undefined || mainDefinition.parameters.length !== 0) {
    return yield* resolutionError(
      'Compiler.compile',
      'Tiny programs require fn main() with no parameters',
      mainDefinition?.start ?? program.start,
      mainDefinition?.end ?? program.end,
      { _tag: 'InvalidMain' },
    )
  }

  const builder = yield* Builder.make({
    moduleName: 'tiny-language',
    sourceFilename: 'program.tiny',
  })
  const i32 = yield* Type.integer(builder, 32)
  const functions = new Map<string, FunctionEntry>()

  for (const definition of program.functions) {
    const parameterTypes = Array.from({ length: definition.parameters.length }, () => i32)
    const signature = yield* Type.functionType(builder, i32, parameterTypes)
    const handle = yield* FunctionActor.declare(builder, definition.name, signature)
    functions.set(
      definition.name,
      Object.freeze({ handle, arity: definition.parameters.length, definition }),
    )
  }

  for (const definition of program.functions) {
    const entry = functions.get(definition.name)
    if (entry === undefined) {
      return yield* unsupported(program, 'A declared Tiny function is missing from its table')
    }
    yield* FunctionActor.buildBody(
      builder,
      entry.handle,
      Effect.fnUntraced(function* (body) {
        const entryBlock = yield* Block.make(body, 'entry')
        const parameters = new Map<string, Value.Value>()
        for (const [index, parameter] of definition.parameters.entries()) {
          parameters.set(parameter.name, yield* Value.argument(body, index))
        }
        const value = yield* lowerExpression(
          {
            builder,
            body,
            i32,
            functions,
            parameters,
            currentBlock: entryBlock,
            nextBlock: 0,
            nextValue: 0,
          },
          definition.body,
        )
        yield* FunctionBody.returnValue(body, value)
      }),
    )
  }

  return builder
})

export interface Compilation {
  readonly source: string
  readonly tokens: ReadonlyArray<Token.Token>
  readonly program: Program.Program
  readonly ir: string
  readonly bitcode: Uint8Array
}

export const compile = Effect.fn('Compiler.compile')(function* (
  source: string,
): Effect.fn.Return<
  Compilation,
  | Diagnostic.LexError
  | Diagnostic.ParseError
  | Diagnostic.ResolutionError
  | Diagnostic.CompileError
  | LlvmError.LlvmError
> {
  const tokens = yield* Lexer.tokenize(source)
  const program = yield* Parser.parse(tokens)
  const builder = yield* buildProgram(program)
  const ir = yield* IrText.render(builder)
  const bitcode = yield* Bitcode.encode(builder)
  return Object.freeze({ source, tokens, program, ir, bitcode })
})
