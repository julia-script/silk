import * as Constraint from './Constraint.js'
import * as Canonical from './internal/Canonical.js'
import * as Type from './Type.js'

export type FunctionKind = 'Function' | 'Effect'
export type ParameterMode = 'Value' | 'Shared' | 'Exclusive' | 'Take'

export interface Parameter {
  readonly type: Type.Type
  readonly mode: ParameterMode
}

export interface CaptureRelationship {
  readonly parameter: number
  readonly capture: number
}

/** One source/intrinsic-neutral callable schema consumed by common call analysis. */
export interface CallableContract {
  readonly functionKind: FunctionKind
  /** Whether invoking the complete callable transfers a caller-owned safety obligation. */
  readonly unsafe: boolean
  readonly binders: ReadonlyArray<Type.Parameter>
  readonly parameters: ReadonlyArray<Parameter>
  readonly result: Type.Type
  readonly constraints: ReadonlyArray<Constraint.Constraint>
  readonly captures: ReadonlyArray<CaptureRelationship>
}

export const make = (options: {
  readonly functionKind: FunctionKind
  readonly unsafe?: boolean
  readonly binders?: ReadonlyArray<Type.Parameter>
  readonly parameters?: ReadonlyArray<Parameter>
  readonly result: Type.Type
  readonly constraints?: ReadonlyArray<Constraint.Constraint>
  readonly captures?: ReadonlyArray<CaptureRelationship>
}): CallableContract =>
  Object.freeze({
    functionKind: options.functionKind,
    unsafe: options.unsafe ?? false,
    binders: Object.freeze(Array.from(options.binders ?? [])),
    parameters: Object.freeze(
      Array.from(options.parameters ?? [], (parameter) => Object.freeze({ ...parameter })),
    ),
    result: options.result,
    constraints: Object.freeze(Array.from(options.constraints ?? [])),
    captures: Object.freeze(
      Array.from(options.captures ?? [], (relationship) => Object.freeze({ ...relationship })),
    ),
  })

export const key = (self: CallableContract): string =>
  Canonical.record('CallableContract', [
    self.functionKind,
    self.unsafe ? 'unsafe' : 'safe',
    Canonical.array(self.binders.map(Type.key)),
    Canonical.array(
      self.parameters.map((parameter) =>
        Canonical.record('Parameter', [parameter.mode, Type.key(parameter.type)]),
      ),
    ),
    Type.key(self.result),
    Canonical.array(self.constraints.map(Constraint.key)),
    Canonical.array(
      self.captures.map((capture) =>
        Canonical.record('Capture', [`${capture.parameter}`, `${capture.capture}`]),
      ),
    ),
  ])
