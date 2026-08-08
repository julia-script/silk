import * as Effect from 'effect/Effect'
import * as Analysis from '../../dist/Analysis.js'
import * as Type from '../../dist/Type.js'

const source = `fn identity(value: I32) -> I32 { return value }
fn select<T>(value: T, enabled: Bool) -> T { return value }
pub fn main() -> I32 {
  let named = identity
  let plusTwo = I32.add(2)
  let whenEnabled = select(true)
  return named(plusTwo(whenEnabled(40)))
}`

const snapshot = await Effect.runPromise(
  Analysis.ofSource('fixture/CallableSemantics', new TextEncoder().encode(source)),
)
const result = snapshot.results.get('fixture/CallableSemantics')

const expression = (fact) => ({
  tag: fact._tag,
  type: fact.type._tag === 'Available' ? Type.key(fact.type.type) : 'unavailable',
  ...(fact._tag === 'FunctionItem'
    ? { reference: fact.reference.spelling }
    : fact._tag === 'CallableSection'
      ? {
          site: fact.site,
          reference: fact.reference.spelling,
          omittedParameter: fact.omittedParameter,
          captures: fact.captures.map((capture) => ({
            ordinal: capture.ordinal,
            parameterOrdinal: capture.parameterOrdinal,
            access: capture.access,
          })),
          mode: fact.mode,
          typeArguments: fact.typeArguments.map(Type.key),
          substitution: [...fact.substitution].map(([parameter, type]) => [
            parameter,
            Type.key(type),
          ]),
        }
      : fact._tag === 'CallableApply'
        ? {
            mode: fact.mode,
            provenance: fact.provenance._tag,
            callee: expression(fact.callee),
            arguments: fact.arguments.map((argument) => expression(argument.expression)),
          }
        : {}),
})

process.stdout.write(
  JSON.stringify({
    functions: (result?.functions ?? []).map((fn) => ({
      name: fn.declaration.name._tag === 'Present' ? fn.declaration.name.spelling : '?',
      bindings: fn.statements.flatMap((statement) =>
        statement._tag === 'BindStatement' ? [expression(statement.binding.initializer)] : [],
      ),
      returned: expression(fn.returnedExpression),
    })),
    diagnostics: (result?.diagnostics ?? []).map((diagnostic) => diagnostic.code),
  }),
)
