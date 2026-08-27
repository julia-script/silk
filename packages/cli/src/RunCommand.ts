import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import * as Result from 'effect/Result'
import { Argument, Command } from 'effect/unstable/cli'
import * as CommandExit from './CommandExit.js'
import * as ProjectOptions from './ProjectOptions.js'
import * as Workflow from './Workflow.js'

const arguments_ = Argument.string('arguments').pipe(
  Argument.withDescription('Arguments passed literally to the compiled program after `--`.'),
  Argument.variadic(),
)

export const command = Command.make(
  'run',
  {
    manifestPath: ProjectOptions.manifestPath,
    backend: ProjectOptions.backend,
    targets: ProjectOptions.targets,
    profile: ProjectOptions.profile,
    release: ProjectOptions.release,
    arguments: arguments_,
  },
  Effect.fnUntraced(function* (config) {
    const options = ProjectOptions.resolve({
      ...(Option.isNone(config.manifestPath) ? {} : { manifestPath: config.manifestPath.value }),
      ...(Option.isNone(config.backend) ? {} : { backend: config.backend.value }),
      targets: config.targets,
      ...(Option.isNone(config.profile) ? {} : { profile: config.profile.value }),
      release: config.release,
    })
    if (Result.isFailure(options)) {
      yield* Console.error(options.failure.message)
      return yield* CommandExit.complete(2)
    }
    yield* CommandExit.complete(yield* Workflow.run(options.success, config.arguments))
  }),
).pipe(Command.withDescription('Build and run the nearest Silk project.'))
