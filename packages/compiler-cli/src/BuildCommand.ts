import * as Console from 'effect/Console'
import * as Effect from 'effect/Effect'
import * as Option from 'effect/Option'
import * as Result from 'effect/Result'
import { Command } from 'effect/unstable/cli'
import * as CommandExit from './CommandExit.js'
import * as ProjectOptions from './ProjectOptions.js'
import * as Workflow from './Workflow.js'

export const command = Command.make(
  'build',
  {
    manifestPath: ProjectOptions.manifestPath,
    backend: ProjectOptions.backend,
    targets: ProjectOptions.targets,
    profile: ProjectOptions.profile,
    release: ProjectOptions.release,
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
    yield* CommandExit.complete(yield* Workflow.build(options.success))
  }),
).pipe(Command.withDescription('Build the nearest Silk project.'))
