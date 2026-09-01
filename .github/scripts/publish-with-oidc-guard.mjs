import { spawn } from 'node:child_process'
import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'

class PublishProcessError extends Data.TaggedError('PublishProcessError') {}

const warning = 'Skipped OIDC'

export function runPublish(command, args, { onOutput, spawnImpl = spawn } = {}) {
  return Effect.runPromise(
    Effect.callback((resume) => {
      const child = spawnImpl(command, args, { stdio: ['inherit', 'pipe', 'pipe'] })

      child.stdout.setEncoding('utf8')
      child.stderr.setEncoding('utf8')
      child.stdout.on('data', (chunk) => {
        process.stdout.write(chunk)
        onOutput(chunk)
      })
      child.stderr.on('data', (chunk) => {
        process.stderr.write(chunk)
        onOutput(chunk)
      })
      child.on('error', (cause) => resume(Effect.fail(new PublishProcessError({ cause }))))
      child.on('close', (code) => resume(Effect.succeed(code ?? 0)))
    }),
  )
}

export async function publishWithGuard({ spawnImpl } = {}) {
  let combined = ''
  const exitCode = await runPublish('pnpm', ['run', 'release:publish'], {
    onOutput: (chunk) => {
      combined += chunk
    },
    spawnImpl,
  })

  return { exitCode, skippedOidc: combined.includes(warning) }
}

if (import.meta.url === `file://${process.argv[1]}`) {
  const { exitCode, skippedOidc } = await publishWithGuard()

  if (skippedOidc) {
    process.stderr.write(
      '\n::error::pnpm reported "Skipped OIDC". Verify the npm trusted-publisher settings.\n',
    )
    process.exit(1)
  }

  process.exit(exitCode)
}
