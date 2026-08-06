import { Command } from 'effect/unstable/cli'
import * as CompileCommand from './CompileCommand.js'

/**
 * The `silk` root command. It owns only composition — every subcommand keeps its own module — so
 * adding a command never edits a shared switch.
 */
export const command = Command.make('silk').pipe(
  Command.withDescription('The Silk Effect bootstrap compiler.'),
  Command.withSubcommands([CompileCommand.command]),
)
