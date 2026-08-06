import { Command } from 'effect/unstable/cli'
import * as BuildCommand from './BuildCommand.js'
import * as BuildExeCommand from './BuildExeCommand.js'
import * as CheckCommand from './CheckCommand.js'
import * as RunCommand from './RunCommand.js'

/**
 * The `silk` root command. It owns only composition — every subcommand keeps its own module — so
 * adding a command never edits a shared switch.
 */
export const command = Command.make('silk').pipe(
  Command.withDescription('The Silk Effect bootstrap compiler.'),
  Command.withSubcommands([
    BuildCommand.command,
    CheckCommand.command,
    RunCommand.command,
    BuildExeCommand.command,
  ]),
)
