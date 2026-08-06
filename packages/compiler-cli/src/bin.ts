#!/usr/bin/env node
import { NodeRuntime, NodeServices } from '@effect/platform-node'
import * as Effect from 'effect/Effect'
import { Command } from 'effect/unstable/cli'
import * as Cli from './Cli.js'

/**
 * The application edge. Platform services are provided exactly once, here, so every module
 * inward of this file stays a pure Effect describing what to do rather than how to reach Node.
 */
Cli.command.pipe(
  Command.run({ version: '0.0.0' }),
  Effect.provide(NodeServices.layer),
  NodeRuntime.runMain,
)
