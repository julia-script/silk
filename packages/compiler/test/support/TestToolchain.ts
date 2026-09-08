import { NodeServices } from '@effect/platform-node'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as FileSystem from 'effect/FileSystem'
import * as Path from 'effect/Path'
import type * as NativeToolchain from '../../src/NativeToolchain.js'

/** Test application edge selecting the configured LLVM tools, including local Homebrew installs. */
export const configured = Effect.gen(function* () {
  const fs = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  let clang = yield* Config.string('SILK_TEST_CLANG').pipe(Config.withDefault(''))
  if (clang.length === 0) {
    clang = 'clang'
    for (const candidate of ['/opt/homebrew/opt/llvm/bin/clang', '/usr/local/opt/llvm/bin/clang']) {
      if (yield* fs.exists(candidate)) {
        clang = candidate
        break
      }
    }
  }
  const adjacentAr = path.join(path.dirname(clang), 'llvm-ar')
  const llvmAr = yield* Config.string('SILK_TEST_LLVM_AR').pipe(
    Config.withDefault((yield* fs.exists(adjacentAr)) ? adjacentAr : 'llvm-ar'),
  )
  const selected: NativeToolchain.Toolchain = { _tag: 'Toolchain', clang, llvmAr }
  return selected
}).pipe(Effect.provide(NodeServices.layer))
