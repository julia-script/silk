/**
 * Builds the Silk extension and language server, then retargets the local editor
 * extensions symlink to this checkout's apps/vscode. Replaces dangling or
 * wrong-worktree links via ln -sfn.
 *
 * Usage:
 *   node scripts/install-local.mjs              # Cursor (~/.cursor/extensions)
 *   node scripts/install-local.mjs --vscode     # also VS Code (~/.vscode/extensions)
 *   node scripts/install-local.mjs --vscode-only
 */
import { execFileSync } from 'node:child_process'
import { existsSync, mkdirSync, rmSync, symlinkSync } from 'node:fs'
import { homedir } from 'node:os'
import { dirname, join, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'

const EXTENSION_DIR_NAME = 'silk-effect.silk-language-0.0.0'
const packageRoot = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const repoRoot = resolve(packageRoot, '../..')

const args = new Set(process.argv.slice(2))
const installCursor = !args.has('--vscode-only')
const installVscode = args.has('--vscode') || args.has('--vscode-only')

if (args.has('--help') || args.has('-h')) {
  console.log(`Usage: node scripts/install-local.mjs [--vscode] [--vscode-only]

  (default)     Symlink into ~/.cursor/extensions
  --vscode      Also symlink into ~/.vscode/extensions
  --vscode-only Only symlink into ~/.vscode/extensions
`)
  process.exit(0)
}

const fail = (message) => {
  console.error(`install-local: ${message}`)
  process.exit(1)
}

const run = (command, commandArgs, cwd) => {
  try {
    execFileSync(command, commandArgs, { cwd, stdio: 'inherit' })
  } catch {
    fail(`${command} ${commandArgs.join(' ')} failed`)
  }
}

console.log('Building @silklang/lsp and silk-language…')
run('pnpm', ['--filter', '@silklang/lsp', '--filter', 'silk-language', 'run', 'build'], repoRoot)

const entrypoint = join(packageRoot, 'dist/extension.js')
if (!existsSync(entrypoint)) {
  fail(`expected extension entrypoint at ${entrypoint}`)
}

const linkExtension = (extensionsRoot, editorLabel) => {
  mkdirSync(extensionsRoot, { recursive: true })
  const linkPath = join(extensionsRoot, EXTENSION_DIR_NAME)

  try {
    // Removes a prior symlink (including dangling / wrong-worktree targets) without
    // following it into the old checkout.
    rmSync(linkPath, { recursive: true, force: true })
    symlinkSync(packageRoot, linkPath)
  } catch (cause) {
    fail(`could not link ${linkPath} -> ${packageRoot}: ${String(cause)}`)
  }

  console.log(`${editorLabel}: ${linkPath} -> ${packageRoot}`)
}

if (installCursor) {
  linkExtension(join(homedir(), '.cursor/extensions'), 'Cursor')
}
if (installVscode) {
  linkExtension(join(homedir(), '.vscode/extensions'), 'VS Code')
}

console.log(`
Installed. Reload the editor window (Developer: Reload Window) so contributions load from this checkout.

After rebuilding only the language server, run "Silk: Restart Language Server" instead of reinstalling.
`)
