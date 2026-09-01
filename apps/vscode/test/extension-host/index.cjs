const vscode = require('vscode')

const timeout = async (promise, label, milliseconds = 15_000) => {
  let timer
  try {
    return await Promise.race([
      promise,
      new Promise((_, reject) => {
        timer = setTimeout(() => reject(new Error(`Timed out waiting for ${label}`)), milliseconds)
      }),
    ])
  } finally {
    clearTimeout(timer)
  }
}

const waitFor = async (condition, label, milliseconds = 15_000) => {
  const started = performance.now()
  while (performance.now() - started < milliseconds) {
    const value = condition()
    if (value !== undefined) return value
    await new Promise((resolve) => setTimeout(resolve, 50))
  }
  throw new Error(`Timed out waiting for ${label}`)
}

const source = (line = '') => `import silk.vector as Vector
import Util

fn outer() -> Effect<i32> {
  let n = 123
  return effect {
    return n
  }
}

pub fn main() -> i32 {
  let mut values = Vector.make<i32>()
  let answer = Util.answer()
${line}
  return run outer()
}
`

const replace = async (editor, text) => {
  const document = editor.document
  const replaced = await editor.edit((edit) => {
    edit.replace(
      new vscode.Range(document.positionAt(0), document.positionAt(document.getText().length)),
      text,
    )
  })
  if (!replaced) throw new Error(`Could not edit ${document.uri}`)
}

const diagnosticText = (uri) =>
  vscode.languages
    .getDiagnostics(uri)
    .map((diagnostic) => diagnostic.message)
    .join('\n')

const hover = async (document) => {
  const offset = document.getText().indexOf('123')
  if (offset < 0) throw new Error('Acceptance fixture lost its hover target')
  await timeout(
    (async () => {
      while (true) {
        const result = await vscode.commands.executeCommand(
          'vscode.executeHoverProvider',
          document.uri,
          document.positionAt(offset),
        )
        if (Array.isArray(result) && result.length > 0) return
        await new Promise((resolve) => setTimeout(resolve, 50))
      }
    })(),
    'hover readiness',
  )
}

const inlayHints = async (document) => {
  const range = new vscode.Range(
    document.positionAt(0),
    document.positionAt(document.getText().length),
  )
  await timeout(
    (async () => {
      while (true) {
        const result = await vscode.commands.executeCommand(
          'vscode.executeInlayHintProvider',
          document.uri,
          range,
        )
        const labels = Array.isArray(result)
          ? result.flatMap((hint) =>
              typeof hint.label === 'string' ? [hint.label] : hint.label.map((part) => part.value),
            )
          : []
        if (labels.includes(': i32')) return
        await new Promise((resolve) => setTimeout(resolve, 50))
      }
    })(),
    'inlay-hint readiness',
  )
}

module.exports.run = async () => {
  const workspace = vscode.workspace.workspaceFolders?.[0]
  if (workspace === undefined) throw new Error('LSP acceptance workspace was not opened')
  const extension = vscode.extensions.getExtension('silk-effect.silk-language')
  if (extension === undefined) throw new Error('Silk development extension is unavailable')
  await extension.activate()

  const mainUri = vscode.Uri.joinPath(workspace.uri, 'src', 'Main.silk')
  const utilUri = vscode.Uri.joinPath(workspace.uri, 'src', 'Util.silk')
  const main = await vscode.workspace.openTextDocument(mainUri)
  const mainEditor = await vscode.window.showTextDocument(main)
  await hover(main)
  await inlayHints(main)

  await replace(mainEditor, source('  effects'))
  await waitFor(
    () => (diagnosticText(mainUri).includes('effects') ? true : undefined),
    'effects diagnostics',
  )
  await timeout(
    vscode.commands.executeCommand(
      'vscode.executeCodeActionProvider',
      mainUri,
      new vscode.Range(12, 0, 12, 12),
    ),
    'quick fixes',
  )

  await replace(mainEditor, source('  effec'))
  if (vscode.languages.getDiagnostics(mainUri).length !== 0)
    throw new Error('Applied diagnostics were not retired synchronously after editing')
  await waitFor(
    () => (diagnosticText(mainUri).includes('effec') ? true : undefined),
    'effec diagnostics',
  )

  await replace(mainEditor, source('  Effect.'))
  if (vscode.languages.getDiagnostics(mainUri).length !== 0)
    throw new Error('Intermediate effec diagnostics survived the next editor version')
  await waitFor(
    () =>
      !diagnosticText(mainUri).includes('effec') &&
      vscode.languages.getDiagnostics(mainUri).length > 0
        ? true
        : undefined,
    'Effect. diagnostics',
  )

  await replace(mainEditor, source())
  if (vscode.languages.getDiagnostics(mainUri).length !== 0)
    throw new Error('Intermediate diagnostics survived the valid editor version')
  await waitFor(
    () => (vscode.languages.getDiagnostics(mainUri).length === 0 ? true : undefined),
    'valid diagnostics',
  )
  await new Promise((resolve) => setTimeout(resolve, 500))
  if (vscode.languages.getDiagnostics(mainUri).length !== 0)
    throw new Error(`Stale diagnostics returned: ${diagnosticText(mainUri)}`)

  const util = await vscode.workspace.openTextDocument(utilUri)
  const utilEditor = await vscode.window.showTextDocument(util)
  await replace(utilEditor, 'pub fn answer2() -> i32 {\n  return 7\n}\n')
  await waitFor(
    () => (vscode.languages.getDiagnostics(mainUri).length > 0 ? true : undefined),
    'dependency diagnostics',
  )
  await replace(utilEditor, 'pub fn answer() -> i32 {\n  return 7\n}\n')
  await waitFor(
    () => (vscode.languages.getDiagnostics(mainUri).length === 0 ? true : undefined),
    'dependency recovery',
  )

  await vscode.window.showTextDocument(main)
  const activeHover = vscode.commands.executeCommand(
    'vscode.executeHoverProvider',
    mainUri,
    main.positionAt(main.getText().indexOf('123')),
  )
  const activeQuickFix = vscode.commands.executeCommand(
    'vscode.executeCodeActionProvider',
    mainUri,
    new vscode.Range(12, 0, 12, 12),
  )
  await timeout(
    vscode.commands.executeCommand('silk.restartLanguageServer'),
    'language-server restart',
  )
  await Promise.allSettled([activeHover, activeQuickFix])
  await hover(main)
  if (vscode.languages.getDiagnostics(mainUri).length !== 0)
    throw new Error(`Restart restored stale diagnostics: ${diagnosticText(mainUri)}`)

  await main.save()
  await util.save()
  await vscode.commands.executeCommand('workbench.action.closeActiveEditor')
  await waitFor(
    () => (vscode.languages.getDiagnostics(mainUri).length === 0 ? true : undefined),
    'diagnostic removal on close',
  )
}
