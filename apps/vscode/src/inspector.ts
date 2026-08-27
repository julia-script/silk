/**
 * The Silk Inspector: one webview panel, one compiler-phase view at a time, for the open project.
 *
 * The panel is a dumb renderer. Every projection comes from the language server over
 * `silk/inspectorView` (the server already holds the committed analysis, and re-realizes a
 * single-root snapshot per document for backend views), and the view registry itself is fetched
 * over `silk/inspectorViews` — the extension host is CommonJS and cannot import the ESM
 * compiler inspector actors that define it.
 */
import * as vscode from 'vscode'

export interface InspectorSession {
  readonly request: <A>(method: string, parameters?: unknown) => Promise<A>
  readonly onInvalidation: (listener: () => void) => vscode.Disposable
}

interface Span {
  readonly module: string
  readonly start: number
  readonly end: number
}

interface RowModel {
  readonly key: string
  readonly lead?: string | number
  readonly depth?: number
  readonly caret?: string
  readonly dot?: string
  readonly label: string
  readonly detail?: string
  readonly span?: Span
  readonly head?: boolean
  readonly tone?: string
}

interface ViewResponse {
  readonly rows: ReadonlyArray<RowModel>
  readonly meta?: string
  readonly facts?: ReadonlyArray<{ readonly text: string; readonly tone?: string }>
  readonly unavailable?: string
  readonly version: number
  readonly moduleUris: Readonly<Record<string, string>>
}

interface ViewDescriptor {
  readonly id: string
  readonly title: string
  readonly phase: string
  readonly tag: string
  readonly hasFilter?: boolean
  readonly action?: { readonly label: string }
}

const viewRequest = 'silk/inspectorView'
const viewsRequest = 'silk/inspectorViews'
const encoder = new TextEncoder()
const decoder = new TextDecoder()

/** Byte offsets are the compiler's currency; the editor's are UTF-16 code units. */
const byteOffsetOf = (document: vscode.TextDocument, position: vscode.Position): number =>
  encoder.encode(document.getText().slice(0, document.offsetAt(position))).length

const positionOfByte = (document: vscode.TextDocument, byteOffset: number): vscode.Position => {
  const bytes = encoder.encode(document.getText())
  return document.positionAt(decoder.decode(bytes.subarray(0, byteOffset)).length)
}

export const registerInspector = (
  context: vscode.ExtensionContext,
  session: InspectorSession,
): void => {
  let panel: vscode.WebviewPanel | undefined
  let views: ReadonlyArray<ViewDescriptor> = []
  const state = {
    viewId: 'pipeline',
    filter: '',
    showTrivia: false,
    // Evaluation is explicit and describes one program: cleared on every new commit.
    evaluate: false,
    uri: undefined as string | undefined,
    moduleUris: {} as Readonly<Record<string, string>>,
  }

  const refresh = async (): Promise<void> => {
    if (panel === undefined || state.uri === undefined) return
    try {
      const result = await session.request<ViewResponse>(viewRequest, {
        uri: state.uri,
        view: state.viewId,
        filter: state.filter,
        showTrivia: state.showTrivia,
        evaluate: state.evaluate,
      })
      state.moduleUris = result.moduleUris
      void panel.webview.postMessage({
        type: 'view',
        viewId: state.viewId,
        views,
        result,
        evaluate: state.evaluate,
      })
    } catch (error) {
      void panel.webview.postMessage({
        type: 'view',
        viewId: state.viewId,
        views,
        result: undefined,
        error: error instanceof Error ? error.message : String(error),
      })
    }
  }

  const postCursor = (editor: vscode.TextEditor): void => {
    if (panel === undefined || editor.document.uri.toString() !== state.uri) return
    const selection = editor.selection
    const module = Object.entries(state.moduleUris).find(([, uri]) => uri === state.uri)?.[0]
    if (module === undefined) return
    void panel.webview.postMessage({
      type: 'cursor',
      span: selection.isEmpty
        ? undefined
        : {
            module,
            start: byteOffsetOf(editor.document, selection.start),
            end: byteOffsetOf(editor.document, selection.end),
          },
    })
  }

  const activateRow = async (span: Span): Promise<void> => {
    const uri = state.moduleUris[span.module]
    if (uri === undefined) return
    const document = await vscode.workspace.openTextDocument(vscode.Uri.parse(uri))
    const editor = await vscode.window.showTextDocument(document, {
      viewColumn: vscode.ViewColumn.One,
      preserveFocus: false,
    })
    const range = new vscode.Range(
      positionOfByte(document, span.start),
      positionOfByte(document, span.end),
    )
    editor.selection = new vscode.Selection(range.start, range.end)
    editor.revealRange(range, vscode.TextEditorRevealType.InCenterIfOutsideViewport)
  }

  const adoptActiveEditor = (editor: vscode.TextEditor | undefined): void => {
    // Sticky on blur: a non-Silk focus (the panel itself, an output view) keeps the last document.
    if (editor === undefined || editor.document.languageId !== 'silk') return
    const uri = editor.document.uri.toString()
    if (uri === state.uri) return
    state.uri = uri
    state.evaluate = false
    void refresh()
  }

  context.subscriptions.push(
    vscode.commands.registerCommand('silk.openInspector', async () => {
      if (panel !== undefined) {
        panel.reveal(undefined, true)
        return
      }
      panel = vscode.window.createWebviewPanel(
        'silkInspector',
        'Silk Inspector',
        { viewColumn: vscode.ViewColumn.Beside, preserveFocus: true },
        { enableScripts: true, retainContextWhenHidden: true },
      )
      panel.onDidDispose(() => {
        panel = undefined
      })
      panel.webview.html = webviewHtml()
      panel.webview.onDidReceiveMessage((message: Record<string, unknown>) => {
        switch (message.type) {
          case 'pickView':
            state.viewId = message.viewId as string
            void refresh()
            return
          case 'setFilter':
            state.filter = message.filter as string
            void refresh()
            return
          case 'toggleTrivia':
            state.showTrivia = !state.showTrivia
            void refresh()
            return
          case 'evaluate':
            state.evaluate = true
            void refresh()
            return
          case 'activateRow':
            void activateRow(message.span as Span)
            return
        }
      })

      if (views.length === 0) {
        try {
          views = (
            await session.request<{ views: ReadonlyArray<ViewDescriptor> }>(viewsRequest)
          ).views.filter((view) => view.id !== 'source')
        } catch {
          // The picker stays empty until the server answers; the next refresh retries nothing —
          // reopening the panel does.
        }
      }
      adoptActiveEditor(vscode.window.activeTextEditor)
      void refresh()
    }),

    vscode.window.onDidChangeActiveTextEditor(adoptActiveEditor),
    vscode.window.onDidChangeTextEditorSelection((event) => postCursor(event.textEditor)),
  )

  // Any commit may change the view (an edited import changes the closure of every dependent), so
  // the panel refreshes on every invalidation rather than tracking project membership itself.
  context.subscriptions.push(
    session.onInvalidation(() => {
      state.evaluate = false
      void refresh()
    }),
  )
}

/**
 * The whole webview, inline: a strict CSP forbids external resources, and the row grammar plus a
 * native `<select>` phase picker do not justify a bundler. Colors come exclusively from
 * `--vscode-*` variables, so the panel follows light, dark, and high-contrast themes.
 */
const webviewHtml = (): string => {
  const nonce = Math.random().toString(36).slice(2)
  return /* html */ `<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="UTF-8">
<meta http-equiv="Content-Security-Policy" content="default-src 'none'; style-src 'unsafe-inline'; script-src 'nonce-${nonce}';">
<style>
  :root { color-scheme: light dark; }
  body {
    margin: 0; padding: 0;
    font-family: var(--vscode-editor-font-family, monospace);
    font-size: 12px;
    color: var(--vscode-foreground);
    background: var(--vscode-editor-background);
  }
  #bar {
    display: flex; align-items: center; gap: 6px;
    padding: 4px 8px;
    border-bottom: 1px solid var(--vscode-panel-border);
    position: sticky; top: 0;
    background: var(--vscode-editor-background);
  }
  #bar select, #bar input {
    background: var(--vscode-input-background);
    color: var(--vscode-input-foreground);
    border: 1px solid var(--vscode-input-border, transparent);
    font: inherit; padding: 1px 4px;
  }
  #bar button {
    background: var(--vscode-button-secondaryBackground, transparent);
    color: var(--vscode-button-secondaryForeground, inherit);
    border: 1px solid var(--vscode-input-border, transparent);
    font: inherit; padding: 1px 7px; cursor: pointer;
  }
  #bar button[aria-pressed="true"] { border-color: var(--vscode-focusBorder); }
  #meta { margin-left: auto; opacity: 0.7; white-space: nowrap; }
  #facts {
    display: flex; gap: 12px; padding: 2px 8px;
    border-bottom: 1px solid var(--vscode-panel-border);
    font-size: 11px; opacity: 0.85; flex-wrap: wrap;
  }
  #facts [data-tone="warning"] { color: var(--vscode-editorWarning-foreground); }
  #facts [data-tone="ok"] { color: var(--vscode-testing-iconPassed, var(--vscode-charts-green)); }
  #facts [data-tone="muted"] { opacity: 0.6; }
  #rows { padding: 2px 0; }
  .row {
    display: flex; align-items: baseline; gap: 6px;
    width: 100%; box-sizing: border-box;
    padding: 0 8px; min-height: 17px; line-height: 17px;
    border: 0; background: none; font: inherit; color: inherit; text-align: left;
  }
  button.row { cursor: pointer; }
  button.row:hover { background: var(--vscode-list-hoverBackground); }
  .row[data-cursor] { background: var(--vscode-editor-selectionHighlightBackground); }
  .row[data-cursor="exact"] { background: var(--vscode-editor-selectionBackground); }
  .row[data-head="true"] .label { font-weight: 700; }
  .row[data-tone="error"] .label { color: var(--vscode-editorError-foreground); }
  .row[data-tone="warning"] .label { color: var(--vscode-editorWarning-foreground); }
  .row[data-tone="ok"] .label { color: var(--vscode-testing-iconPassed, var(--vscode-charts-green)); }
  .row[data-tone="symbol"] .label { color: var(--vscode-charts-purple, var(--vscode-textLink-foreground)); }
  .lead { min-width: 26px; text-align: right; opacity: 0.5; font-variant-numeric: tabular-nums; }
  .label { white-space: pre; }
  .detail { opacity: 0.65; overflow: hidden; text-overflow: ellipsis; white-space: nowrap; }
  .span { margin-left: auto; opacity: 0.4; font-variant-numeric: tabular-nums; white-space: nowrap; }
  #empty { padding: 16px 12px; opacity: 0.75; }
</style>
</head>
<body>
<div id="bar">
  <select id="picker" aria-label="Inspector view"></select>
  <span id="controls"></span>
  <span id="meta"></span>
</div>
<div id="facts" hidden></div>
<div id="rows"></div>
<div id="empty" hidden></div>
<script nonce="${nonce}">
  const vscode = acquireVsCodeApi()
  let current = { viewId: 'pipeline', views: [], rows: [], cursor: undefined }

  const picker = document.getElementById('picker')
  const controls = document.getElementById('controls')
  const meta = document.getElementById('meta')
  const facts = document.getElementById('facts')
  const rowsHost = document.getElementById('rows')
  const empty = document.getElementById('empty')

  picker.addEventListener('change', () => {
    vscode.postMessage({ type: 'pickView', viewId: picker.value })
  })

  const escapeHtml = (value) =>
    String(value).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' }[c]))

  const cursorState = (span, cursor) => {
    if (span === undefined || cursor === undefined) return undefined
    if (span.module !== cursor.module) return undefined
    if (span.start === cursor.start && span.end === cursor.end) return 'exact'
    return span.start <= cursor.start && span.end >= cursor.end ? 'contains' : undefined
  }

  const renderPicker = (views, viewId) => {
    picker.innerHTML = views
      .map((view) => \`<option value="\${view.id}" \${view.id === viewId ? 'selected' : ''}>\${escapeHtml(view.tag)} · \${escapeHtml(view.title)}</option>\`)
      .join('')
  }

  const renderControls = (view, evaluate) => {
    let html = ''
    if (view !== undefined && view.hasFilter === true) {
      html += \`<input id="filter" placeholder="filter…" aria-label="Filter rows">\`
      html += \`<button id="trivia" aria-pressed="false">trivia</button>\`
    }
    if (view !== undefined && view.action !== undefined) {
      html += \`<button id="action">\${escapeHtml(view.action.label)}</button>\`
    }
    controls.innerHTML = html
    const filter = document.getElementById('filter')
    if (filter !== null) {
      let timer
      filter.addEventListener('input', () => {
        clearTimeout(timer)
        timer = setTimeout(() => vscode.postMessage({ type: 'setFilter', filter: filter.value }), 200)
      })
    }
    const trivia = document.getElementById('trivia')
    if (trivia !== null) trivia.addEventListener('click', () => vscode.postMessage({ type: 'toggleTrivia' }))
    const action = document.getElementById('action')
    if (action !== null) action.addEventListener('click', () => vscode.postMessage({ type: 'evaluate' }))
  }

  const renderRows = () => {
    rowsHost.innerHTML = current.rows
      .map((row) => {
        const pickable = row.span !== undefined
        const state = cursorState(row.span, current.cursor)
        const indent = (row.depth ?? 0) * 13
        return \`<\${pickable ? 'button' : 'div'} class="row" data-key="\${escapeHtml(row.key)}"
          \${state === undefined ? '' : \`data-cursor="\${state}"\`}
          \${row.tone === undefined ? '' : \`data-tone="\${row.tone}"\`}
          data-head="\${row.head === true}">
          \${row.lead === undefined ? '' : \`<span class="lead">\${escapeHtml(row.lead)}</span>\`}
          \${indent === 0 ? '' : \`<span style="width:\${indent}px;flex:none"></span>\`}
          <span class="label">\${escapeHtml(row.label)}</span>
          <span class="detail">\${escapeHtml(row.detail ?? '')}</span>
          <span class="span">\${row.span === undefined ? '' : \`\${row.span.start}–\${row.span.end}\`}</span>
        </\${pickable ? 'button' : 'div'}>\`
      })
      .join('')
  }

  rowsHost.addEventListener('click', (event) => {
    const target = event.target.closest('button.row')
    if (target === null) return
    const row = current.rows.find((candidate) => candidate.key === target.dataset.key)
    if (row !== undefined && row.span !== undefined) vscode.postMessage({ type: 'activateRow', span: row.span })
  })

  window.addEventListener('message', (event) => {
    const message = event.data
    if (message.type === 'cursor') {
      current.cursor = message.span
      renderRows()
      return
    }
    if (message.type !== 'view') return
    current.viewId = message.viewId
    current.views = message.views
    renderPicker(message.views, message.viewId)
    const view = message.views.find((candidate) => candidate.id === message.viewId)
    renderControls(view, message.evaluate === true)
    if (message.result === undefined) {
      current.rows = []
      renderRows()
      meta.textContent = ''
      facts.hidden = true
      empty.hidden = false
      empty.textContent = message.error ?? 'No Silk document is active.'
      return
    }
    meta.textContent = message.result.meta ?? ''
    if (message.result.facts !== undefined && message.result.facts.length > 0) {
      facts.hidden = false
      facts.innerHTML = message.result.facts
        .map((fact) => \`<span data-tone="\${fact.tone ?? 'default'}">\${escapeHtml(fact.text)}</span>\`)
        .join('')
    } else {
      facts.hidden = true
    }
    if (message.result.unavailable !== undefined) {
      current.rows = []
      renderRows()
      empty.hidden = false
      empty.textContent = message.result.unavailable
      return
    }
    empty.hidden = true
    current.rows = message.result.rows
    renderRows()
  })
</script>
</body>
</html>`
}
