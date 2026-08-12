import { assert, it } from '@effect/vitest'
import { renderToStaticMarkup } from 'react-dom/server'
import { EditorProvider } from '../app/editor/EditorProvider'
import EditorPage from '../app/editor/page'

it('server-renders the editor route in its browser-safe boot state', () => {
  const markup = renderToStaticMarkup(
    <EditorProvider>
      <EditorPage />
    </EditorProvider>,
  )

  assert.include(markup, 'Silk workspace')
  assert.include(markup, 'Booting the browser workspace')
  assert.include(markup, 'data-testid="terminal-host"')
  assert.notInclude(markup, 'Terminal unavailable')
})
