'use client'

import { useEffect } from 'react'

/** Defines `<silk-snippet>` in the browser; the module touches customElements, so never on the server. */
export function RegisterSnippets() {
  useEffect(() => {
    void import('@silklang/editor-support/register')
  }, [])
  return null
}
