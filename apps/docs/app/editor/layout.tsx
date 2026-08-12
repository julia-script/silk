import type { ReactNode } from 'react'
import '@xterm/xterm/css/xterm.css'
import { EditorProvider } from './EditorProvider'

export default function EditorLayout({ children }: { readonly children: ReactNode }) {
  return <EditorProvider>{children}</EditorProvider>
}
