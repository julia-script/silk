'use client'

import { RegistryProvider } from '@effect/atom-react'
import type { ReactNode } from 'react'

export interface EditorProviderProps {
  readonly children: ReactNode
}

export function EditorProvider({ children }: EditorProviderProps) {
  return <RegistryProvider>{children}</RegistryProvider>
}
