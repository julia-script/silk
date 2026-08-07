import type { Metadata } from 'next'
import { InterpLab } from './interp-lab'

export const metadata: Metadata = {
  title: 'Interpreter lab',
  description:
    'The @silk-effect/wasm direct interpreter driven as a debugger: step through a WebAssembly module instruction by instruction with breakpoints, call stack, locals, value stack, globals, and memory.',
  robots: {
    index: false,
    follow: false,
  },
}

export default function InterpLabPage() {
  return <InterpLab />
}
