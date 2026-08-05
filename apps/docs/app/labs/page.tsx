import type { Metadata } from 'next'
import { Workbench } from './workbench'

export const metadata: Metadata = {
  title: 'Compiler workbench',
  description:
    'The internal compiler lab: one program, every pipeline phase as a toggleable panel — syntax, module closure, declaration index, HIR, ownership, instance discovery, MIR, both backends, and the planned native toolchain.',
  robots: {
    index: false,
    follow: false,
  },
}

export default function LabsPage() {
  return <Workbench />
}
