import { baseOptions } from '@/lib/layout.shared'
import 'dockview/dist/styles/dockview.css'
import { HomeLayout } from 'fumadocs-ui/layouts/home'
import type { ReactNode } from 'react'

/**
 * The workbench is a tool, not a document: it wants the full width and has no place in the
 * package-docs page tree. Nesting a layout here replaces the parent `DocsLayout` for this
 * subtree, which is what drops the docs sidebar while keeping the site nav and theme toggle.
 */
export default function Layout({ children }: { children: ReactNode }) {
  return <HomeLayout {...baseOptions()}>{children}</HomeLayout>
}
