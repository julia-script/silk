import 'dockview/dist/styles/dockview.css'
import './workbench-theme.css'
import type { ReactNode } from 'react'

/**
 * The workbench gets the whole viewport.
 *
 * `HomeLayout` used to wrap this route to keep the site nav and theme toggle, but 56px of doc
 * chrome above a tool that is measured in 17px rows is the single largest thing standing between
 * the user and another pane. A tool is not a document page: navigation belongs to the app bar
 * this layout makes room for, not to the docs shell.
 */
export default function Layout({ children }: { children: ReactNode }) {
  return children
}
