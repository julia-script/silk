import { source } from '@/lib/source'
import { DocsLayout } from 'fumadocs-ui/layouts/docs'
import { baseOptions } from '@/lib/layout.shared'
import type { ReactNode } from 'react'
import { AISearch, AISearchPanel, AISearchTrigger } from '@/components/ai/search'
import { buttonVariants } from '@/components/ui/button'
import { cn } from '@/lib/cn'
import { MessageCircleIcon } from 'lucide-react'

export default function Layout({ children }: { children: ReactNode }) {
  return (
    <DocsLayout tree={source.getPageTree()} {...baseOptions()}>
      <AISearch>
        <AISearchPanel />
        <AISearchTrigger
          position="float"
          className={cn(
            buttonVariants({
              color: 'secondary',
              className: 'rounded-2xl text-fd-muted-foreground',
            }),
          )}
        >
          <MessageCircleIcon className="size-4.5" />
          Ask AI
        </AISearchTrigger>
      </AISearch>
      {children}
    </DocsLayout>
  )
}
