import { isMarkdownPreferred, rewritePath } from 'fumadocs-core/negotiation'
import type { NextRequest } from 'next/server'
import { NextResponse } from 'next/server'

const llmPath = rewritePath('/docs{/*path}', '/llms.mdx/docs{/*path}')

export function proxy(request: NextRequest): NextResponse {
  if (isMarkdownPreferred(request)) {
    const destination = llmPath.rewrite(request.nextUrl.pathname)
    if (destination) {
      return NextResponse.rewrite(new URL(destination, request.nextUrl), {
        headers: { Vary: 'Accept' },
      })
    }
  }

  return NextResponse.next()
}

export const config = {
  matcher: '/docs/:path*',
}
