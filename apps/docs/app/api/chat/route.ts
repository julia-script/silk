import { createOpenRouter } from '@openrouter/ai-sdk-provider'
import {
  convertToModelMessages,
  createUIMessageStreamResponse,
  stepCountIs,
  streamText,
  tool,
  toUIMessageStream,
} from 'ai'
import { z } from 'zod'
import * as Config from 'effect/Config'
import * as Effect from 'effect/Effect'
import * as Schema from 'effect/Schema'
import { source } from '@/lib/source'
import { Document, type DocumentData } from 'flexsearch'
import type { ChatUIMessage, SearchTool } from '../../../components/ai/search'

interface CustomDocument extends DocumentData {
  url: string
  title: string
  description: string
  content: string
}
const searchServer = createSearchServer()
const encodeJson = Schema.encodeSync(Schema.fromJsonString(Schema.Unknown))

async function createSearchServer() {
  const search = new Document<CustomDocument>({
    document: {
      id: 'url',
      index: ['title', 'description', 'content'],
      store: true,
    },
  })

  const docs = await chunkedAll(
    source.getPages().map(async (page) => {
      if (!('getText' in page.data)) return null

      return {
        title: page.data.title,
        description: page.data.description,
        url: page.url,
        content: await page.data.getText('processed'),
      } as CustomDocument
    }),
  )

  for (const doc of docs) {
    if (doc) search.add(doc)
  }

  return search
}

async function chunkedAll<O>(promises: Promise<O>[]): Promise<O[]> {
  const SIZE = 50
  const out: O[] = []
  for (let i = 0; i < promises.length; i += SIZE) {
    out.push(...(await Promise.all(promises.slice(i, i + SIZE))))
  }
  return out
}

/** System prompt, you can update it to provide more specific information */
const systemPrompt = [
  'You are an AI assistant for the Silk programming language documentation.',
  'Use the `search` tool to retrieve relevant docs context before answering when needed.',
  'The `search` tool returns raw JSON results from documentation. Use those results to ground your answer and cite sources as markdown links using the document `url` field when available.',
  'If you cannot find the answer in search results, say you do not know and suggest a better search query.',
].join('\n')

export interface Options {
  readonly apiKey: string
  readonly model: string
}

export const makePost = (options: Options) => async (req: Request) => {
  const { apiKey } = options
  if (!apiKey) {
    return Response.json({ error: 'Ask AI is not configured.' }, { status: 503 })
  }

  const reqJson = await req.json()
  const openrouter = createOpenRouter({ apiKey })

  const result = streamText({
    model: openrouter.chat(options.model),
    instructions: systemPrompt,
    stopWhen: stepCountIs(5),
    tools: {
      search: searchTool,
    },
    messages: await convertToModelMessages<ChatUIMessage>(reqJson.messages ?? [], {
      convertDataPart(part) {
        if (part.type === 'data-client')
          return {
            type: 'text',
            text: `[Client Context: ${encodeJson(part.data)}]`,
          }
      },
    }),
    toolChoice: 'auto',
  })

  return createUIMessageStreamResponse({
    stream: toUIMessageStream({
      stream: result.stream,
      onError() {
        return 'Ask AI could not complete the request. Please try again.'
      },
    }),
  })
}

export const POST = (request: Request): Promise<Response> =>
  makePost({
    apiKey: Effect.runSync(Config.string('OPENROUTER_API_KEY').pipe(Config.withDefault(''))),
    model: Effect.runSync(
      Config.string('OPENROUTER_MODEL').pipe(Config.withDefault('anthropic/claude-sonnet-5')),
    ),
  })(request)

const searchTool = tool({
  description: 'Search the docs content and return raw JSON results.',
  inputSchema: z.object({
    query: z.string(),
    limit: z.number().int().min(1).max(100).default(10),
  }),
  async execute({ query, limit }) {
    const search = await searchServer
    return await search.searchAsync(query, { limit, merge: true, enrich: true })
  },
}) satisfies SearchTool
