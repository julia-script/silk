import { assert, it } from '@effect/vitest'
import { afterEach, vi } from 'vitest'

const calls = vi.hoisted<{ count: number; result: 'answer' | 'error' }>(() => ({
  count: 0,
  result: 'answer',
}))

vi.mock('@/lib/source', () => ({
  source: {
    getPages: () => [],
  },
}))

vi.mock('@openrouter/ai-sdk-provider', () => ({
  createOpenRouter: () => ({
    chat: () => ({
      specificationVersion: 'v3',
      provider: 'test',
      modelId: 'test',
      supportedUrls: {},
      doGenerate: () => {
        throw new Error('This route should stream responses')
      },
      doStream: () => {
        calls.count += 1
        return {
          stream: new ReadableStream({
            start(controller) {
              if (calls.result === 'error') {
                controller.enqueue({
                  type: 'error',
                  error: new Error('secret provider details'),
                })
                controller.close()
                return
              }

              controller.enqueue({ type: 'stream-start', warnings: [] })
              controller.enqueue({ type: 'text-start', id: 'answer' })
              controller.enqueue({ type: 'text-delta', id: 'answer', delta: 'Silk answer' })
              controller.enqueue({ type: 'text-end', id: 'answer' })
              controller.enqueue({
                type: 'finish',
                finishReason: { unified: 'stop', raw: 'stop' },
                usage: {
                  inputTokens: {
                    total: 1,
                    noCache: 1,
                    cacheRead: 0,
                    cacheWrite: 0,
                  },
                  outputTokens: {
                    total: 1,
                    text: 1,
                    reasoning: 0,
                  },
                },
              })
              controller.close()
            },
          }),
        }
      },
    }),
  }),
}))

import { POST } from './route'

const previousApiKey = process.env.OPENROUTER_API_KEY

afterEach(() => {
  calls.count = 0
  calls.result = 'answer'
  if (previousApiKey === undefined) {
    delete process.env.OPENROUTER_API_KEY
  } else {
    process.env.OPENROUTER_API_KEY = previousApiKey
  }
})

const request = () =>
  new Request('http://localhost/api/chat', {
    method: 'POST',
    headers: { 'content-type': 'application/json' },
    body: JSON.stringify({
      messages: [
        {
          id: 'question',
          role: 'user',
          parts: [{ type: 'text', text: 'What is Silk?' }],
        },
      ],
    }),
  })

it('passes assistant instructions separately from the chat messages', async () => {
  process.env.OPENROUTER_API_KEY = 'test-key'

  const response = await POST(request())
  const body = await response.text()

  assert.strictEqual(calls.count, 1)
  assert.include(body, 'Silk answer')
  assert.notInclude(body, 'An error occurred.')
})

it('returns a safe, actionable message when the model stream fails', async () => {
  process.env.OPENROUTER_API_KEY = 'test-key'
  calls.result = 'error'

  const response = await POST(request())
  const body = await response.text()

  assert.include(body, 'Ask AI could not complete the request. Please try again.')
  assert.notInclude(body, 'secret provider details')
})
