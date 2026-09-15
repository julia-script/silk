import * as Console from 'effect/Console'
import * as DateTime from 'effect/DateTime'
import * as Effect from 'effect/Effect'
import * as Exit from 'effect/Exit'
import * as FileSystem from 'effect/FileSystem'
import * as Option from 'effect/Option'
import * as Path from 'effect/Path'
import * as Schema from 'effect/Schema'
import * as Tracer from 'effect/Tracer'

interface Event {
  readonly name: string
  readonly ph: 'M' | 'X'
  readonly pid: number
  readonly tid: number
  readonly cat?: string
  readonly ts?: number
  readonly dur?: number
  readonly args: Readonly<Record<string, unknown>>
}

/** Convert nanoseconds to relative microseconds without losing epoch-scale precision. */
const encode = (spans: ReadonlyArray<Tracer.Span>) => {
  const completed = spans
    .flatMap((span) => (span.status._tag === 'Ended' ? [{ span, status: span.status }] : []))
    .sort((a, b) => {
      if (a.status.startTime !== b.status.startTime)
        return a.status.startTime < b.status.startTime ? -1 : 1
      if (a.status.endTime !== b.status.endTime) return a.status.endTime > b.status.endTime ? -1 : 1
      return 0
    })
  const origin = completed[0]?.status.startTime ?? 0n
  const byId = new Map(spans.map((span) => [span.spanId, span]))
  const assignments = new Map<string, number>()
  const lanes: Array<Array<{ readonly id: string; readonly end: bigint }>> = []
  const events: Array<Event> = [
    {
      name: 'process_name',
      ph: 'M',
      pid: 1,
      tid: 0,
      args: { name: 'Silk compiler — Effect spans' },
    },
  ]

  for (const { span, status } of completed) {
    const parent = Option.getOrUndefined(span.parent)?.spanId
    const ancestors = new Set<string>()
    let ancestor = parent
    while (ancestor !== undefined && !ancestors.has(ancestor)) {
      ancestors.add(ancestor)
      const value = byId.get(ancestor)
      ancestor = value === undefined ? undefined : Option.getOrUndefined(value.parent)?.spanId
    }
    // Nested spans share a lane; concurrent sibling spans get independent lanes.
    const preferred = parent === undefined ? undefined : assignments.get(parent)
    const candidates = [...(preferred === undefined ? [] : [preferred]), ...lanes.keys()]
    let lane: number | undefined
    for (const candidate of candidates) {
      const stack = lanes[candidate]
      if (stack === undefined) continue
      let top = stack.at(-1)
      while (top !== undefined && top.end <= status.startTime) {
        stack.pop()
        top = stack.at(-1)
      }
      if (top === undefined || (ancestors.has(top.id) && status.endTime <= top.end)) {
        lane = candidate
        stack.push({ id: span.spanId, end: status.endTime })
        break
      }
    }
    if (lane === undefined) {
      lane = lanes.length
      lanes.push([{ id: span.spanId, end: status.endTime }])
      events.push({
        name: 'thread_name',
        ph: 'M',
        pid: 1,
        tid: lane + 1,
        args: { name: `Effect spans ${lane + 1}` },
      })
    }
    assignments.set(span.spanId, lane)
    events.push({
      name: span.name,
      cat: 'effect',
      ph: 'X',
      pid: 1,
      tid: lane + 1,
      ts: Number(status.startTime - origin) / 1000,
      dur: Number(status.endTime - status.startTime) / 1000,
      args: {
        ...Object.fromEntries(span.attributes),
        traceId: span.traceId,
        spanId: span.spanId,
        parentSpanId: parent ?? '',
        status: Exit.isFailure(status.exit) ? 'failure' : 'success',
      },
    })
  }
  return { traceEvents: events, displayTimeUnit: 'ms' }
}

/** Mirror the installed tracer and preserve a dated export after the root span closes. */
export const record = Effect.fnUntraced(function* <A, E, R>(
  self: Effect.Effect<A, E, R>,
  destination: string,
) {
  const fs = yield* FileSystem.FileSystem
  const path = yield* Path.Path
  const startedAt = DateTime.formatIso(yield* DateTime.now)
  const parsed = path.parse(destination)
  const output = path.join(
    parsed.dir,
    `${parsed.name}.${startedAt.replaceAll(':', '-')}${parsed.ext}`,
  )
  const upstream = yield* Tracer.Tracer
  const spans: Array<Tracer.Span> = []
  const tracer = Tracer.make({
    context: upstream.context?.bind(upstream),
    span(options) {
      const span = upstream.span(options)
      spans.push(span)
      return span
    },
  })
  const save = Effect.gen(function* () {
    const json = yield* Schema.encodeEffect(Schema.fromJsonString(Schema.Unknown))({
      ...encode(spans),
      otherData: { startedAt },
    })
    yield* fs.makeDirectory(path.dirname(output), { recursive: true })
    yield* fs.writeFileString(output, `${json}\n`, { flag: 'wx' })
    yield* Console.log(`Chrome trace: ${output}`)
  }).pipe(Effect.withTracerEnabled(false))
  return yield* self.pipe(
    Effect.withTracer(tracer),
    Effect.onExit(() => save),
  )
})
