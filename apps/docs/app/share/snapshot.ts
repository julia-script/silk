import * as Data from 'effect/Data'
import * as Effect from 'effect/Effect'

export class SnapshotError extends Data.TaggedError('SnapshotError')<{
  readonly message: string
}> {}

const xml = (value: string): string =>
  value
    .replaceAll('&', '&amp;')
    .replaceAll('<', '&lt;')
    .replaceAll('>', '&gt;')
    .replaceAll('"', '&quot;')
    .replaceAll("'", '&apos;')

const number = (value: number): string => String(Math.round(value * 100) / 100)

const relative = (bounds: DOMRect, root: DOMRect) => ({
  height: bounds.height,
  width: bounds.width,
  x: bounds.left - root.left,
  y: bounds.top - root.top,
})

const rect = (
  bounds: ReturnType<typeof relative>,
  options: {
    readonly fill: string
    readonly radius?: number
    readonly stroke?: string
    readonly strokeWidth?: number
  },
): string =>
  `<rect x="${number(bounds.x)}" y="${number(bounds.y)}" width="${number(bounds.width)}" height="${number(bounds.height)}" fill="${xml(options.fill)}"${options.stroke === undefined ? '' : ` stroke="${xml(options.stroke)}" stroke-width="${number(options.strokeWidth ?? 1)}"`}${options.radius === undefined ? '' : ` rx="${number(options.radius)}"`}/>`

const text = (value: string, element: Element, bounds: DOMRect, root: DOMRect): string => {
  const style = getComputedStyle(element)
  const box = relative(bounds, root)
  const baseline = box.y + box.height * 0.8
  return `<text x="${number(box.x)}" y="${number(baseline)}" fill="${xml(style.color)}" fill-opacity="${xml(style.opacity)}" font-family="${xml(style.fontFamily)}" font-size="${xml(style.fontSize)}" font-style="${xml(style.fontStyle)}" font-weight="${xml(style.fontWeight)}" letter-spacing="${xml(style.letterSpacing)}" xml:space="preserve">${xml(value)}</text>`
}

const elementText = (element: Element, root: DOMRect): string => {
  const value = element.textContent ?? ''
  return value === '' ? '' : text(value, element, element.getBoundingClientRect(), root)
}

const textNodes = (element: Element): ReadonlyArray<Text> => {
  const nodes: Array<Text> = []
  const walker = document.createTreeWalker(element, NodeFilter.SHOW_TEXT)
  while (walker.nextNode() !== null) {
    if (walker.currentNode instanceof Text && walker.currentNode.data !== '')
      nodes.push(walker.currentNode)
  }
  return nodes
}

const renderedText = (node: Text, root: DOMRect): string => {
  const parent = node.parentElement
  if (parent === null) return ''
  const range = document.createRange()
  range.selectNodeContents(node)
  const bounds = range.getBoundingClientRect()
  return bounds.width === 0 || bounds.height === 0 ? '' : text(node.data, parent, bounds, root)
}

const squiggle = (element: Element, root: DOMRect): string => {
  const bounds = relative(element.getBoundingClientRect(), root)
  const y = bounds.y + bounds.height + 1
  const end = bounds.x + bounds.width
  const points: Array<string> = []
  for (let x = bounds.x; x <= end; x += 2) {
    points.push(`${number(x)},${number(y + (Math.round((x - bounds.x) / 2) % 2 === 0 ? 0 : 2))}`)
  }
  const color = getComputedStyle(element).textDecorationColor
  return `<polyline points="${points.join(' ')}" fill="none" stroke="${xml(color)}" stroke-width="1"/>`
}

const wrap = (value: string, maximum: number): ReadonlyArray<string> => {
  const lines: Array<string> = []
  let line = ''
  for (const word of value.split(/\s+/)) {
    const candidate = line === '' ? word : `${line} ${word}`
    if (candidate.length <= maximum || line === '') line = candidate
    else {
      lines.push(line)
      line = word
    }
  }
  if (line !== '') lines.push(line)
  return lines
}

export interface Snapshot {
  readonly height: number
  readonly svg: string
  readonly width: number
}

const captureVisible = (surface: HTMLElement): Snapshot => {
  const root = surface.getBoundingClientRect()
  const width = Math.ceil(root.width)
  const height = Math.ceil(root.height)
  if (width === 0 || height === 0) throw new Error('The image preview is not visible yet.')

  const figure = surface.querySelector<HTMLElement>('[data-snapshot-figure]')
  const label = surface.querySelector<HTMLElement>('[data-snapshot-label]')
  const codeArea = surface.querySelector<HTMLElement>('[data-snapshot-code]')
  const snippet = surface.querySelector<HTMLElement>('silk-snippet')
  const content = snippet?.shadowRoot?.querySelector<HTMLElement>('.cm-content')
  if (
    figure === null ||
    label === null ||
    codeArea === null ||
    content === null ||
    content === undefined
  )
    throw new Error('The Silk editor is still loading.')

  const surfaceStyle = getComputedStyle(surface)
  const figureStyle = getComputedStyle(figure)
  const labelStyle = getComputedStyle(label)
  const codeStyle = getComputedStyle(codeArea)
  const figureBounds = relative(figure.getBoundingClientRect(), root)
  const labelBounds = relative(label.getBoundingClientRect(), root)
  const codeBounds = relative(codeArea.getBoundingClientRect(), root)
  const primitives: Array<string> = [
    rect({ x: 0, y: 0, width, height }, { fill: surfaceStyle.backgroundColor }),
    rect(figureBounds, {
      fill: figureStyle.backgroundColor,
      stroke: figureStyle.borderTopColor,
      strokeWidth: Number.parseFloat(figureStyle.borderTopWidth) || 1,
    }),
    rect(labelBounds, { fill: labelStyle.backgroundColor }),
    `<line x1="${number(labelBounds.x)}" x2="${number(labelBounds.x + labelBounds.width)}" y1="${number(labelBounds.y + labelBounds.height)}" y2="${number(labelBounds.y + labelBounds.height)}" stroke="${xml(labelStyle.borderBottomColor)}" stroke-width="${number(Number.parseFloat(labelStyle.borderBottomWidth) || 1)}"/>`,
    rect(codeBounds, { fill: codeStyle.backgroundColor }),
  ]

  for (const child of label.children) primitives.push(elementText(child, root))

  const clipId = 'silk-code-clip'
  const codeText = textNodes(content)
    .map((node) => renderedText(node, root))
    .join('')
  const diagnostics = Array.from(
    snippet?.shadowRoot?.querySelectorAll<HTMLElement>('.cm-lintRange-error') ?? [],
  )
    .map((element) => squiggle(element, root))
    .join('')
  primitives.push(
    `<defs><clipPath id="${clipId}"><rect x="${number(codeBounds.x)}" y="${number(codeBounds.y)}" width="${number(codeBounds.width)}" height="${number(codeBounds.height)}"/></clipPath></defs>`,
    `<g clip-path="url(#${clipId})">${codeText}${diagnostics}</g>`,
  )

  const diagnostic = surface.querySelector<HTMLElement>('[data-snapshot-diagnostic]')
  if (diagnostic !== null) {
    const style = getComputedStyle(diagnostic)
    const bounds = relative(diagnostic.getBoundingClientRect(), root)
    primitives.push(
      rect(bounds, {
        fill: style.backgroundColor,
        radius: Number.parseFloat(style.borderRadius) || 0,
        stroke: style.borderTopColor,
        strokeWidth: Number.parseFloat(style.borderTopWidth) || 1,
      }),
    )
    const message = diagnostic.querySelector('span')?.textContent ?? ''
    const fontSize = Number.parseFloat(style.fontSize) || 12
    const lineHeight = Number.parseFloat(style.lineHeight) || fontSize * 1.45
    const maximum = Math.max(18, Math.floor((bounds.width - 48) / (fontSize * 0.62)))
    const lines = wrap(message, maximum)
    const color = xml(style.color)
    const family = xml(style.fontFamily)
    const x = bounds.x + 36
    const firstY =
      bounds.y + Math.max(lineHeight, (bounds.height - lines.length * lineHeight) / 2 + fontSize)
    primitives.push(
      `<path d="M ${number(bounds.x + 14)} ${number(bounds.y + 22)} L ${number(bounds.x + 20)} ${number(bounds.y + 10)} L ${number(bounds.x + 26)} ${number(bounds.y + 22)} Z" fill="none" stroke="${xml(getComputedStyle(diagnostic.querySelector('svg') ?? diagnostic).color)}" stroke-width="1.5"/>`,
      ...lines.map(
        (line, index) =>
          `<text x="${number(x)}" y="${number(firstY + index * lineHeight)}" fill="${color}" font-family="${family}" font-size="${number(fontSize)}" font-weight="${xml(style.fontWeight)}">${xml(line)}</text>`,
      ),
    )
  }

  const svg = `<svg xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}" role="img" aria-label="Silk source code snapshot">${primitives.join('')}</svg>`
  return { height, svg, width }
}

const nextPaint = Effect.callback<void>((resume) => {
  requestAnimationFrame(() => requestAnimationFrame(() => resume(Effect.void)))
})

/**
 * Draws the editor as origin-clean SVG primitives at a stable share width, independent of the
 * device currently editing it.
 */
export const capture = Effect.fn('Snapshot.capture')(function* (
  surface: HTMLElement,
): Effect.fn.Return<Snapshot> {
  return yield* Effect.acquireUseRelease(
    Effect.sync(() => {
      const originalStyle = surface.getAttribute('style')
      surface.style.position = 'fixed'
      surface.style.insetBlockStart = '0'
      surface.style.insetInlineStart = '-10000px'
      surface.style.maxWidth = 'none'
      surface.style.visibility = 'hidden'
      return originalStyle
    }),
    () => nextPaint.pipe(Effect.andThen(Effect.sync(() => captureVisible(surface)))),
    (originalStyle) =>
      Effect.sync(() => {
        if (originalStyle === null) surface.removeAttribute('style')
        else surface.setAttribute('style', originalStyle)
      }),
  )
})

/** Rasterizes a pure-SVG snapshot at 2× for crisp clipboard output. */
export const png = Effect.fn('Snapshot.png')(function* (
  snapshot: Snapshot,
): Effect.fn.Return<Blob, SnapshotError> {
  const source = new Blob([snapshot.svg], { type: 'image/svg+xml;charset=utf-8' })
  return yield* Effect.acquireUseRelease(
    Effect.sync(() => URL.createObjectURL(source)),
    (url) =>
      Effect.gen(function* () {
        const image = yield* Effect.callback<HTMLImageElement, SnapshotError>((resume) => {
          const image = new Image()
          image.onload = () => resume(Effect.succeed(image))
          image.onerror = () =>
            resume(
              Effect.fail(
                new SnapshotError({ message: 'The browser could not rasterize this SVG.' }),
              ),
            )
          image.src = url
        })
        const scale = 2
        const canvas = document.createElement('canvas')
        canvas.width = snapshot.width * scale
        canvas.height = snapshot.height * scale
        const context = canvas.getContext('2d')
        if (context === null)
          return yield* new SnapshotError({
            message: 'Canvas rendering is unavailable in this browser.',
          })
        context.scale(scale, scale)
        context.drawImage(image, 0, 0, snapshot.width, snapshot.height)
        return yield* Effect.callback<Blob, SnapshotError>((resume) => {
          canvas.toBlob((blob) => {
            if (blob === null)
              resume(
                Effect.fail(new SnapshotError({ message: 'The browser could not create a PNG.' })),
              )
            else resume(Effect.succeed(blob))
          }, 'image/png')
        })
      }),
    (url) => Effect.sync(() => URL.revokeObjectURL(url)),
  )
})

export const download = (blob: Blob, filename: string): void => {
  const url = URL.createObjectURL(blob)
  const link = document.createElement('a')
  link.download = filename
  link.href = url
  link.click()
  URL.revokeObjectURL(url)
}
