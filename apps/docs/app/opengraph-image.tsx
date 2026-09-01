/**
 * OG image for silklang.org — drop into apps/docs/app/opengraph-image.tsx
 * Next.js serves it at /opengraph-image and injects the og:image meta tag.
 * Rendered with next/og (Satori): flexbox only, inline styles.
 */
import { ImageResponse } from 'next/og'

export const runtime = 'edge'
export const alt = 'Silk — What If Effect Were a Low-Level Language?'
export const size = { width: 1200, height: 630 }
export const contentType = 'image/png'

const font = (file: string) =>
  fetch(`https://cdn.jsdelivr.net/gh/JetBrains/JetBrainsMono@2.304/fonts/ttf/${file}`).then((r) =>
    r.arrayBuffer(),
  )

// Palette from introduction.css
const c = {
  bgDeep: '#0e0d0c',
  bgBar: '#121110',
  bgPane: '#161513',
  hairline: '#201e1b',
  hairlineStrong: '#262320',
  ink: '#ece7df',
  ink2: '#cfc8bf',
  ink4: '#8d857c',
  gutter: '#726b63',
  amber: '#c6a678',
  errorInk: '#e89468',
  violet: '#b7a6d6',
  greenInk: '#8fc7a8',
}

const codeLines: Array<Array<[string, string]>> = [
  [
    ['effect fn', c.amber],
    [' program() ', c.ink],
    ['->', c.ink2],
    [' () ', c.ink],
    ['! LogError', c.errorInk],
    [' ', c.ink],
    ['? &mut Logger', c.amber],
    [' {', c.ink],
  ],
  [
    ['  ', c.ink],
    ['run', c.amber],
    [' Effect.log(', c.ink],
    ['"Hello from Silk!"', c.greenInk],
    [')', c.ink],
  ],
  [['}', c.ink]],
]

export default async function Image() {
  const [regular, medium] = await Promise.all([
    font('JetBrainsMono-Regular.ttf'),
    font('JetBrainsMono-Medium.ttf'),
  ])
  return new ImageResponse(
    <div
      style={{
        width: '100%',
        height: '100%',
        display: 'flex',
        flexDirection: 'column',
        background: c.bgDeep,
        fontFamily: 'JetBrains Mono',
      }}
    >
      {/* top bar, like the workbench chrome */}
      <div
        style={{
          height: 54,
          display: 'flex',
          alignItems: 'center',
          padding: '0 32px',
          background: c.bgBar,
          borderBottom: `1px solid ${c.hairlineStrong}`,
          fontSize: 17,
          letterSpacing: '0.14em',
          textTransform: 'uppercase',
          color: c.ink4,
        }}
      >
        <span style={{ color: c.amber }}>silk</span>
        <span style={{ flex: 1 }} />
        <span style={{ color: c.gutter }}>silklang.org</span>
      </div>
      <div
        style={{
          flex: 1,
          display: 'flex',
          flexDirection: 'column',
          justifyContent: 'center',
          gap: 36,
          padding: '0 96px',
        }}
      >
        <div style={{ display: 'flex', flexDirection: 'column', gap: 22 }}>
          <div
            style={{
              fontSize: 58,
              letterSpacing: '0.03em',
              color: c.amber,
              fontWeight: 500,
              lineHeight: 1,
            }}
          >
            § SILK
          </div>
          <div
            style={{
              fontSize: 31,
              lineHeight: 1.25,
              letterSpacing: '0.03em',
              color: c.ink,
              fontWeight: 500,
            }}
          >
            What If Effect Were a Low‑Level Language?
          </div>
          <div
            style={{
              height: 1,
              background: `linear-gradient(90deg, ${c.amber}, ${c.violet} 40%, rgba(0,0,0,0) 75%)`,
            }}
          />
        </div>
        {/* code pane */}
        <div
          style={{
            display: 'flex',
            flexDirection: 'column',
            background: c.bgPane,
            border: `1px solid ${c.hairlineStrong}`,
          }}
        >
          <div
            style={{
              height: 44,
              display: 'flex',
              alignItems: 'center',
              padding: '0 22px',
              background: c.bgBar,
              borderBottom: `1px solid ${c.hairline}`,
              fontSize: 15,
              letterSpacing: '0.14em',
              textTransform: 'uppercase',
            }}
          >
            <span style={{ color: c.amber }}>```SILK</span>
            <span style={{ flex: 1 }} />
            <span style={{ color: c.gutter }}>main.silk</span>
          </div>
          <div style={{ display: 'flex', flexDirection: 'column', padding: '26px 28px' }}>
            {codeLines.map((line, i) => (
              <div
                key={i}
                style={{ display: 'flex', whiteSpace: 'pre', fontSize: 29, lineHeight: 1.8 }}
              >
                {line.map(([text, color], j) => (
                  <span key={j} style={{ color, whiteSpace: 'pre' }}>
                    {text}
                  </span>
                ))}
              </div>
            ))}
          </div>
        </div>
      </div>
    </div>,
    {
      ...size,
      fonts: [
        { name: 'JetBrains Mono', data: regular, weight: 400, style: 'normal' },
        { name: 'JetBrains Mono', data: medium, weight: 500, style: 'normal' },
      ],
    },
  )
}
