import localFont from 'next/font/local'

// JetBrains Mono from google/fonts at 23e54b51ddffbc7713c583748e3bd86f62b1fa4a.
// Keep the font files local so the docs build does not depend on Google Fonts resolution.
export const introMono = localFont({
  src: [
    { path: './fonts/JetBrainsMono.ttf', weight: '400 600', style: 'normal' },
    { path: './fonts/JetBrainsMono-Italic.ttf', weight: '400 600', style: 'italic' },
  ],
  variable: '--intro-mono',
  display: 'swap',
})

export const shareMono = localFont({
  src: './fonts/JetBrainsMono.ttf',
  weight: '400 600',
  style: 'normal',
  variable: '--share-mono',
  display: 'swap',
})
