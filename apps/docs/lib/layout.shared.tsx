import type { BaseLayoutProps } from 'fumadocs-ui/layouts/shared'

const xIcon = (
  <svg aria-hidden="true" fill="currentColor" viewBox="0 0 24 24">
    <path d="M18.901 1.153h3.68l-8.04 9.19L24 22.847h-7.406l-5.8-7.584-6.638 7.584H.474l8.6-9.83L0 1.154h7.594l5.243 6.932 6.064-6.933Zm-1.291 19.492h2.039L6.486 3.24H4.298L17.61 20.645Z" />
  </svg>
)

const blueskyIcon = (
  <svg aria-hidden="true" fill="currentColor" viewBox="0 0 24 24">
    <path d="M12 10.8C10.913 8.686 7.954 4.747 5.202 2.805 2.566.944 1.561 1.266.902 1.565.139 1.908 0 3.08 0 3.768c0 .69.378 5.65.624 6.479.815 2.736 3.713 3.66 6.383 3.364-4.621.684-8.733 2.362-3.348 8.36C9.58 28.1 11.771 20.66 12 19.24c.229 1.42 2.419 8.86 8.34 2.73 5.385-5.998 1.273-7.676-3.348-8.36 2.67.297 5.568-.628 6.383-3.364.246-.828.624-5.79.624-6.478 0-.69-.139-1.861-.902-2.206-.659-.298-1.664-.62-4.3 1.24C16.046 4.748 13.087 8.687 12 10.8Z" />
  </svg>
)

export function baseOptions(): BaseLayoutProps {
  return {
    nav: {
      title: 'Silk',
    },
    links: [
      {
        type: 'icon',
        label: 'Follow JuliaScript on X',
        text: 'X',
        url: 'https://x.com/JuliaScript',
        icon: xIcon,
      },
      {
        type: 'icon',
        label: 'Follow JuliaScript on Bluesky',
        text: 'Bluesky',
        url: 'https://bsky.app/profile/juliascript.bsky.social',
        icon: blueskyIcon,
      },
    ],
    githubUrl: 'https://github.com/julia-script/silk',
  }
}
