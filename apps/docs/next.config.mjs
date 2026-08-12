import { createMDX } from 'fumadocs-mdx/next';

const withMDX = createMDX();

const editorIsolationHeaders = [
  { key: 'Cross-Origin-Embedder-Policy', value: 'require-corp' },
  { key: 'Cross-Origin-Opener-Policy', value: 'same-origin' },
];

/** @type {import('next').NextConfig} */
const config = {
  reactStrictMode: true,
  // ponytail: content lives in packages/*/docs, outside this app's root.
  outputFileTracingRoot: new URL('../../', import.meta.url).pathname,
  async headers() {
    return [
      {
        source: '/editor',
        headers: editorIsolationHeaders,
      },
      {
        source: '/editor/:path*',
        headers: editorIsolationHeaders,
      },
    ];
  },
};

export default withMDX(config);
