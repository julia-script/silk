import { createMDX } from 'fumadocs-mdx/next';

const withMDX = createMDX();

const editorIsolationHeaders = [
  { key: 'Cross-Origin-Embedder-Policy', value: 'require-corp' },
  { key: 'Cross-Origin-Opener-Policy', value: 'same-origin' },
];

/** @type {import('next').NextConfig} */
const config = {
  reactStrictMode: true,
  experimental: {
    useTypeScriptCli: true,
  },
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
      {
        source: '/docs/:path*',
        headers: [{ key: 'Vary', value: 'Accept' }],
      },
    ];
  },
  async rewrites() {
    return [
      {
        source: '/docs/:path*.md',
        destination: '/llms.mdx/docs/:path*',
      },
    ];
  },
};

export default withMDX(config);
