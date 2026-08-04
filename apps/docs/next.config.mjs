import { createMDX } from 'fumadocs-mdx/next';

const withMDX = createMDX();

/** @type {import('next').NextConfig} */
const config = {
  reactStrictMode: true,
  // ponytail: content lives in packages/*/docs, outside this app's root.
  outputFileTracingRoot: new URL('../../', import.meta.url).pathname,
};

export default withMDX(config);
