import type { NextConfig } from "next";
import path from "path";

const __dirname = new URL(".", import.meta.url).pathname;
console.log(__dirname);
const nextConfig: NextConfig = {
	experimental: {
		useWasmBinary: true,
	},
	webpack: (config, options) => {
		// config.experiments = { asyncWebAssembly: true };
		config.module.rules.push({
			test: /\.wasm$/,
			// use: ["base64-inline-loader"],
			use: [path.resolve(__dirname, "buffer-loader.js")],
		});
		return config;
	},
	async headers() {
		return [
			{
				source: "/(.*)",
				headers: [
					// 					Cross-Origin-Embedder-Policy: require-corp
					// Cross-Origin-Opener-Policy: same-origin
					{
						key: "Cross-Origin-Embedder-Policy",
						value: "require-corp",
					},
					{
						key: "Cross-Origin-Opener-Policy",
						value: "same-origin",
					},
				],
			},
		];
	},
};

export default nextConfig;
