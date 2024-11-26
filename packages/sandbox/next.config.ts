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
};

export default nextConfig;
