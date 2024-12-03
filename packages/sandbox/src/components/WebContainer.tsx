"use client";
import langWasm from "@lang/lib/dist/bin/lang-wasi.wasm";
import {
	type DirectoryNode,
	type FileNode,
	type FileSystemTree,
	WebContainer,
} from "@webcontainer/api";
import { memoize } from "lodash-es";
import { createContext, use } from "react";

const createContainer = memoize(async () => {
	if (typeof window === "undefined") {
		return new WebContainer();
	}
	const term = await WebContainer.boot({
		workdirName: "silk",
	});
	await term.mount(files, {
		// mountPoint: "",
	});
	console.log(term.path);
	// await term.spawn("source", ["~/.jshrc"], {});
	// await term.spawn("chmod +x ~/silk/bin/silk", {});
	// mv /home/silk/.jshrc ~/
	// const res = await term.spawn("mv", ["/home/silk/.jshrc", "~/"], {
	// 	output: true,
	// });
	// console.log(await res.output.getReader().read());
	return term;
});

const executable = `#!/usr/bin/env node
const { spawnSync } = require("node:child_process");
spawnSync("wasm", ["~/silk/bin/silk.wasm", "--", ...process.argv.slice(2)], {
	stdio: "inherit",
});
`;
const source = `export fn fib(n: i32): i32 {
  var a:i32 = 0;
  var b:i32 = 1;
  if (n > 0) {
    while (n > 0) {
      n = n - 1;
      var t:i32 = a + b;
      a = b;
      b = t;
    }
    return b
  }
  return a
}
`;

const bash = `#!/bin/bash
wasm ./main.wasm -- $@
`;
const dir = (dir: FileSystemTree): DirectoryNode => ({
	directory: dir,
});
const file = (contents: string | Uint8Array): FileNode => ({
	file: {
		contents,
	},
});
// const files: FileSystemTree = {
// 	src: {
// 		directory: {
// 			"main.slk": {
// 				file: {
// 					contents: source,
// 				},
// 			},
// 			"main.wasm": {
// 				file: {
// 					contents: langWasm,
// 				},
// 			},
// 		},
// 	},
// };
const files: FileSystemTree = {
	bin: dir({
		// silk: file(executable),
		silk: file(bash),
		"silk.wasm": file(langWasm),
	}),
	// home: dir({
	// }),
	".jshrc": file(`alias silk="wasm ~/silk/bin/silk.wasm --mapdir ./:./ --"`),
	// src: dir({
	// "package.json": file(
	// 	JSON.stringify(
	// 		{
	// 			name: "sandbox",
	// 			version: "0.0.1",
	// 			dependencies: {},
	// 			bin: {
	// 				zilk: "wasm ./silk --",
	// 			},
	// 		},
	// 		null,
	// 		2,
	// 	),
	// ),
	project: dir({
		src: dir({
			"main.slk": file(source),
		}),
	}),
	// abc: dir({
	// 	"def.ts": file("Hello, world!"),
	// 	"ghi.ts": file("Hello, Universe"),
	// 	jkl: dir({}),
	// 	mnopq: dir({}),
	// }),
	// }),
};

type WebContainerContextProps = {
	container: WebContainer;
};
const WebContainerContext = createContext<WebContainerContextProps | null>(
	null,
);
export const WebContainerProvider = ({
	children,
}: { children: React.ReactNode }) => {
	const container = use(createContainer());
	return (
		<WebContainerContext.Provider value={{ container }}>
			{children}
		</WebContainerContext.Provider>
	);
};

export const useWebContainer = () => {
	const context = use(WebContainerContext);
	if (!context) {
		throw new Error(
			"useWebContainer must be used within a WebContainerProvider",
		);
	}
	return context;
};
