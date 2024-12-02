"use client";
import langWasm from "@lang/lib/zig-out/bin/lang-wasi.wasm";
import {
	type DirectoryNode,
	type FileNode,
	type FileSystemTree,
	WebContainer,
} from "@webcontainer/api";
import { memoize } from "lodash-es";
import { createContext, use, useContext, useEffect, useState } from "react";
const createContainer = memoize(async () => {
	if (typeof window === "undefined") {
		return new WebContainer();
	}
	const term = await WebContainer.boot(files);
	await term.mount(files);
	return term;
});
const source = `
export fn fib(n: i32): i32 {
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
	src: dir({
		"main.slk": file(source),
		"main.wasm": file(langWasm),
		abc: dir({
			"def.ts": file("Hello, world!"),
			"ghi.ts": file("Hello, Universe"),
			jkl: dir({}),
			mnopq: dir({}),
		}),
	}),
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
