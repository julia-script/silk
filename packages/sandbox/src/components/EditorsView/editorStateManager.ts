import type { WebContainer } from "@webcontainer/api";
// import { vscodeDark } from "@uiw/codemirror-theme-vscode";
import { oneDark } from "@codemirror/theme-one-dark";
import { EditorState, type Extension } from "@codemirror/state";
import { langLanguage } from "@lang/grammar/dist/language";
import {
	Decoration,
	EditorView,
	keymap,
	type ViewUpdate,
} from "@codemirror/view";
import { basicSetup } from "codemirror";
import { useSyncExternalStore } from "react";
import { useWebContainer } from "../WebContainer";
import { decode } from "@webassemblyjs/wasm-parser";
import { print } from "@webassemblyjs/wast-printer";
import type { LRLanguage } from "@codemirror/language";
import { wastLanguage } from "@codemirror/lang-wast";
import { StateField, StateEffect } from "@codemirror/state";
import { markField } from "../code-mirror/marks";

const wasmToWat = (bytes: Uint8Array) => {
	const module = decode(bytes, { isDebug: false });
	return print(module) as string;
};

const createState = ({
	content,
	onSave,
	onViewUpdate,
	readonly,
	language,
}: {
	content: string;
	onSave?: (editorView: EditorView) => void;
	onViewUpdate?: (update: ViewUpdate) => void;
	readonly?: boolean;
	language?: LRLanguage;
}) => {
	const extensions: Extension[] = [basicSetup, markField];
	if (language) {
		extensions.push(language);
	}
	extensions.push(oneDark);
	if (readonly) {
		extensions.push(EditorState.readOnly.of(true));
	}

	if (onSave) {
		extensions.push(
			keymap.of([
				{
					key: "Cmd-s",
					stopPropagation: true,
					run: (e) => {
						onSave(e);
						return true;
					},
				},
				{
					key: "Ctrl-s",
					stopPropagation: true,
					run: (e) => {
						onSave(e);
						return true;
					},
				},
			]),
		);
	}

	if (onViewUpdate) {
		extensions.push(
			EditorView.updateListener.of((update) => {
				onViewUpdate(update);
			}),
		);
	}
	return EditorState.create({
		doc: content,
		extensions,
	});
};

type State = {
	saved: boolean;
	fileExists: boolean;
	ready: boolean;
} & (
	| {
			state: Uint8Array;
			type: "bytes";
			textRepresentation?: EditorState;
			ready: boolean;
	  }
	| {
			state: EditorState;
			type: "editor";
			ready: boolean;
	  }
	| {
			type: "unknown";
			ready: boolean;
			state: null;
	  }
);
export class DocumentState {
	state: State = {
		type: "unknown",
		state: null,
		saved: false,
		ready: false,
		fileExists: false,
	};
	subscribers: Set<(event: "update" | "save") => void> = new Set();
	constructor(
		public container: WebContainer,
		public path: string,
	) {
		this.emit("update");

		let promise: Promise<void>;
		const isWasm = this.path.endsWith(".wasm");
		if (isWasm) {
			promise = this.container.fs.readFile(this.path).then((bytes) => {
				this.updateState({
					state: bytes,
					type: "bytes",
					ready: true,
					saved: true,
					textRepresentation: createState({
						content: wasmToWat(bytes),
						readonly: true,
						language: wastLanguage,
					}),
					fileExists: true,
				});
			});
		} else {
			promise = this.container.fs
				.readFile(this.path, "utf-8")
				.then((content) => {
					this.updateState({
						saved: true,
						fileExists: true,
						type: "editor",
						ready: true,
						state: createState({
							content,
							language: langLanguage,
							onSave: async (e) => {
								await this.container.fs.writeFile(
									this.path,
									e.state.doc.toString(),
								);
								this.updateState({
									...this.state,
									saved: true,
								});
							},
							onViewUpdate: (e) => {
								this.updateState({
									...this.state,
									state: e.state,
									saved: this.state.saved === false ? false : !e.docChanged,
								} as State);
							},
						}),
					});
				});
		}
		promise.catch((e) => {
			if (e instanceof Error) {
				if (e.message.includes("ENOENT")) {
					this.updateState({
						type: "unknown",
						fileExists: false,
						ready: true,
						saved: false,
						state: null,
					});
					return;
				}
			}
			throw e;
		});
	}
	updateState = (state: State) => {
		this.state = state;
		this.emit("update");
	};
	static init = (container: WebContainer, path: string) => {
		return new DocumentState(container, path);
	};
	emit = (event: "update" | "save") => {
		for (const callback of this.subscribers) {
			callback(event);
		}
	};
	getState = (): State => {
		return this.state;
	};

	subscribe = (callback: (event: "update" | "save") => void) => {
		this.subscribers.add(callback);

		return () => {
			this.subscribers.delete(callback);
		};
	};

	dispose = () => {
		// DocumentState.getDocumentState.cache.delete(this.container);
		DocumentState.cache.get(this.container)?.delete(this.path);
	};
	static cache = new Map<WebContainer, Map<string, DocumentState>>();
	static getContainerDocumentMap = (container: WebContainer) => {
		let containerCache = DocumentState.cache.get(container);
		if (!containerCache) {
			containerCache = new Map<string, DocumentState>();
			DocumentState.cache.set(container, containerCache);
		}
		return containerCache;
	};
	static delete = (container: WebContainer, path: string) => {
		DocumentState.getContainerDocumentMap(container).delete(path);
	};
	static getDocumentState = (container: WebContainer, path: string) => {
		const containerCache = DocumentState.getContainerDocumentMap(container);

		const documentState = containerCache.get(path);
		if (!documentState) {
			const documentState = new DocumentState(container, path);
			containerCache.set(path, documentState);
			return documentState;
		}

		return documentState;
	};
}

export const useDocumentState = (path: string) => {
	const { container } = useWebContainer();
	const documentState = DocumentState.getDocumentState(container, path);

	return useSyncExternalStore(
		documentState.subscribe,
		documentState.getState,
		documentState.getState,
	);
};
