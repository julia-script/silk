import type { WebContainer } from "@webcontainer/api";
import { vscodeDark } from "@uiw/codemirror-theme-vscode";
import { EditorState } from "@codemirror/state";
import { langLanguage } from "@lang/grammar/dist/language";
import { EditorView, keymap, type ViewUpdate } from "@codemirror/view";
import { basicSetup } from "codemirror";
import { Command, Edit } from "lucide-react";
import { memoize } from "lodash";
import { useEffect, useSyncExternalStore } from "react";
import { useWebContainer } from "../WebContainer";
import { decode } from "@webassemblyjs/wasm-parser";
import { print } from "@webassemblyjs/wast-printer";
import type { LRLanguage } from "@codemirror/language";
import { wastLanguage } from "@codemirror/lang-wast";
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
	const extensions = [vscodeDark, basicSetup];
	if (language) {
		extensions.push(language);
	}
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
		// extensions: [
		// 	vscodeDark,
		// 	basicSetup,
		// 	langLanguage,
		// 	keymap.of([
		// 		{
		// 			key: "Cmd-s",
		// 			stopPropagation: true,
		// 			run: (e) => {
		// 				onSave?.(e);
		// 				return true;
		// 			},
		// 		},
		// 		{
		// 			key: "Ctrl-s",
		// 			stopPropagation: true,
		// 			run: (e) => {
		// 				onSave?.(e);
		// 				return true;
		// 			},
		// 		},
		// 	]),
		// 	EditorView.updateListener.of((update) => {
		// 		onViewUpdate?.(update);
		// 	}),
		// ],
	});
};
export class DocumentState {
	state: EditorState;
	subscribers: Set<(event: "update" | "save") => void> = new Set();
	saved = true;
	ready = false;
	constructor(
		public container: WebContainer,
		public path: string,
	) {
		this.state = createState({
			content: "",
		});
		this.emit("update");

		const isWasm = this.path.endsWith(".wasm");
		if (isWasm) {
			this.container.fs.readFile(this.path).then((bytes) => {
				this.state = createState({
					content: wasmToWat(bytes),
					language: wastLanguage,
					readonly: true,
				});
				this.emit("update");
			});
		} else {
			this.container.fs.readFile(this.path, "utf-8").then((content) => {
				this.state = createState({
					content,
					onSave: async (e) => {
						await this.container.fs.writeFile(
							this.path,
							e.state.doc.toString(),
						);
						this.saved = true;
						this.emit("save");
					},
					onViewUpdate: (e) => {
						this.state = e.state;
						if (!e.docChanged) return;
						this.saved = false;
						this.emit("update");
					},
					language: langLanguage,
				});
				this.ready = true;
				this.emit("update");
			});
		}
	}
	static init = (container: WebContainer, path: string) => {
		return new DocumentState(container, path);
	};
	emit = (event: "update" | "save") => {
		this._state = undefined;
		for (const callback of this.subscribers) {
			callback(event);
		}
	};

	_state:
		| {
				state: EditorState;
				saved: boolean;
				ready: boolean;
		  }
		| undefined;
	getState = () => {
		if (this._state) return this._state;
		this._state = {
			state: this.state,
			saved: this.saved,
			ready: this.ready,
		};
		return this._state as {
			state: EditorState;
			saved: boolean;
			ready: boolean;
		};
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
	useEffect(() => {
		console.log("documentState", documentState);
	}, [documentState]);

	return useSyncExternalStore(
		documentState.subscribe,
		documentState.getState,
		documentState.getState,
	);
};
