import { create } from "zustand";
import { createEditorState } from "./createEditorState";
import type { WebContainer } from "@webcontainer/api";
import { omit, set, uniqueId, without } from "lodash";
import { devtools, persist } from "zustand/middleware";
import type { EditorState } from "@codemirror/state";
import {
	adjustChildren,
	remapSplits,
	type Size,
} from "@/components/EditorsView/resize";
import { useWebContainer } from "@/components/WebContainer";

export type Buf =
	| {
			id: Uuid;
			type: "file";
			file: string;
	  }
	| {
			type: "empty";
			id: Uuid;
	  };

export type Uuid = string;
type TabsView = {
	id: Uuid;
	type: "tabs";
	// buffer: Uuid;
	tabs: Uuid[];
	focusedTab: null | Uuid;
};
export type SplitView = {
	id: Uuid;

	type: "split";
	splits: {
		id: Uuid;
		size: number;
	}[];
	splitType: "vertical" | "horizontal";
};
export type View = TabsView | SplitView;
interface EditorStore {
	isHydrated: boolean;
	buffers: Record<string, Buf>;
	views: Record<string, View>;

	rootView: Uuid;
	focusedView: Uuid;
	openBuffers: string[];
	focusedBuffer: Uuid;

	openFile: (path: string, tabId?: Uuid) => void;
	closeFile: (path: string, tabId?: Uuid) => void;
	setFocusedFile: (view: Uuid, file: string | null) => void;
	// setOpenBuffers: (buffers: string[]) => void;
	// updateView: (view: View) => void;
	updateViews: (views: Record<Uuid, View>) => void;
	updateBuffer: (buffer: Buf) => void;

	// splitView: (
	// 	view: View,
	// 	direction: "top" | "left" | "right" | "bottom",
	// 	mainSize?: number,
	// ) => void;
	// closeView: (view: Uuid) => void;
}

const bufferId = () => uniqueId("buf-");
const viewId = () => uniqueId("view-");
const initialViewId = viewId();
const initialBufferId = bufferId();

export const editorStateMap = new Map<
	Uuid,
	Promise<{
		state: EditorState;
		saved: boolean;
	}>
>();

export const getEditorState = (
	path: Uuid,
	container: WebContainer,
	onDocChanged?: (state: EditorState) => void,
): Promise<{ state: EditorState; saved: boolean }> => {
	const cached = editorStateMap.get(path);
	if (cached) {
		return Promise.resolve(cached);
	}
	const promise = container.fs.readFile(path, "utf-8").then((content) => {
		const state = createEditorState({
			initialValue: content,
			onViewUpdate(update) {
				if (!update.docChanged) return;
				// console.log("onViewUpdate", update);
				onDocChanged?.(update.state);
				// setEditorState(path, update.state, false);
			},
		});
		const result = { state, saved: true };
		editorStateMap.set(path, Promise.resolve(result));
		return result;
	});

	return promise;
};
export const setEditorState = (
	path: Uuid,
	state: EditorState,
	saved: boolean,
) => {
	editorStateMap.set(path, Promise.resolve({ state, saved }));
};

export const useEditorStore = create<EditorStore>()(
	devtools(
		persist(
			(set) => ({
				isHydrated: false,
				buffers: {
					// [initialBufferId]: {
					// 	id: initialBufferId,
					// 	type: "file",
					// 	file: "",
					// },
				} as Record<string, Buf>,
				openBuffers: [] as string[],
				focusedBuffer: initialBufferId,
				views: {
					[initialViewId]: {
						id: initialViewId,
						type: "tabs",
						tabs: [],
						focusedTab: null,
					},
				} as Record<string, View>,
				focusedView: initialViewId,
				rootView: initialViewId,

				openFile: (path, _tabId) =>
					set((state) => {
						const tabId = _tabId ?? state.focusedView;
						const tabView = state.views[tabId];
						if (tabView.type !== "tabs") {
							throw new Error("View is not a tabs view");
						}
						const tabs = [...tabView.tabs];

						if (!tabs.includes(path)) {
							tabs.push(path);
						}

						return {
							...state,
							views: {
								...state.views,
								[tabId]: {
									...tabView,
									tabs,
									focusedTab: path,
								},
							},
							buffers: {
								...state.buffers,
								[path]: {
									id: path,
									type: "file",
									file: path,
								},
							},
						};
					}),
				closeFile: (bufferId, _tabId) =>
					set((state) => {
						const tabId = _tabId ?? state.focusedView;
						const tabView = state.views[tabId];
						if (tabView.type !== "tabs") {
							throw new Error("View is not a tabs view");
						}
						let focusedTab = tabView.focusedTab;
						if (focusedTab === bufferId) {
							const index = tabView.tabs.indexOf(bufferId);
							focusedTab = index > 0 ? tabView.tabs[index - 1] : null;
						}
						const tabs = without(tabView.tabs, bufferId);
						const views = {
							...state.views,
							[tabId]: {
								...tabView,
								tabs,
								focusedTab,
							},
						};
						let deleteBuffer = true;
						for (const view of Object.values(views)) {
							if (view.type !== "tabs") continue;
							if (view.tabs.includes(bufferId)) {
								deleteBuffer = false;
								break;
							}
						}
						return {
							views,
							buffers: deleteBuffer
								? omit(state.buffers, bufferId)
								: state.buffers,
						};
					}),
				setFocusedFile: (view, file) =>
					set((state) => {
						const tabView = state.views[view];
						if (!tabView || tabView.type !== "tabs") {
							throw new Error(`View '${view}' is not a tabs view`);
						}
						return {
							views: {
								...state.views,
								[view]: {
									...tabView,
									focusedTab: file,
								},
							},
							// buffers: {
							// 	...state.buffers,
							// 	[state.focusedBuffer]: {
							// 		...state.buffers[state.focusedBuffer],
							// 		focusedFile: file,
							// 	},
							// },
						};
					}),
				updateViews: (views) => set({ views }),
				updateBuffer: (buffer) =>
					set((state) => ({
						buffers: { ...state.buffers, [buffer.id]: buffer },
					})),
			}),
			{
				name: "Editor Store",
				skipHydration: false,
				merge: (persistedState, currentState) => {
					return {
						...currentState,
						...(persistedState ?? {}),
						isHydrated: true,
						editorStates: {} as Record<
							Uuid,
							{ state: Promise<EditorState>; saved: boolean }
						>,
					};
				},
			},
		),
		{
			name: "Editor Store",
		},
	),
);
