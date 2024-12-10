import { create } from "zustand";
import { createEditorState } from "./createEditorState";
import type { WebContainer } from "@webcontainer/api";
import { merge, omit, replace, set, uniqueId, without } from "lodash";
import { devtools, persist } from "zustand/middleware";
import type { EditorState } from "@codemirror/state";
import {
	adjustChildren,
	remapSplits,
	type Size,
} from "@/components/EditorsView/resize";
import { useWebContainer } from "@/components/WebContainer";
import { useCallback } from "react";

export type AnonymousBuf =
	| {
			type: "file";
			file: string;
	  }
	| {
			type: "empty";
	  }
	| {
			type: "inspect";
			file: string;
	  };

type Buf = {
	id: Uuid;
} & AnonymousBuf;
export const genBufId = (buf: AnonymousBuf) => {
	if ("file" in buf) {
		return `${buf.type}:${buf.file}`;
	}
	return buf.type;
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
export type BufferHighlight = {
	start: number;
	end: number;
	type: "highlight";
	className: string;
};
interface EditorStore {
	isHydrated: boolean;
	buffers: Record<string, Buf>;
	views: Record<string, View>;

	rootView: Uuid;
	focusedView: Uuid;
	openBuffers: string[];
	focusedBuffer: Uuid;
	bufferHighlights: Record<Uuid, BufferHighlight[]>;

	openFile: (path: string, tabId?: Uuid) => void;
	closeFile: (path: string, tabId?: Uuid) => void;
	setFocusedBuffer: (view: Uuid, file: string | null) => void;
	// setOpenBuffers: (buffers: string[]) => void;
	// updateView: (view: View) => void;
	updateViews: (views: Record<Uuid, View>) => void;
	splitView: (
		viewId: Uuid,
		direction: "top" | "left" | "right" | "bottom",
		focusedBuffer: Uuid,
	) => void;
	updateBuffer: (buffer: Buf) => void;

	openBuffer: (buffer: AnonymousBuf, tabId?: Uuid) => void;
	// splitView: (
	// 	view: View,
	// 	direction: "top" | "left" | "right" | "bottom",
	// 	mainSize?: number,
	// ) => void;
	// closeView: (view: Uuid) => void;
	updateBufferHighlights: (
		bufferId: Uuid,
		highlights: BufferHighlight[],
	) => void;
}

const bufferId = () => uniqueId("buf-");
const getViewId = () => uniqueId("view-");
const initialRootViewId = getViewId();
const initialTabViewId = getViewId();
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
				onDocChanged?.(update.state);
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
					[initialRootViewId]: {
						id: initialRootViewId,
						type: "split",
						splits: [{ id: initialTabViewId, size: 1 }],
						splitType: "vertical",
					},
					[initialTabViewId]: {
						id: initialTabViewId,
						type: "tabs",
						tabs: [],
						focusedTab: null,
					},
				} as Record<string, View>,
				focusedView: initialTabViewId,
				rootView: initialRootViewId,
				bufferHighlights: {} as Record<Uuid, BufferHighlight[]>,
				openBuffer: (buffer, _tabId) =>
					set((state) => {
						const tabId = _tabId ?? state.focusedView;
						const tabView = state.views[tabId];

						if (tabView.type !== "tabs") {
							throw new Error("View is not a tabs view");
						}
						const tabs = [...tabView.tabs];
						const id = genBufId(buffer);
						if (!tabs.includes(id)) {
							tabs.push(id);
						}
						// const tabs = [...tabView.tabs];
						return {
							buffers: {
								...state.buffers,
								[id]: {
									id,
									...buffer,
								} as Buf,
							},
							views: {
								...state.views,
								[tabId]: {
									...tabView,
									tabs,
									focusedTab: id,
								},
							},
						};
					}),
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
				setFocusedBuffer: (view, bufferId) =>
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
									focusedTab: bufferId,
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
				updateBufferHighlights: (bufferId, highlights) =>
					set((state) => ({
						bufferHighlights: {
							...state.bufferHighlights,
							[bufferId]: highlights,
						},
					})),
				splitView: (viewId, direction, focusedBuffer) =>
					set((state) => {
						const view = state.views[viewId];
						if (view.type === "split") {
							throw new Error(`View '${viewId}' is already a split view`);
						}
						const views = { ...state.views };
						const newTabView: TabsView = {
							id: getViewId(),
							type: "tabs",
							tabs: [focusedBuffer],
							focusedTab: focusedBuffer,
						};
						let parentSplit = Object.values(state.views).find(
							(v) =>
								v.type === "split" && v.splits.some((s) => s.id === viewId),
						) as SplitView | null;
						if (!parentSplit) {
							throw new Error(
								`Couldn't find parent split view for '${viewId}'`,
							);
						}
						parentSplit = {
							...parentSplit,
						};
						const newSplitAxis = getAxis(direction);

						if (newSplitAxis !== parentSplit.splitType) {
							// if the parent split view is not on the same axis of the new split, we need to create a new split view to use as parent
							// if parent has only one or less splits, we can just reorient it
							if (parentSplit.splits.length <= 1) {
								parentSplit.splitType = newSplitAxis;
							} else {
								const newParentSplit: SplitView = {
									id: getViewId(),
									type: "split",
									splits: [{ id: viewId, size: 1 }],
									splitType: newSplitAxis,
								};
								views[parentSplit.id] = {
									...parentSplit,
									splits: parentSplit.splits.map((s) =>
										s.id === viewId
											? {
													...s,
													id: newParentSplit.id,
												}
											: s,
									),
								};

								views[newParentSplit.id] = newParentSplit;
								parentSplit = newParentSplit;
							}
						}

						const newSplitWidth = 1 / (parentSplit.splits.length + 1);
						const adjustedParentSplits = parentSplit.splits.map((s) => ({
							...s,
							size: s.size - newSplitWidth / parentSplit.splits.length,
						}));
						if (direction === "left" || direction === "top")
							adjustedParentSplits.push({
								id: newTabView.id,
								size: newSplitWidth,
							});
						else
							adjustedParentSplits.unshift({
								id: newTabView.id,
								size: newSplitWidth,
							});
						parentSplit.splits = adjustedParentSplits;

						views[parentSplit.id] = parentSplit;
						views[newTabView.id] = newTabView;

						return {
							...state,
							views,
						};
					}),
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
const isSameAxis = (
	a: "vertical" | "horizontal",
	b: "left" | "right" | "top" | "bottom",
) => {
	return a === getAxis(b);
};

const getAxis = (direction: "left" | "right" | "top" | "bottom") =>
	direction === "left" || direction === "right" ? "vertical" : "horizontal";

export const useHighlights = () => {
	const { bufferHighlights, updateBufferHighlights } = useEditorStore();
	return {
		highlights: bufferHighlights,
		update: useCallback(
			(file: string, highlights: BufferHighlight[]) => {
				updateBufferHighlights(genBufId({ type: "file", file }), highlights);
			},
			[updateBufferHighlights],
		),
	};
};
