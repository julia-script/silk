"use client";

import path from "path";
import {
	ResizableHandle,
	ResizablePanel,
	ResizablePanelGroup,
} from "@/components/ui/resizable";
import { type Uuid, useEditorStore } from "@/lib/store";
import { cn } from "@/lib/utils";
import { ChevronRight, Dot, FileIcon, X } from "lucide-react";
import { Fragment, type PropsWithChildren, Suspense, useEffect } from "react";
import { FileTree } from "../FileTree";
import { Terminal } from "../Terminal";
import { useWebContainer } from "../WebContainer";
import { CodeMirror } from "../code-mirror";
import { HexView } from "./HexView";
import { Tab, TabRow } from "./Tabs";
import { DocumentState, useDocumentState } from "./editorStateManager";
export const EditorsView = () => {
	const rootView = useEditorStore((state) => state.rootView);
	const buffers = useEditorStore((state) => state.buffers);
	const store = useEditorStore();
	const { container } = useWebContainer();
	useEffect(() => {
		const documentsMap = DocumentState.getContainerDocumentMap(container);
		const openFiles = new Set<string>(Object.keys(buffers));
		for (const [path, documentState] of documentsMap.entries()) {
			if (openFiles.has(path)) continue;
			const doc = documentsMap.get(path);

			if (doc) {
				documentsMap.delete(path);
				doc.dispose();
			}
		}
	}, [buffers, container]);

	return (
		<div className="flex flex-col h-full w-full relative font-mono">
			<ResizablePanelGroup direction="horizontal">
				<ResizablePanel defaultSize={10} className="min-w-48">
					<FileTree />
				</ResizablePanel>
				<ResizableHandle />
				<ResizablePanel defaultSize={90}>
					<ResizablePanelGroup direction="vertical">
						<ResizablePanel defaultSize={70}>
							{store.isHydrated && rootView && <Panel id={rootView} />}
						</ResizablePanel>
						<ResizableHandle />
						<Terminal />
					</ResizablePanelGroup>
				</ResizablePanel>
			</ResizablePanelGroup>
		</div>
	);
};
const Panel = ({
	id,
	depth = 1,
}: PropsWithChildren<{
	id: string;
	depth?: number;
}>) => {
	const store = useEditorStore();
	const view = store.views[id];
	if (view.type === "tabs") {
		return <TabsView id={id} />;
	}
	// if (view.splitType !== splitType) {
	return (
		<ResizablePanelGroup
			direction={view.splitType}
			id={id}
			onLayout={(layout) => {
				store.updateViews({
					...store.views,
					[view.id]: {
						...view,
						splits: view.splits.map((split, index) => ({
							...split,
							size: layout[index],
						})),
					},
				});
			}}
		>
			{view.splits.map((split, index) => (
				<Fragment key={split.id}>
					{index > 0 && <ResizableHandle withHandle />}
					<ResizablePanel
						key={split.id}
						className="ring-1 ring-neutral-500"
						defaultSize={split.size}
						minSize={10}
					>
						<Panel id={split.id} depth={depth + 1} />
					</ResizablePanel>
				</Fragment>
			))}
		</ResizablePanelGroup>
	);
	// }
};

const TabsView = ({ id }: { id: string }) => {
	const store = useEditorStore();
	const view = store.views[id];
	if (view.type !== "tabs") throw new Error("Not a tabs view");

	return (
		<div className="bg-neutral-950 flex flex-col h-full grow">
			<TabRow id={id} className="bg-neutral-950 ">
				{view.tabs.map((tab) => {
					switch (store.buffers[tab].type) {
						case "file":
							return <FileTabButton key={tab} bufferId={tab} viewId={id} />;
						default:
							return null;
					}
				})}
			</TabRow>
			<div className="flex flex-col overflow-y-auto grow">
				{view.focusedTab && <BufferView id={view.focusedTab} />}
			</div>
		</div>
	);
};
const FileTabButton = ({
	bufferId,
	viewId,
}: { bufferId: string; viewId: string }) => {
	const store = useEditorStore();
	const view = store.views[viewId];
	if (view.type !== "tabs") throw new Error("Not a tabs view");
	const buffer = store.buffers[bufferId];
	if (buffer.type !== "file") throw new Error("Not a file buffer");
	const editorState = useDocumentState(buffer.file);

	return (
		<Tab
			key={bufferId}
			id={bufferId}
			active={view.focusedTab === bufferId}
			className={cn("group relative", {
				italic: !editorState.saved && editorState.ready,
				"line-through": !editorState.fileExists && editorState.ready,
			})}
			onClick={() => {
				console.log("clicked", bufferId);
				store.setFocusedFile(viewId, buffer.file);
			}}
		>
			<FileIcon className="size-3 opacity-50" />
			<span>{path.basename(buffer.file)}</span>
			<div className="relative h-full flex items-center w-2">
				<span
					className={cn(
						"transition-opacity duration-100 ease-in-out group-hover:opacity-0 absolute left-0 p-0.5 cursor-none",
						editorState.saved ? "opacity-0" : "opacity-100",
					)}
				>
					<Dot className="size-2 scale-[3]" />
				</span>
				<button
					type="button"
					className="opacity-0 left-0 absolute group-hover:opacity-100 transition-opacity duration-150 ease-in-out hover:bg-neutral-500 rounded-[2px] p-0.5"
					onClick={(e) => {
						e.preventDefault();
						e.stopPropagation();
						store.closeFile(bufferId, viewId);
					}}
				>
					<X className="w-2 h-2" />
				</button>
			</div>
		</Tab>
	);
};

const BufferView = ({ id }: { id: string }) => {
	const store = useEditorStore();
	const buffer = store.buffers[id];

	if (buffer.type !== "file") return <div>No buffer</div>;
	const editorState = useDocumentState(buffer.file);
	return (
		<div className="text-white flex flex-col grow overflow-hidden">
			<Divider />
			<Breadcrumbs filePath={buffer.file} />
			<Divider />

			<Suspense fallback={<div>Loading...</div>}>
				{editorState.type === "bytes" ? (
					<HexView filePath={buffer.file} />
				) : (
					<CodeView filePath={buffer.file} />
				)}
			</Suspense>
		</div>
	);
};
const Divider = () => {
	return <hr className="h-px bg-neutral-700/30  border-0" />;
};
const Breadcrumbs = ({ filePath }: { filePath: Uuid }) => {
	return (
		<div className="flex items-center gap-1 text-white px-2 text-xs py-1 ">
			{filePath.split(/\.?\//).map((part, i, list) => {
				const isLast = i === list.length - 1;
				return (
					<Fragment key={part + i}>
						{i > 0 && <ChevronRight className={cn("size-4 opacity-60")} />}
						{isLast && <FileIcon className={cn("size-3 opacity-60")} />}
						<span
							className={cn({
								"opacity-50": !isLast,
								"font-bold": isLast,
							})}
						>
							{part}
						</span>
					</Fragment>
				);
			})}
		</div>
	);
};
const CodeView = ({ filePath }: { filePath: Uuid }) => {
	const editorState = useDocumentState(filePath);
	if (editorState.type !== "editor") {
		return null;
	}

	return (
		<>
			<CodeMirror className="grow" editorState={editorState.state} />
		</>
	);
};
