"use client";

import path from "path";
import {
	ContextMenu,
	ContextMenuContent,
	ContextMenuItem,
	ContextMenuSeparator,
	ContextMenuShortcut,
	ContextMenuTrigger,
} from "@/components/ui/context-menu";
import {
	ResizableHandle,
	ResizablePanel,
	ResizablePanelGroup,
} from "@/components/ui/resizable";
import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { useSilkTools } from "@/lib/silk-toos";
import { type Uuid, genBufId, useEditorStore } from "@/lib/store";
import { cn } from "@/lib/utils";
import {
	ChevronRight,
	Dot,
	FileIcon,
	SplitSquareHorizontal,
	SplitSquareVertical,
	X,
} from "lucide-react";
import {
	Fragment,
	type PropsWithChildren,
	Suspense,
	useEffect,
	useMemo,
	useState,
} from "react";
import { FileTree } from "../FileTree";
import { Terminal } from "../Terminal";
import { useWebContainer } from "../WebContainer";
import { CodeMirror } from "../code-mirror";
import { AstVisualizer, GenericNode } from "./AstVisualizer";
import { HexView } from "./HexView";
import { Tab, TabRow } from "./Tabs";
import { TokenVisualizer } from "./TokenVisualizer";

import { InspectView } from "./InspectView";
import { DocumentState, useDocumentState } from "./editorStateManager";
const invertDirection = (direction: "horizontal" | "vertical") => {
	return direction === "horizontal" ? "vertical" : "horizontal";
};
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
			// Resizable panel considers a left and right split to be horizontal. I'm more used to vim splits, where that would be vertical, so I switch it at render time
			direction={invertDirection(view.splitType)}
			id={id}
			onLayout={(layout) => {
				store.updateViews({
					...store.views,
					[view.id]: {
						...view,
						splits: view.splits.map((split, index) => ({
							...split,
							size: layout[index] / 100,
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
						defaultSize={split.size * 100}
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
	const buffer = view.focusedTab && store.buffers[view.focusedTab];

	return (
		<div className="bg-neutral-950 flex flex-col h-full grow">
			<TabRow id={id} className="bg-neutral-950 ">
				{view.tabs.map((tab) => {
					switch (store.buffers[tab].type) {
						case "inspect":
						case "file":
							return <FileTabButton key={tab} bufferId={tab} viewId={id} />;
						default:
							return null;
					}
				})}
			</TabRow>
			<div className="flex flex-col overflow-y-auto grow">
				{view.focusedTab && buffer && (
					<>
						{buffer.type === "inspect" && <InspectView id={buffer.id} />}
						{buffer.type === "file" && <BufferView id={buffer.id} />}
					</>
				)}
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
	if (buffer.type !== "file" && buffer.type !== "inspect")
		throw new Error("Not a file buffer");
	const editorState = useDocumentState(buffer.file);

	return (
		<ContextMenu>
			<ContextMenuTrigger>
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
						store.setFocusedBuffer(viewId, buffer.id);
					}}
				>
					<FileIcon className="size-3 opacity-50" />
					<span>
						{buffer.type === "inspect" ? "inspect: " : ""}
						{path.basename(buffer.file)}
					</span>
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
			</ContextMenuTrigger>
			<ContextMenuContent className="text-xs">
				{(
					[
						{
							dir: "left",
						},
						{
							dir: "right",
						},
						{
							dir: "top",
						},
						{
							dir: "bottom",
						},
					] as const
				).map(({ dir }) => (
					<ContextMenuItem
						key={dir}
						onClick={() => store.splitView(viewId, dir, bufferId)}
						className="text-xs"
					>
						{/* <ContextMenuSeparator /> */}
						{dir === "left" || dir === "right" ? (
							<SplitSquareHorizontal className="mr-2 size-3" />
						) : (
							<SplitSquareVertical className="mr-2 size-3" />
						)}
						Split {dir}
					</ContextMenuItem>
				))}
			</ContextMenuContent>
		</ContextMenu>
	);
};

const BufferView = ({ id }: { id: string }) => {
	const store = useEditorStore();
	const buffer = store.buffers[id];

	if (buffer.type !== "file") throw new Error("Not a file buffer");
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
	const store = useEditorStore();
	if (editorState.type !== "editor" || !editorState.ready) {
		return null;
	}

	return (
		<>
			<CodeMirror
				className="grow"
				highlights={
					store.bufferHighlights[genBufId({ type: "file", file: filePath })]
				}
				editorState={editorState.state}
				focusOnMount={true}
			/>
		</>
	);
};

// export default function InspectView({ id }: { id: string }) {
// 	const store = useEditorStore();
// 	const buffer = store.buffers[id];
// 	const silk = useSilkTools();
// 	const [activeTab, setActiveTab] = useState<"ast" | "tokens">("ast");

// 	if (buffer.type !== "inspect") throw new Error("Not an inspect buffer");

// 	const editorState = useDocumentState(buffer.file);
// 	const docString = useMemo(() => {
// 		if (editorState.type !== "editor") return "not an editor";
// 		return editorState.state.doc.toString();
// 	}, [editorState]);

// 	const lexed = useMemo(() => {
// 		return silk.lex(docString);
// 	}, [docString, silk]);

// 	const ast = useMemo(() => {
// 		return silk.parseAst(docString);
// 	}, [docString, silk]);

// 	const hir = useMemo(() => {
// 		return silk.parseHir(docString);
// 	}, [docString, silk]);

// 	console.log(hir);

// 	const updateHighlights = (start: number, end: number) => {
// 		store.updateBufferHighlights(
// 			genBufId({
// 				type: "file",
// 				file: buffer.file,
// 			}),
// 			[{ start, end, type: "highlight" }],
// 		);
// 	};

// 	return (
// 		<Tabs
// 			value={activeTab}
// 			onValueChange={(value) => setActiveTab(value as "ast" | "tokens" | "hir")}
// 		>
// 			<TabsList>
// 				<TabsTrigger value="tokens">Tokens</TabsTrigger>
// 				<TabsTrigger value="ast">AST</TabsTrigger>
// 				<TabsTrigger value="hir">HIR</TabsTrigger>
// 			</TabsList>
// 			<TabsContent value="ast">
// 				<GenericNode
// 					index={0}
// 					getChildren={(node) => {
// 						const astNode = ast.nodes[node];
// 						const children: [string, unknown][] = [];
// 						const data = astNode.data;
// 						const tag = Object.keys(data)[0] as keyof typeof data;
// 						for (const [key, value] of Object.entries(data[tag])) {
// 							if (key.endsWith("list")) {
// 								const list = value as number[];
// 								children.push(
// 									...list.map(
// 										(v, i) => [`${key}[${i}]`, v] as [string, number],
// 									),
// 								);
// 								continue;
// 							}
// 							switch (key) {
// 								case "list": {
// 									break;
// 								}
// 								case "token":
// 									children.push([key, `%${value}`]);
// 									continue;

// 								default:
// 									children.push([key, value as number]);
// 									break;
// 							}
// 						}

// 						return children;
// 					}}
// 					getLabel={(node) => {
// 						const astNode = ast.nodes[node];
// 						const data = astNode.data;
// 						const tag = Object.keys(data)[0] as keyof typeof data;
// 						return `${tag}: ${astNode.start_token}-${astNode.end_token} #${node}`;
// 					}}
// 					onNodeClick={(index) => {
// 						const node = ast.nodes[index];
// 						const data = node.data;
// 						const startToken = lexed[node.start_token];
// 						const endToken = lexed[node.end_token];
// 						updateHighlights(startToken.start, endToken.end);
// 					}}
// 					onNodeHoverEnter={() => {}}
// 					onNodeHoverLeave={() => {}}
// 				/>
// 			</TabsContent>
// 			<TabsContent value="tokens">
// 				<TokenVisualizer
// 					tokens={lexed}
// 					onSelectionUpdate={(tokens) => {
// 						console.log("token selected", tokens);
// 						store.updateBufferHighlights(
// 							genBufId({
// 								type: "file",
// 								file: buffer.file,
// 							}),
// 							tokens.map((token) => ({
// 								start: token.start,
// 								end: token.end,
// 								type: "highlight",
// 							})),
// 						);
// 					}}
// 				/>
// 			</TabsContent>
// 			<TabsContent value="hir">
// 				<GenericNode
// 					index={0}
// 					getChildren={(node) => {
// 						const hirInst = hir.instructions[node];
// 						const children: [string, unknown][] = [];
// 						const tag = Object.keys(hirInst)[0] as keyof typeof hirInst;

// 						for (const [key, value] of Object.entries(hirInst[tag])) {
// 							if (key.endsWith("_list")) {
// 								const list = value as number[];
// 								children.push(
// 									...list.map(
// 										(v, i) => [`${key}[${i}]`, v] as [string, number],
// 									),
// 								);
// 							} else if (
// 								key.endsWith("_node") ||
// 								key.startsWith("ty_") ||
// 								key.endsWith("literal") ||
// 								key === "identifier"
// 							) {
// 								if (typeof value === "number") {
// 									children.push([key, `#${value}`]);
// 								} else {
// 									children.push([key, value]);
// 								}
// 							} else if (typeof value === "number") {
// 								children.push([key, value as number]);
// 							}
// 						}

// 						return children;
// 					}}
// 					getLabel={(node) => {
// 						const hirInst = hir.instructions[node];
// 						const tag = Object.keys(hirInst)[0] as keyof typeof hirInst;
// 						return `${tag} #${node}`;
// 					}}
// 					onNodeClick={(index) => {
// 						// const node = ast.nodes[index];
// 						// const data = node.data;
// 						// const startToken = lexed[node.start_token];
// 						// const endToken = lexed[node.end_token];
// 						// updateHighlights(startToken.start, endToken.end);
// 					}}
// 					onNodeHoverEnter={() => {}}
// 					onNodeHoverLeave={() => {}}
// 				/>
// 			</TabsContent>
// 		</Tabs>
// 	);
// }
