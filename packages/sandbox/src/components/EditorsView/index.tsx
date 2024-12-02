"use client";

import path from "path";
import {
	ResizableHandle,
	ResizablePanel,
	ResizablePanelGroup,
} from "@/components/ui/resizable";
import { type Uuid, getEditorState, useEditorStore } from "@/lib/store";
import { cn } from "@/lib/utils";
import { arrayMove } from "@dnd-kit/sortable";
import type { WebContainer } from "@webcontainer/api";
import { memoize } from "lodash";
import { ChevronLeft, ChevronRight, Dot, FileIcon, X } from "lucide-react";
import {
	Fragment,
	type PropsWithChildren,
	Suspense,
	use,
	useCallback,
	useEffect,
	useMemo,
	useState,
} from "react";
import { BaseHexEditor } from "react-hex-editor";
import { FileTree } from "../FileTree";
import { useWebContainer } from "../WebContainer";
import { CodeMirror } from "../code-mirror";
import { Tab, TabRow, Tabs } from "./Tabs";
import { DocumentState, useDocumentState } from "./editorStateManager";

import { Button } from "@/components/ui/button";
import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { Input } from "@/components/ui/input";
import { Label } from "@/components/ui/label";
import {
	Select,
	SelectContent,
	SelectItem,
	SelectTrigger,
	SelectValue,
} from "@/components/ui/select";
import { Terminal } from "../Terminal";
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
				italic: !editorState.saved,
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
	return (
		<div className="text-white flex flex-col grow overflow-hidden">
			<Divider />
			<Breadcrumbs filePath={buffer.file} />
			<Divider />

			<Suspense fallback={<div>Loading...</div>}>
				{buffer.file.endsWith(".wasm") ? (
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

	return (
		<>
			<CodeMirror className="grow" editorState={editorState.state} />
		</>
	);
};
const getFile = memoize(
	(container: WebContainer, filePath: Uuid) => {
		return container.fs.readFile(filePath);
	},
	(container, filePath) => filePath,
);
export const HexView = ({ filePath }: { filePath: string }) => {
	const { container } = useWebContainer();
	const [columns, setColumns] = useState(16);
	const bytes = use(getFile(container, filePath));
	const [rowsPerPage, setRowsPerPage] = useState(16);
	const [page, setPage] = useState(0);

	const totalRows = Math.ceil(bytes.length / columns);
	const totalPages = Math.ceil(totalRows / rowsPerPage);
	const offset = page * rowsPerPage * columns;

	const handlePrevPage = () => setPage((prev) => Math.max(0, prev - 1));
	const handleNextPage = () =>
		setPage((prev) => Math.min(totalPages - 1, prev + 1));

	return (
		<div className="w-full max-w-6xl mx-auto flex-col flex grow-0 rounded-lg space-y-4 py-4  h-full overflow-auto px-4">
			<div className="text-xs">
				<p className="text-neutral-300">File: {filePath}</p>
				<p className="text-neutral-300">Total bytes: {bytes.length}</p>
			</div>
			<div className="flex flex-wrap gap-4">
				<div className="flex items-center space-x-2 text-xs">
					<Label htmlFor="columns" className="text-neutral-300">
						Columns:
					</Label>
					<Select
						value={columns.toString()}
						onValueChange={(value) => setColumns(Number(value))}
					>
						<SelectTrigger className="w-[100px] bg-neutral-950 text-neutral-100 border-neutral-700">
							<SelectValue placeholder="Columns" />
						</SelectTrigger>
						<SelectContent>
							<SelectItem value="4">4</SelectItem>
							<SelectItem value="8">8</SelectItem>
							<SelectItem value="16">16</SelectItem>
							<SelectItem value="32">32</SelectItem>
						</SelectContent>
					</Select>
				</div>
				<div className="flex items-center space-x-2 text-xs">
					<Label htmlFor="rowsPerPage" className="text-neutral-300 ">
						Rows per page:
					</Label>
					<Select
						value={rowsPerPage.toString()}
						onValueChange={(value) => setRowsPerPage(Number(value))}
					>
						<SelectTrigger className="w-[100px] bg-neutral-950 text-neutral-100 border-neutral-700">
							<SelectValue placeholder="Rows" />
						</SelectTrigger>
						<SelectContent>
							<SelectItem value="8">8</SelectItem>
							<SelectItem value="16">16</SelectItem>
							<SelectItem value="32">32</SelectItem>
							<SelectItem value="64">64</SelectItem>
						</SelectContent>
					</Select>
				</div>
			</div>
			<HexViewer
				offset={offset}
				rowsToDisplay={rowsPerPage}
				bytes={bytes}
				columns={columns}
			/>
			<div className="flex justify-between items-center mt-4">
				<Button
					onClick={handlePrevPage}
					disabled={page === 0}
					variant="outline"
					className="bg-neutral-950 text-neutral-100 border-neutral-700"
				>
					<ChevronLeft className="mr-2 h-4 w-4" /> Previous
				</Button>
				<span className="text-neutral-300">
					Page {page + 1} of {totalPages}
				</span>
				<Button
					onClick={handleNextPage}
					disabled={page === totalPages - 1}
					variant="outline"
					className="bg-neutral-950 text-neutral-100 border-neutral-700"
				>
					Next <ChevronRight className="ml-2 h-4 w-4" />
				</Button>
			</div>
			{filePath.endsWith(".wasm") && <WatView path={filePath} />}
		</div>
	);
};

const WatView = ({ path }: { path: string }) => {
	const editorState = useDocumentState(path);
	return (
		// <div className=" overflow-auto flex flex-col h-full grow">
		<CodeMirror
			editorState={editorState.state}
			className="h-full overflow-auto min-h-96"
		/>
		// </div>
	);
};

const HexViewer = ({
	offset,
	rowsToDisplay,
	bytes,
	columns,
}: {
	columns: number;
	offset: number;
	rowsToDisplay: number;
	bytes: Uint8Array;
}) => {
	const [hoveredIndex, setHoveredIndex] = useState<number | null>(null);
	const [selectionStart, setSelectionStart] = useState<number | null>(null);
	const [selectionEnd, setSelectionEnd] = useState<number | null>(null);
	const [isSelecting, setIsSelecting] = useState(false);

	const handleMouseEnter = (index: number) => {
		setHoveredIndex(index);
		if (isSelecting) {
			setSelectionEnd(index);
		}
	};

	const handleMouseLeave = () => {
		setHoveredIndex(null);
	};

	const handleMouseDown = (index: number) => {
		setIsSelecting(true);
		setSelectionStart(index);
		setSelectionEnd(index);
	};

	const handleMouseUp = () => {
		setIsSelecting(false);
	};

	const handleMouseMove = (index: number) => {
		if (isSelecting) {
			setSelectionEnd(index);
		}
	};

	const isSelected = useCallback(
		(index: number) => {
			if (selectionStart === null || selectionEnd === null) return false;
			const start = Math.min(selectionStart, selectionEnd);
			const end = Math.max(selectionStart, selectionEnd);
			return index >= start && index <= end;
		},
		[selectionStart, selectionEnd],
	);

	const rows = useMemo(() => {
		const result: number[] = [];
		for (let i = offset; i < offset + rowsToDisplay * columns; i += columns) {
			result.push(i);
		}
		return result;
	}, [offset, rowsToDisplay, columns]);

	return (
		<div
			className="grid grid-cols-[auto_1px_1fr_auto] gap-0 font-mono text-sm overflow-x-auto border border-neutral-700 rounded-md bg-neutral-950 shrink-0"
			onMouseUp={handleMouseUp}
			onMouseLeave={handleMouseUp}
		>
			<div className="flex flex-col text-right pr-2 bg-neutral-950">
				{rows.map((row) => (
					<HexCell
						byte={row}
						key={row}
						className="py-1 px-2 text-neutral-500"
					/>
				))}
			</div>
			<div className="bg-neutral-700" />
			<div
				className="grid gap-0 bg-neutral-950 px-2"
				style={{
					gridTemplateColumns: `repeat(${columns}, minmax(0, 1fr))`,
				}}
			>
				{rows.map((row) => (
					<div key={row} className="contents">
						{[...Array(columns)].map((_, i) => {
							const index = row - offset + i;
							const byte = bytes[row + i];
							return (
								<HexCell
									byte={byte}
									key={i}
									className={`py-1 text-center cursor-pointer select-none ${
										hoveredIndex === index ? "bg-neutral-700" : ""
									} ${isSelected(index) ? "bg-neutral-600" : ""} ${byte === undefined ? "invisible" : ""}`}
									onMouseEnter={() => handleMouseEnter(index)}
									onMouseLeave={handleMouseLeave}
									onMouseDown={() => handleMouseDown(index)}
									onMouseMove={() => handleMouseMove(index)}
								/>
							);
						})}
					</div>
				))}
			</div>
			<div className="flex flex-col pl-2 bg-neutral-950">
				{rows.map((row) => (
					<div key={row} className="py-1 px-2 select-none">
						{[...Array(columns)].map((_, i) => {
							const index = row - offset + i;
							const byte = bytes[row + i];
							return (
								<span
									key={i}
									className={`cursor-pointer ${
										hoveredIndex === index ? "bg-neutral-700" : ""
									} ${isSelected(index) ? "bg-neutral-600" : ""} ${byte === undefined ? "invisible" : ""}`}
									onMouseEnter={() => handleMouseEnter(index)}
									onMouseLeave={handleMouseLeave}
									onMouseDown={() => handleMouseDown(index)}
									onMouseMove={() => handleMouseMove(index)}
								>
									{byte !== undefined
										? byte >= 32 && byte <= 126
											? String.fromCharCode(byte)
											: "."
										: " "}
								</span>
							);
						})}
					</div>
				))}
			</div>
		</div>
	);
};

const HexCell = ({
	byte,
	className,
	onMouseEnter,
	onMouseLeave,
	onMouseDown,
	onMouseMove,
}: {
	byte: number | undefined;
	className?: string;
	onMouseEnter?: () => void;
	onMouseLeave?: () => void;
	onMouseDown?: () => void;
	onMouseMove?: () => void;
}) => {
	if (byte === undefined) {
		return <span className={className}>{"\u00A0\u00A0"}</span>;
	}

	const byteString = byte.toString(16).padStart(2, "0").toUpperCase();

	return (
		<span
			className={`${className} text-neutral-300`}
			onMouseEnter={onMouseEnter}
			onMouseLeave={onMouseLeave}
			onMouseDown={onMouseDown}
			onMouseMove={onMouseMove}
		>
			{byteString[0] === "0" ? (
				<span className="opacity-50">
					{byteString[0]}
					{byteString[1] === "0" ? byteString[1] : ""}
				</span>
			) : (
				byteString[0]
			)}
			{byte > 0 && byteString[1]}
		</span>
	);
};
