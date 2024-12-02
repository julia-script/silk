import { ChevronLeft, ChevronRight } from "lucide-react";
import { use, useCallback, useMemo, useState } from "react";
import { useWebContainer } from "../WebContainer";
import { CodeMirror } from "../code-mirror";
import { useDocumentState } from "./editorStateManager";

import { Button } from "@/components/ui/button";
import { Label } from "@/components/ui/label";
import {
	Select,
	SelectContent,
	SelectItem,
	SelectTrigger,
	SelectValue,
} from "@/components/ui/select";
import type { Uuid } from "@/lib/store";
import type { WebContainer } from "@webcontainer/api";
import memoize from "lodash-es/memoize";

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
