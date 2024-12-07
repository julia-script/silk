"use client";

import path from "path";
import { Button } from "@/components/ui/button";
import { useEditorStore } from "@/lib/store";
import { cn } from "@/lib/utils";
import { File, Inspect } from "lucide-react";
import { useWebContainer } from "../WebContainer";
import { Directory } from "./Directory";
import { FileTreeButton } from "./FileTreeButton";

export const Entry = ({
	entryPath,
	isDirectory,
}: {
	entryPath: string;
	isDirectory: boolean;
}) => {
	if (isDirectory) {
		return <Directory dirPath={entryPath} />;
	}

	const fileName = path.basename(entryPath);
	const { focusedBuffer, openBuffer, buffers } = useEditorStore();
	const { container } = useWebContainer();
	const isOpen = entryPath in buffers;
	const buf = buffers[entryPath];

	// buf.state.doc
	return (
		<FileTreeButton
			// className={cn(
			// 	"flex gap-1 w-full justify-start py-1 px-1 text-xs font-normal m-0 h-auto",
			// 	focusedBuffer === entryPath && "bg-neutral-800",
			// 	isOpen && "font-bold",
			// )}
			active={focusedBuffer === entryPath}
			onClick={async () => {
				// if (!isOpen) {
				// const content = await container.fs.readFile(entryPath, "utf-8");
				openBuffer({
					type: "file",
					file: entryPath,
				});
				// }
				// setFocusedBuffer(entryPath);
			}}
			icon={File}
		>
			{fileName}
			{fileName.endsWith(".slk") && (
				<button
					type="button"
					className="ml-auto px-2"
					onClick={() => {
						openBuffer({
							type: "inspect",
							file: entryPath,
						});
					}}
				>
					<Inspect className="text-white opacity-50 size-3" />
				</button>
			)}
		</FileTreeButton>
	);
};
