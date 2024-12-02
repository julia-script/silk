"use client";

import { CodeMirror } from "@/components/code-mirror";
import { type Buf, useEditorStore } from "@/lib/store";
import { cn } from "@/lib/utils";
import type { ViewUpdate } from "@codemirror/view";
import { useMutation, useSuspenseQuery } from "@tanstack/react-query";
import { useCallback, useEffect } from "react";
import { useWebContainer } from "../WebContainer";

export const BufEditor = ({ buf }: { buf: string }) => {
	const { container } = useWebContainer();

	const store = useEditorStore();
	const bufState = store.buffers[buf];

	if (!bufState) return <div>No buffer</div>;
	return (
		<div className="flex flex-col gap-2 relative ring">
			{/* <CodeMirror editorState={bufState.state} /> */}
			{store.rootView && <View id={store.rootView} />}
		</div>
	);
};

// const View = ({ id }: { id: string }) => {
// 	const store = useEditorStore();
// 	const view = store.views[id];

// 	if (view.type === "view")
// 		return <div className="ring absolute h-full w-full">{view.id}</div>;

// 	return (
// 		<div className="flex flex-col gap-2 relative">
// 			{view.splits.map((split) => {
// 				return (
// 					<div
// 						key={split.id}
// 						className={cn("absolute w-full h-full")}
// 						style={{
// 							flex: split.size,
// 						}}
// 					>
// 						<View id={split.id} />
// 					</div>
// 				);
// 			})}
// 		</div>
// 	);
// };
