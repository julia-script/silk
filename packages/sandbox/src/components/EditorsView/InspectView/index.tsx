import { Tabs, TabsContent, TabsList, TabsTrigger } from "@/components/ui/tabs";
import { genBufId, useEditorStore } from "@/lib/store";
import { useMemo, useState } from "react";
import { ErrorBoundary } from "react-error-boundary";
import { useDocumentState } from "../editorStateManager";
import { AstView } from "./AstView";
import { HirView } from "./HirView";
import { TokenView } from "./TokenView";

export function InspectView({ id }: { id: string }) {
	const store = useEditorStore();
	const buffer = store.buffers[id];
	const [activeTab, setActiveTab] = useState<"ast" | "tokens" | "hir">("ast");

	if (buffer.type !== "inspect") throw new Error("Not an inspect buffer");

	const editorState = useDocumentState(buffer.file);
	const docString = useMemo(() => {
		if (editorState.type !== "editor") return "not an editor";
		return editorState.state.doc.toString();
	}, [editorState]);

	const updateHighlights = (start: number, end: number) => {
		store.updateBufferHighlights(
			genBufId({
				type: "file",
				file: buffer.file,
			}),
			[{ start, end, type: "highlight" }],
		);
	};

	return (
		<Tabs
			value={activeTab}
			onValueChange={(value) => setActiveTab(value as typeof activeTab)}
		>
			<TabsList>
				<TabsTrigger value="tokens">Tokens</TabsTrigger>
				<TabsTrigger value="ast">AST</TabsTrigger>
				<TabsTrigger value="hir">HIR</TabsTrigger>
			</TabsList>
			<ErrorBoundary
				resetKeys={[activeTab, docString]}
				fallbackRender={({ error }) => <div>{error.message}</div>}
			>
				<TabsContent value="tokens">
					<TokenView docString={docString} filePath={buffer.file} />
				</TabsContent>
				<TabsContent value="ast">
					<AstView docString={docString} filePath={buffer.file} />
				</TabsContent>
				<TabsContent value="hir">
					<HirView docString={docString} />
				</TabsContent>
			</ErrorBoundary>
		</Tabs>
	);
}
