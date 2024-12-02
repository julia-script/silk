"use client";
import { cn } from "@/lib/utils";
import { defaultKeymap } from "@codemirror/commands";
import { syntaxHighlighting } from "@codemirror/language";
import { HighlightStyle } from "@codemirror/language";
import type { EditorState } from "@codemirror/state";
import type { ViewUpdate } from "@codemirror/view";
import { langLanguage, parserWithMetadata } from "@lang/grammar/dist/language";
import { tags as t } from "@lezer/highlight";
import { useMutation } from "@tanstack/react-query";
// import { parser } from "@lang/grammar/dist/parser";
import {
	defaultSettingsVscodeDark,
	vscodeDark,
} from "@uiw/codemirror-theme-vscode";
import { EditorView, basicSetup } from "codemirror";
import {
	type ComponentProps,
	experimental_useEffectEvent,
	useEffect,
	useRef,
	useState,
} from "react";
// import "@codemirror/themes";
import { ayuLight } from "thememirror";

type CodeMirrorProps = {
	value?: string;
	onViewUpdate?: (viewUpdate: ViewUpdate) => void;
	// language: LRLanguage;
	editorState: EditorState;
};
const myHighlightStyle = HighlightStyle.define([
	{ tag: t.typeName, class: "text-pink-500" },
	{ tag: t.keyword, color: "pink" },
	{ tag: t.comment, color: "#f5d", fontStyle: "italic" },
	{ tag: t.function(t.name), class: "text-blue-500" },
]);
const theme = syntaxHighlighting(myHighlightStyle);
export const CodeMirror = ({
	editorState,
	className,
	...props
}: CodeMirrorProps & ComponentProps<"div">) => {
	const ref = useRef<HTMLDivElement>(null);

	const [editorView, setEditorView] = useState<EditorView | null>(null);

	useEffect(() => {
		if (!ref.current) return;

		const view = new EditorView({
			// state: editorState,

			parent: ref.current,
		});

		setEditorView(view);
		return () => {
			view.destroy();
		};
	}, []);

	useEffect(() => {
		if (!editorView) return;
		if (editorView.state !== editorState) {
			editorView.setState(editorState);
		}
	}, [editorState, editorView]);

	return (
		<div
			ref={ref}
			className={cn(
				"w-full h-full relative flex flex-col grow overflow-hidden [&>.cm-editor]:overflow-hidden",
				className,
			)}
			{...props}
		/>
	);
};
