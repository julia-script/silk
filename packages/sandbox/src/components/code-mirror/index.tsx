"use client";
import type { BufferHighlight } from "@/lib/store";
import { cn } from "@/lib/utils";
import { defaultKeymap } from "@codemirror/commands";
import { syntaxHighlighting } from "@codemirror/language";
import { HighlightStyle } from "@codemirror/language";
import { type EditorState, StateEffect, StateField } from "@codemirror/state";
import { Decoration, type ViewUpdate } from "@codemirror/view";
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
import { updateMarks } from "./marks";

type CodeMirrorProps = {
	value?: string;
	onViewUpdate?: (viewUpdate: ViewUpdate) => void;
	focusOnMount?: boolean;
	// language: LRLanguage;
	editorState: EditorState;
	highlights?: BufferHighlight[];
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
	focusOnMount = false,
	highlights,
	className,
	...props
}: CodeMirrorProps & ComponentProps<"div">) => {
	const ref = useRef<HTMLDivElement>(null);

	const [editorView, setEditorView] = useState<EditorView | null>(null);

	useEffect(() => {
		if (!ref.current) return;

		const view = new EditorView({
			parent: ref.current,
			extensions: [],
		});
		if (focusOnMount) {
			view.focus();
		}

		setEditorView(view);
		return () => {
			view.destroy();
			setEditorView(null);
		};
	}, [focusOnMount]);

	useEffect(() => {
		if (!editorView) return;
		if (editorView.state !== editorState) {
			editorView.setState(editorState);
		}
	}, [editorState, editorView]);

	useEffect(() => {
		if (!editorView) return;
		if (!highlights) return;
		console.log("updating marks", highlights);
		editorView.dispatch({
			effects: updateMarks.of(highlights),
		});
	}, [highlights, editorView]);

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
