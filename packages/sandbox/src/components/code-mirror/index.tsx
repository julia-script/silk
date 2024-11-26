"use client";
import { defaultKeymap } from "@codemirror/commands";
import { syntaxHighlighting } from "@codemirror/language";
import { HighlightStyle } from "@codemirror/language";
import { EditorState } from "@codemirror/state";
import { langLanguage, parserWithMetadata } from "@lang/grammar/dist/language";
import { tags as t } from "@lezer/highlight";
// import { parser } from "@lang/grammar/dist/parser";
import {
	defaultSettingsVscodeDark,
	vscodeDark,
} from "@uiw/codemirror-theme-vscode";
import { EditorView, basicSetup } from "codemirror";
import { experimental_useEffectEvent, useEffect, useRef } from "react";
// import "@codemirror/themes";
import { ayuLight } from "thememirror";

type CodeMirrorProps = {
	defaultState?: string;
	// language: LRLanguage;
};
const myHighlightStyle = HighlightStyle.define([
	{ tag: t.typeName, class: "text-pink-500" },
	{ tag: t.keyword, color: "pink" },
	{ tag: t.comment, color: "#f5d", fontStyle: "italic" },
	{ tag: t.function(t.name), class: "text-blue-500" },
]);
const theme = syntaxHighlighting(myHighlightStyle);
export const CodeMirror = ({ defaultState }: CodeMirrorProps) => {
	const ref = useRef<HTMLDivElement>(null);

	// const intialize = experimental_useEffectEvent(() => {
	// 	if (!ref.current) return;
	// 	// if (ref.current) {
	// 	//   view = new EditorView({ state: startState, parent: ref.current });
	// 	// }
	// 	const startState = EditorState.create({
	// 		doc: "Hello World",
	// 		extensions: [keymap.of(defaultKeymap)],
	// 	});

	// 	const view = new EditorView({
	// 		state: startState,
	// 		parent: document.body,
	// 	});
	// });

	useEffect(() => {
		if (!ref.current) return;
		const startState = EditorState.create({
			doc: defaultState ?? "Hello World",
			extensions: [vscodeDark, basicSetup, langLanguage],
		});

		const view = new EditorView({
			state: startState,
			parent: ref.current,
		});
	}, [defaultState]);
	return <div ref={ref} className="h-full w-full" />;
};
