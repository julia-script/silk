import { vscodeDark } from "@uiw/codemirror-theme-vscode";
import { EditorState } from "@codemirror/state";
import { langLanguage } from "@lang/grammar/dist/language";
import { EditorView, keymap, type ViewUpdate } from "@codemirror/view";
import { basicSetup } from "codemirror";
import { Command } from "lucide-react";

export const createEditorState = ({
	initialValue,
	onViewUpdate,
	onSave,
}: {
	initialValue: string;
	onViewUpdate?: (update: ViewUpdate) => void;
	onSave?: () => void;
}) => {
	return EditorState.create({
		doc: initialValue,

		extensions: [
			vscodeDark,
			basicSetup,
			langLanguage,
			keymap.of([
				{
					key: "Cmd-s",
					stopPropagation: true,
					run: (e) => {
						onSave?.();
						return true;
					},
				},
				{
					key: "Ctrl-s",
					stopPropagation: true,
					run: (e) => {
						onSave?.();
						return true;
					},
				},
			]),
			EditorView.updateListener.of((update) => {
				onViewUpdate?.(update);
			}),
		],
	});
};
