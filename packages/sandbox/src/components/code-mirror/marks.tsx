import type { BufferHighlight } from "@/lib/store";
import { cn } from "@/lib/utils";
import { StateEffect, StateField } from "@codemirror/state";
import { Decoration } from "@codemirror/view";
import { EditorView } from "codemirror";

export const updateMarks = StateEffect.define<BufferHighlight[]>();

// This value must be added to the set of extensions to enable this
export const markField = StateField.define({
	// Start with an empty set of decorations
	create() {
		return Decoration.none;
	},
	// This is called whenever the editor updates—it computes the new set
	update(value, tr) {
		value = value.map(tr.changes);
		for (const effect of tr.effects) {
			if (effect.is(updateMarks)) {
				value = Decoration.set(
					effect.value.map((h) =>
						getHighlightMark(h.className).range(h.start, h.end),
					),
				);
			}
		}
		return value;
	},
	// Indicate that this field provides a set of decorations
	provide: (f) => EditorView.decorations.from(f),
});

const getHighlightMark = (className: string) =>
	Decoration.mark({
		attributes: {
			class: cn(" ring-pink-500 rounded-[2px]", className),
			style: "outline: solid 1px var(--tw-ring-color);outline-offset: 2px",
		},
	});
const highlightMap: Record<BufferHighlight["type"], Decoration> = {
	highlight: Decoration.mark({
		attributes: {
			class: " ring-pink-500 rounded-[2px]",
			style: "outline: solid 1px var(--tw-ring-color);outline-offset: 2px",
		},
	}),
};
