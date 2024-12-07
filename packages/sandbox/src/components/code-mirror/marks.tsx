import type { BufferHighlight } from "@/lib/store";
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
					effect.value.map((h) => highlightMap[h.type].range(h.start, h.end)),
				);
			}
		}
		console.log("updating mark value", value);
		return value;
	},
	// Indicate that this field provides a set of decorations
	provide: (f) => EditorView.decorations.from(f),
});

const highlightMap: Record<BufferHighlight["type"], Decoration> = {
	highlight: Decoration.mark({
		attributes: {
			class: " ring-pink-500 rounded-[2px]",
			style: "outline: solid 1px var(--tw-ring-color);outline-offset: 2px",
		},
	}),
};
