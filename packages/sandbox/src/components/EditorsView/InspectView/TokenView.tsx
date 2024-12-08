import { useSilkTools } from "@/lib/silk-toos";
import type { Highlight } from "@/lib/store";
import { useMemo } from "react";
import { TokenVisualizer } from "../TokenVisualizer";

interface TokenViewProps {
	docString: string;
	onHighlight: (highlights: Highlight[]) => void;
}

export function TokenView({ docString, onHighlight }: TokenViewProps) {
	const silk = useSilkTools();
	const tokens = useMemo(() => silk.lex(docString), [docString, silk]);

	return (
		<TokenVisualizer
			tokens={tokens}
			onSelectionUpdate={(tokens) => {
				onHighlight(
					tokens.map((token) => ({
						start: token.start,
						end: token.end,
						type: "highlight",
					})),
				);
			}}
		/>
	);
}
