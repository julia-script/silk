import { useSilkTools } from "@/lib/silk-toos";
import { type Highlight, useHighlights } from "@/lib/store";
import { useMemo } from "react";
import { TokenVisualizer } from "../TokenVisualizer";

interface TokenViewProps {
	docString: string;
	filePath: string;
}

export function TokenView({ docString, filePath }: TokenViewProps) {
	const silk = useSilkTools();
	const tokens = useMemo(() => silk.lex(docString), [docString, silk]);
	const { update } = useHighlights();

	return (
		<TokenVisualizer
			tokens={tokens}
			onSelectionUpdate={(tokens) => {
				update(
					filePath,
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
