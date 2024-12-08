import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { ScrollArea } from "@/components/ui/scroll-area";
import { cn } from "@/lib/utils";
import type { Token } from "@lang/lib/dist/types";
import type React from "react";
import { type ComponentProps, useEffect, useState } from "react";

interface TokenVisualizerProps {
	tokens: Token[];

	onSelectionUpdate: (tokens: Token[]) => void;
}

const getTokenId = (token: Token, index: number) => `${token.tag}-${index}`;
export const TokenVisualizer: React.FC<TokenVisualizerProps> = ({
	tokens,

	onSelectionUpdate,
}) => {
	const [selectedToken, setSelectedToken] = useState<number | null>(null);
	const [hoveredToken, setHoveredToken] = useState<number | null>(null);
	useEffect(() => {
		setSelectedToken(null);
	}, [tokens]);
	useEffect(() => {
		const selectedTokens: Token[] = [];
		if (selectedToken !== null) {
			selectedTokens.push(tokens[selectedToken]);
		}
		if (hoveredToken !== null && hoveredToken !== selectedToken) {
			selectedTokens.push(tokens[hoveredToken]);
		}
		onSelectionUpdate(selectedTokens.sort((a, b) => a.start - b.start));
	}, [selectedToken, hoveredToken]);

	return (
		<div className="w-full max-w-3xl mx-auto py-4 px-2">
			<h2 className="text-lg font-semibold mb-2">Tokens</h2>
			<ScrollArea className="h-[500px] w-full">
				<div className="flex flex-wrap gap-1">
					{tokens.map((token, index) => {
						const tokenId = getTokenId(token, index);
						const isSelected = selectedToken === index;
						return (
							<TokenComponent
								index={index}
								className={cn({
									"ring-1 ring-pink-500": isSelected,
								})}
								key={tokenId}
								token={token}
								onClick={() => {
									setSelectedToken(isSelected ? null : index);
								}}
								onMouseEnter={() => {
									if (isSelected) return;
									setHoveredToken(index);
								}}
								onMouseLeave={() => {
									// if (!isSelected) return;
									setHoveredToken(null);
								}}
							/>
						);
					})}
				</div>
			</ScrollArea>
		</div>
	);
};

interface TokenProps {
	index: number;
	token: Token;
}

export const TokenComponent = ({
	token,
	index,
	className,
	...props
}: ComponentProps<"button"> & TokenProps) => {
	return (
		<button
			className={cn(
				"text-xs inline-block px-2 py-1 m-1 rounded cursor-pointer font-mono transition-colors",
				className,
			)}
			type="button"
			disabled={token.end - token.start <= 0}
			style={{
				backgroundColor: getBackgroundColor(token.tag),
				color: getTextColor(token.tag),
			}}
			title={`${token.tag} (${token.start}-${token.end})`}
			{...props}
		>
			[{index}] {token.tag} ({token.start}-{token.end})
		</button>
	);
};

function getBackgroundColor(tag: string): string {
	if (tag.startsWith("keyword_")) return "hsl(var(--primary))";
	if (tag === "identifier") return "hsl(var(--secondary))";
	if (tag.includes("literal")) return "hsl(var(--accent))";
	return "hsl(var(--muted))";
}

function getTextColor(tag: string): string {
	if (tag.startsWith("keyword_")) return "hsl(var(--primary-foreground))";
	if (tag === "identifier") return "hsl(var(--secondary-foreground))";
	if (tag.includes("literal")) return "hsl(var(--accent-foreground))";
	return "hsl(var(--muted-foreground))";
}
