import { useSilkTools } from "@/lib/silk-toos";
import { useMemo } from "react";
import { GenericNode } from "../AstVisualizer";

interface AstViewProps {
	docString: string;
	onHighlight: (start: number, end: number) => void;
}

export function AstView({ docString, onHighlight }: AstViewProps) {
	const silk = useSilkTools();
	const ast = useMemo(() => silk.parseAst(docString), [docString, silk]);
	const tokens = useMemo(() => silk.lex(docString), [docString, silk]);

	return (
		<GenericNode
			index={0}
			getChildren={(node) => {
				const astNode = ast.nodes[node];
				const children: [string, unknown][] = [];
				const data = astNode.data;
				const tag = String(Object.keys(data)[0]);
				for (const [key, value] of Object.entries(data[tag])) {
					if (key.endsWith("list")) {
						const list = value as number[];
						children.push(
							...list.map((v, i) => [`${key}[${i}]`, v] as [string, number]),
						);
						continue;
					}
					switch (key) {
						case "list":
							break;
						case "token":
							children.push([key, `%${value}`]);
							continue;
						default:
							children.push([key, value as number]);
							break;
					}
				}
				return children;
			}}
			getLabel={(node) => {
				const astNode = ast.nodes[node];
				const data = astNode.data;
				const tag = String(Object.keys(data)[0]);
				return `${tag}: ${astNode.start_token}-${astNode.end_token} #${node}`;
			}}
			onNodeClick={(index) => {
				const node = ast.nodes[index];
				const startToken = tokens[node.start_token];
				const endToken = tokens[node.end_token];
				onHighlight(startToken.start, endToken.end);
			}}
			onNodeHoverEnter={() => {}}
			onNodeHoverLeave={() => {}}
		/>
	);
}
