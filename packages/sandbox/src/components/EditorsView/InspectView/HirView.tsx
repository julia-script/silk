import { useSilkTools } from "@/lib/silk-toos";
import { useMemo } from "react";

interface HirViewProps {
	docString: string;
}

export function HirView({ docString }: HirViewProps) {
	const silk = useSilkTools();
	const hir = useMemo(() => silk.parseHir(docString), [docString, silk]);
	return null;

	// return (
	// <GenericNode
	// 	index={0}
	// 	getChildren={(node) => {
	// 		const hirInst = hir.instructions[node];
	// 		const children: [string, unknown][] = [];
	// 		const tag = String(Object.keys(hirInst)[0]);

	// 		for (const [key, value] of Object.entries(hirInst[tag])) {
	// 			if (key.endsWith("_list")) {
	// 				const list = value as number[];
	// 				children.push(
	// 					...list.map((v, i) => [`${key}[${i}]`, v] as [string, number]),
	// 				);
	// 			} else if (
	// 				key.endsWith("_node") ||
	// 				key.startsWith("ty_") ||
	// 				key.endsWith("literal") ||
	// 				key === "identifier"
	// 			) {
	// 				if (typeof value === "number") {
	// 					children.push([key, `#${value}`]);
	// 				} else {
	// 					children.push([key, value]);
	// 				}
	// 			} else if (typeof value === "number") {
	// 				children.push([key, value as number]);
	// 			}
	// 		}

	// 		return children;
	// 	}}
	// 	getLabel={(node) => {
	// 		const hirInst = hir.instructions[node];
	// 		const tag = String(Object.keys(hirInst)[0]);
	// 		return `${tag} #${node}`;
	// 	}}
	// 	onNodeClick={() => {}}
	// 	onNodeHoverEnter={() => {}}
	// 	onNodeHoverLeave={() => {}}
	// />
	// );
}
