import { useSilkTools } from "@/lib/silk-toos";
import { useHighlights } from "@/lib/store";
import type {
	AstNode as AstNodeData,
	Token as TokenData,
} from "@lang/lib/dist/types";
import { Component, PuzzleIcon } from "lucide-react";
import {
	Fragment,
	type PropsWithChildren,
	createContext,
	useContext,
	useMemo,
	useState,
} from "react";
import { HirNode, TreeNode, TreeProvider } from "../AstVisualizer";

interface AstViewProps {
	docString: string;
	filePath: string;
}

export function AstView({ docString, filePath }: AstViewProps) {
	return (
		<TreeProvider source={docString} file={filePath}>
			<TreeNode node={0} />
			{/* <HirNode node={0} /> */}
		</TreeProvider>
	);
	// return (
	// 	<GenericNode
	// 		node={0}
	// 		getChildren={(propLabel, node) => {
	// 			const astNode = ast.nodes[node];
	// 			const tag = String(
	// 				Object.keys(astNode.data)[0],
	// 			) as keyof typeof astNode.data;
	// 			const data = astNode.data[tag];
	// 			console.log(data);
	// 			return [];
	// 			// return Object.entries(data).flatMap(([key, value]) => {
	// 			// 	// if (propLabel)
	// 			// 	if (key.endsWith("list")) {
	// 			// 		const list = value as number[];
	// 			// 		return list.map((v, i) => [`${key}[${i}]`, v] as [string, number]);
	// 			// 	}
	// 			// 	return [[key, value]] as const;
	// 			// });
	// 			// const children: [string, unknown][] = [];
	// 			// const data = astNode.data;
	// 			// for (const [key, value] of Object.entries(data[tag])) {
	// 			// 	if (key.endsWith("list")) {
	// 			// 		const list = value as number[];
	// 			// 		children.push(
	// 			// 			...list.map((v, i) => [`${key}[${i}]`, v] as [string, number]),
	// 			// 		);
	// 			// 		continue;
	// 			// 	}
	// 			// 	switch (key) {
	// 			// 		case "list":
	// 			// 			break;
	// 			// 		case "token":
	// 			// 			children.push([key, `%${value}`]);
	// 			// 			continue;
	// 			// 		default:
	// 			// 			children.push([key, value as number]);
	// 			// 			break;
	// 			// 	}
	// 			// }
	// 			// return children;
	// 		}}
	// 		getLabel={(propLabel, node) => {
	// 			const astNode = ast.nodes[node];
	// 			const data = astNode.data;
	// 			const tag = String(Object.keys(data)[0]);
	// 			return `${tag}: ${astNode.start_token}-${astNode.end_token} #${node}`;
	// 		}}
	// 		onNodeClick={(index) => {
	// 			// const node = ast.nodes[index];
	// 			// const startToken = tokens[node.start_token];
	// 			// const endToken = tokens[node.end_token];
	// 			// update(filePath, [
	// 			// 	{
	// 			// 		start: startToken.start,
	// 			// 		end: endToken.end,
	// 			// 		type: "highlight",
	// 			// 	},
	// 			// ]);
	// 		}}
	// 		onNodeHoverEnter={() => {}}
	// 		onNodeHoverLeave={() => {}}
	// 	/>
	// );
}

// type AstNodeDataData = AstNodeData["data"];
// type Union = {
// 	[K in AstNodeDataData as keyof K]: K[keyof K];
// };
// type Tag = keyof Union;
// type Data = Union[Tag];

// const TreeProvider = ({
// 	children,
// 	source,
// }: PropsWithChildren<{
// 	source: string;
// 	file: string;
// }>) => {
// 	const [expandedNodes, setExpandedNodes] = useState(new Set<number>());
// 	const silk = useSilkTools();
// 	const ast = useMemo(() => {
// 		try {
// 			return silk.parseAst(source).nodes;
// 		} catch (e) {
// 			console.error(e);
// 			return [];
// 		}
// 	}, [source, silk]);
// 	const tokens = useMemo(() => {
// 		try {
// 			return silk.lex(source);
// 		} catch (e) {
// 			console.error(e);
// 			return [];
// 		}
// 	}, [source, silk]);
// 	const { update } = useHighlights();
// 	return (
// 		<TreeContext.Provider
// 			value={{
// 				source,
// 				tokenList: tokens,
// 				nodes: ast,
// 				expandedNodes,
// 				setExpandedNodes,
// 			}}
// 		>
// 			{children}
// 		</TreeContext.Provider>
// 	);
// };
// const TreeContext = createContext<{
// 	source: string;
// 	tokenList: TokenData[];
// 	nodes: AstNodeData[];
// 	expandedNodes: Set<number>;
// 	setExpandedNodes: (nodes: Set<number>) => void;
// }>({
// 	source: "",
// 	tokenList: [],
// 	nodes: [],
// 	expandedNodes: new Set(),
// 	setExpandedNodes: () => {},
// });
// const useTreeContext = () => useContext(TreeContext);
// const AstNode = ({
// 	node,
// 	// items,
// 	// tokenList,
// 	propertyName,
// 	// source,
// }: {
// 	node: number;
// 	// items: AstNodeData[];
// 	// tokenList: TokenData[];
// 	propertyName?: React.ReactNode;
// 	// source: string;
// }) => {
// 	const { tokenList, nodes } = useTreeContext();
// 	const astNode = nodes[node];
// 	const tag = Object.keys(astNode.data)[0];
// 	const data = astNode.data[tag as keyof Data] as Data;
// 	return (
// 		<Node>
// 			<Node.Label>
// 				{propertyName && `${propertyName}: `}
// 				{tag}
// 			</Node.Label>
// 			<Node.Body>
// 				{Object.entries(data).map(([key, value], i) => {
// 					const isToken = key === "token";
// 					if (isToken)
// 						return <Token key={`${i}-${key}`} token={value as number} />;
// 					if (Array.isArray(value))
// 						return (
// 							<Fragment key={`${i}-${key}`}>
// 								<span>{key}:</span>
// 								{value.map((v, j) => (
// 									<AstNode
// 										key={`${key}-${j}-${i}-${v}`}
// 										node={v}
// 										propertyName={`[${j}]`}
// 									/>
// 								))}
// 							</Fragment>
// 						);
// 					if (value === 0 && tag === "root") return null;
// 					return (
// 						<AstNode key={`${i}-${key}`} node={value} propertyName={key} />
// 					);
// 					// return (
// 					// 	<Fragment key={`${i}-${key}`}>
// 					// 		{/* <div>{key}:</div>
// 					// 		{Array.isArray(value) && (
// 					// 			<div>
// 					// 				{value.map((v, i) => (
// 					// 					<AstNode
// 					// 						key={`${i}-${v}`}
// 					// 						node={v}
// 					// 						items={items}
// 					// 						tokenList={tokenList}
// 					// 					/>
// 					// 				))}
// 					// 			</div>
// 					// 		)} */}
// 					// 		{typeof value === "number" &&
// 					// 			(value !== 0 || tag === "root") &&
// 					// 			!isToken && (
// 					// 				<AstNode node={value} items={items} tokenList={tokenList} />
// 					// 			)}
// 					// 	</Fragment>
// 					// );
// 				})}
// 			</Node.Body>
// 		</Node>
// 	);
// };

// const Token = ({ token }: { token: number }) => {
// 	const { source, tokenList } = useTreeContext();
// 	const tokenData = tokenList[token];

// 	return (
// 		<div className="flex items-center gap-1">
// 			<PuzzleIcon className="size-3 opacity-50" />
// 			<span>
// 				[{`%${token}`}] '{source.slice(tokenData.start, tokenData.end)}'
// 			</span>
// 		</div>
// 	);
// };
