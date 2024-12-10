import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { ScrollArea } from "@/components/ui/scroll-area";
import { useSilkTools } from "@/lib/silk-toos";
import { useEditorStore, useHighlights } from "@/lib/store";
import { cn } from "@/lib/utils";
import type {
	AstNode as AstNodeData,
	HirInstruction,
	Token as TokenData,
} from "@lang/lib/dist/types";
import {
	ChevronDown,
	ChevronRight,
	HexagonIcon,
	NetworkIcon,
	PuzzleIcon,
	WorkflowIcon,
} from "lucide-react";
import type React from "react";
import {
	type Dispatch,
	Fragment,
	type PropsWithChildren,
	createContext,
	useContext,
	useEffect,
	useMemo,
	useState,
} from "react";

const NodeContext = createContext<{
	depth: number;
	expanded: boolean;
	setExpanded: Dispatch<React.SetStateAction<boolean>>;
}>({
	depth: 0,
	expanded: false,
	setExpanded: () => {},
});

export function Node({
	onClick,
	onMouseEnter,
	onMouseLeave,
	children,
}: PropsWithChildren<{
	onClick?: () => void;
	onMouseEnter?: () => void;
	onMouseLeave?: () => void;
}>) {
	const context = useContext(NodeContext);
	const [expanded, setExpanded] = useState(context.depth < 10);
	return (
		<NodeContext.Provider
			value={{ depth: context.depth + 1, expanded, setExpanded }}
		>
			<div className="flex flex-col gap-1 [&_svg]:size-4 text-xs">
				{children}
			</div>
		</NodeContext.Provider>
	);
}
Node.Label = function NodeLabel({ children }: PropsWithChildren) {
	const context = useContext(NodeContext);

	return (
		<div
			className="flex items-center cursor-pointer gap-2 border rounded py-1 px-1"
			onClick={() => context.setExpanded(!context.expanded)}
		>
			{context.expanded ? (
				<ChevronDown className="w-4 h-4" />
			) : (
				<ChevronRight className="w-4 h-4" />
			)}
			{children}
		</div>
	);
};
Node.Body = function NodeBody({ children }: PropsWithChildren) {
	const context = useContext(NodeContext);
	if (!context.expanded) return null;
	return <div className={"ml-4 flex flex-col gap-1"}>{children}</div>;
};

export const Tree = ({ children }: PropsWithChildren) => {
	return <div>{children}</div>;
};

export const TreeProvider = ({
	children,
	source,
	file,
}: PropsWithChildren<{
	source: string;
	file: string;
}>) => {
	const [expandedNodes, setExpandedNodes] = useState(new Set<number>());
	const [highlights, setHighlights] = useState<
		{
			type: "token" | "ast-node" | "hir-node";
			index: number;
		}[]
	>([]);
	const silk = useSilkTools();
	const ast = useMemo(() => {
		try {
			return silk.parseAst(source).nodes;
		} catch (e) {
			console.error(e);
			return [];
		}
	}, [source, silk]);
	const tokens = useMemo(() => {
		try {
			return silk.lex(source);
		} catch (e) {
			console.error(e);
			return [];
		}
	}, [source, silk]);

	const hir = useMemo(() => {
		try {
			return silk.parseHir(source).instructions;
		} catch (e) {
			console.error(e);
			return [];
		}
	}, [source, silk]);
	const { update } = useHighlights();
	useEffect(() => {
		update(
			file,
			highlights.map((h) => {
				if (h.type === "token")
					return {
						start: tokens[h.index].start,
						end: tokens[h.index].end,
						type: "highlight",
						className: "ring-yellow-500",
					};
				if (h.type === "ast-node") {
					const startToken = tokens[ast[h.index].start_token];
					const endToken =
						tokens[Math.min(ast[h.index].end_token, tokens.length - 1)];
					return {
						start: startToken.start,
						end: endToken.end,
						type: "highlight",
						className: "ring-pink-500",
					};
				}
				throw new Error("Unknown highlight type");
			}),
		);
	}, [highlights, tokens, update, file, ast]);
	return (
		<TreeContext.Provider
			value={{
				source,
				tokenList: tokens,
				nodes: ast,
				hir,
				expandedNodes,
				setExpandedNodes,
				filePath: file,
				highlights,
				setHighlights,
			}}
		>
			{children}
		</TreeContext.Provider>
	);
};
const TreeContext = createContext<{
	source: string;
	tokenList: TokenData[];
	nodes: AstNodeData[];
	hir: HirInstruction[];
	filePath: string;
	expandedNodes: Set<number>;
	highlights: {
		type: "token" | "ast-node" | "hir-node";
		index: number;
	}[];
	setHighlights: (
		highlights: {
			type: "token" | "ast-node" | "hir-node";
			index: number;
		}[],
	) => void;
	setExpandedNodes: (nodes: Set<number>) => void;
} | null>(null);

const useTreeContext = () => {
	const context = useContext(TreeContext);
	if (!context)
		throw new Error("useTreeContext must be used within a TreeProvider");
	return context;
};
type AstNodeDataData = AstNodeData["data"];
type Union = {
	[K in AstNodeDataData as keyof K]: K[keyof K];
};
type Tag = keyof Union;
type Data = Union[Tag];
export const TreeNode = ({
	node,
	propertyName,
}: {
	node: number;
	propertyName?: React.ReactNode;
}) => {
	const { nodes } = useTreeContext();
	const astNode = nodes[node];
	const tag = Object.keys(astNode.data)[0];
	const data = astNode.data[tag as keyof Data] as Data;
	return (
		<Node>
			<Node.Label>
				<AstNode node={node} propertyName={propertyName} />
			</Node.Label>
			<Node.Body>
				{Object.entries(data).map(([key, value], i) => {
					const isToken = key === "token";
					if (isToken)
						return <Token key={`${i}-${key}`} token={value as number} />;
					if (Array.isArray(value))
						return (
							<Fragment key={`${i}-${key}`}>
								<span>{key}:</span>
								{value.map((v, j) => (
									<TreeNode
										key={`${key}-${j}-${i}-${v}`}
										node={v}
										propertyName={`[${j}]`}
									/>
								))}
							</Fragment>
						);
					if (value === 0 && tag === "root") return null;
					return (
						<TreeNode key={`${i}-${key}`} node={value} propertyName={key} />
					);
				})}
			</Node.Body>
		</Node>
	);
};

const Token = ({ token }: { token: number }) => {
	const { source, tokenList, filePath, setHighlights, highlights } =
		useTreeContext();
	const active = highlights.some(
		(h) => h.type === "token" && h.index === token,
	);
	const tokenData = tokenList[token];

	return (
		<div
			className={cn(
				"flex items-center gap-1 text-yellow-600 grow cursor-pointer px-1",
				active && "text-pink-600",
			)}
			onClick={(e) => {
				e.stopPropagation();
				setHighlights([
					{
						type: "token",
						index: token,
					},
				]);
			}}
		>
			<PuzzleIcon className="size-3 opacity-50" />
			<span className="opacity-50">[{token}]</span>
			<span>.{tokenData.tag}</span>
			<span>'{source.slice(tokenData.start, tokenData.end)}'</span>
			<span className="opacity-50 ml-auto">
				{tokenData.start} - {tokenData.end}
			</span>
		</div>
	);
};

const AstNode = ({
	node,
	propertyName,
}: { node: number; propertyName?: React.ReactNode }) => {
	const { nodes, source, filePath, tokenList, setHighlights, highlights } =
		useTreeContext();
	const active = highlights.some(
		(h) => h.type === "ast-node" && h.index === node,
	);
	const astNode = nodes[node];
	const tag = Object.keys(astNode.data)[0];

	return (
		<div
			className={cn("flex items-center gap-1 grow", active && "text-pink-600")}
			onClick={(e) => {
				e.stopPropagation();
				setHighlights([
					{
						type: "ast-node",
						index: node,
					},
				]);
			}}
		>
			<HexagonIcon className="size-3 opacity-50 text-pink-600" />
			{propertyName && `${propertyName}: `}
			<span className="opacity-50">[{node}]</span>
			<span className="opacity-50">.{tag}</span>
			<span className="opacity-50 ml-auto">
				t#{astNode.start_token} - t#{astNode.end_token}
			</span>
		</div>
	);
};

type FlatHirInst = {
	[K in HirInstruction as string]: K[keyof K] & { tag: keyof K };
}[string];
export const HirNode = ({
	node,
	propertyName,
}: {
	node: number;
	propertyName?: React.ReactNode;
}) => {
	const { hir } = useTreeContext();
	console.log(hir);
	const hirInst = hir[node];
	const tag = Object.keys(hirInst)[0];
	const data = hirInst[tag as keyof HirInstruction] as Record<
		string,
		number | number[] | boolean | null | string
	>;
	// return (
	// 	<Node>
	// 		<Node.Label>
	// 			{/* {propertyName ? `${propertyName}: ` : ""} {tag} */}
	// 			<HirN node={node} propertyName={propertyName} />
	// 		</Node.Label>
	// 		<Node.Body>
	// 			{Object.entries(data).map(([key, value]) => {
	// 				if (value === null) return null;
	// 				if (Array.isArray(value))
	// 					return <HirNode key={key} node={value[0]} propertyName={key} />;
	// 				if (typeof value === "boolean" || typeof value === "string")
	// 					return (
	// 						<div key={key}>
	// 							{key}: {JSON.stringify(value)}
	// 						</div>
	// 					);
	// 				if (key.endsWith("node"))
	// 					return <AstNode key={key} node={value} propertyName={key} />;
	// 				if (key.endsWith("token")) return <Token key={key} token={value} />;
	// 				// return <HirNode key={key} node={value} propertyName={key} />;

	// 				return <HirNode key={key} node={value} propertyName={key} />;
	// 			})}
	// 		</Node.Body>
	// 	</Node>
	// );
};

const HirN = ({
	node,
	propertyName,
}: { node: number; propertyName?: React.ReactNode }) => {
	const { hir, source, filePath, tokenList, setHighlights, highlights } =
		useTreeContext();
	const active = highlights.some(
		(h) => h.type === "ast-node" && h.index === node,
	);
	const hirInstruction = hir[node];
	const tag = Object.keys(hirInstruction)[0];

	return (
		<div
			className={cn("flex items-center gap-1 grow", active && "text-green-600")}
			onClick={(e) => {
				e.stopPropagation();
				// setHighlights([
				// 	{
				// 		type: "ast-node",
				// 		index: node,
				// 	},
				// ]);
			}}
		>
			<HexagonIcon className="size-3 opacity-50 text-green-600" />
			{propertyName && `${propertyName}: `}
			<span className="opacity-50">[{node}]</span>
			<span className="opacity-50">.{tag}</span>
			{/* <span className="opacity-50 ml-auto">
				t#{hirInstruction.start_token} - t#{hirInstruction.end_token}
			</span> */}
		</div>
	);
};
