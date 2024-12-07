import { Card, CardContent, CardHeader, CardTitle } from "@/components/ui/card";
import { ScrollArea } from "@/components/ui/scroll-area";
import type { AstNode } from "@lang/lib/dist/types";
import { ChevronDown, ChevronRight } from "lucide-react";
import type React from "react";
import { useEffect, useState } from "react";

interface AstVisualizerProps {
	ast: AstNode[];
	onNodeClick: (node: AstNode) => void;
	onNodeHoverEnter: (node: AstNode) => void;
	onNodeHoverLeave: () => void;
}

export const AstVisualizer: React.FC<AstVisualizerProps> = ({
	ast,
	onNodeClick,
	onNodeHoverEnter,
	onNodeHoverLeave,
}) => {
	return (
		<Node
			nodeIndex={0}
			allNodes={ast}
			depth={0}
			id="root"
			onNodeClick={onNodeClick}
			onNodeHoverEnter={onNodeHoverEnter}
			onNodeHoverLeave={onNodeHoverLeave}
		/>
	);
};

interface AstNodeProps {
	nodeIndex: number;
	allNodes: AstNode[];
	depth: number;
	id: string;
	onNodeClick: (node: AstNode) => void;
	onNodeHoverEnter: (node: AstNode) => void;
	onNodeHoverLeave: () => void;
	propName?: string;
}

export const Node: React.FC<AstNodeProps> = ({
	nodeIndex,
	allNodes,
	depth,
	id,
	onNodeClick,
	propName,
	onNodeHoverEnter,
	onNodeHoverLeave,
}) => {
	const node = allNodes[nodeIndex];
	const [isExpanded, setIsExpanded] = useState(depth < 4);
	const nodeType = getNodeType(node);
	const nodeLabel = getNodeLabel(node);

	const renderChildren = () => {
		const children: React.ReactNode[] = [];
		const nodeData = node.data[nodeType];

		for (const [key, value] of Object.entries(nodeData)) {
			if (key === "token") continue;
			if (Array.isArray(value)) {
				for (const [index, childIndex] of value.entries()) {
					if (childIndex !== 0) {
						children.push(
							<Node
								key={`${key}-${index}`}
								id={`${key}-${index}`}
								nodeIndex={childIndex}
								allNodes={allNodes}
								depth={depth + 1}
								onNodeClick={onNodeClick}
								onNodeHoverEnter={onNodeHoverEnter}
								onNodeHoverLeave={onNodeHoverLeave}
								propName={key}
							/>,
						);
					}
				}
			} else if (typeof value === "number" && value !== 0) {
				children.push(
					<Node
						key={key}
						nodeIndex={value}
						allNodes={allNodes}
						depth={depth + 1}
						id={`${id}-${key}`}
						onNodeClick={onNodeClick}
						onNodeHoverEnter={onNodeHoverEnter}
						onNodeHoverLeave={onNodeHoverLeave}
						propName={key}
					/>,
				);
			}
		}

		return children;
	};

	const hasChildren = renderChildren().length > 0;

	return (
		<div className="" id={id}>
			<div
				className="flex items-center cursor-pointer"
				onClick={() => setIsExpanded(!isExpanded)}
			>
				{hasChildren ? (
					isExpanded ? (
						<ChevronDown className="w-4 h-4 mr-1" />
					) : (
						<ChevronRight className="w-4 h-4 mr-1" />
					)
				) : (
					<span className="w-4 h-4 mr-1" />
				)}
				<span
					className="font-mono text-xs"
					onClick={(e) => {
						e.preventDefault();
						e.stopPropagation();
						onNodeClick(node);
					}}
					onMouseEnter={() => onNodeHoverEnter(node)}
					onMouseLeave={() => {
						onNodeHoverLeave();
					}}
				>
					{`#${nodeIndex} `}
					{propName ? `${propName}: ` : ""}
					{nodeLabel} ({node.start_token}-{node.end_token})
				</span>
			</div>
			{isExpanded && hasChildren && (
				<div className="ml-4">{renderChildren()}</div>
			)}
		</div>
	);
};

export function getNodeType(node: AstNode): string {
	const keys = Object.keys(node.data);
	if (keys.length === 0) return "unknown";

	return Object.keys(node.data)[0];
}

export function getNodeLabel(node: AstNode): string {
	const type = getNodeType(node);
	switch (type) {
		case "root":
		case "expr":
		case "block":
			return `${type} (${node.data[type].list.length} items)`;
		case "identifier":
		case "number_literal":
		case "string_literal":
		case "char_literal":
		case "true_literal":
		case "false_literal":
			return `${type} (token: ${(node.data)[type].token})`;
		case "fn_proto":
			return `${type} (${(node.data)[type].name})`;
		default:
			return type;
	}
}
export const GenericNode = ({
	index,
	getChildren,
	getLabel,
	depth = 0,
	onNodeClick,
	onNodeHoverEnter,
	onNodeHoverLeave,
	propLabel,
}: {
	index: number;
	getChildren: (node: number) => [string, unknown][];
	getLabel: (node: number) => React.ReactNode;
	depth?: number;
	onNodeClick: (node: number) => void;
	onNodeHoverEnter: (node: number) => void;
	onNodeHoverLeave: () => void;
	propLabel?: string;
}) => {
	const children = getChildren(index);
	const hasChildren = !!children.length;
	const [isExpanded, setIsExpanded] = useState(depth < 10);

	return (
		<div>
			<div
				className="flex items-center cursor-pointer"
				onClick={() => setIsExpanded(!isExpanded)}
			>
				{hasChildren ? (
					isExpanded ? (
						<ChevronDown className="w-4 h-4 mr-1" />
					) : (
						<ChevronRight className="w-4 h-4 mr-1" />
					)
				) : (
					<span className="w-4 h-4 mr-1" />
				)}
				<span
					className="font-mono text-xs"
					onClick={(e) => {
						e.preventDefault();
						e.stopPropagation();
						onNodeClick(index);
					}}
					onMouseEnter={() => onNodeHoverEnter(index)}
					onMouseLeave={() => {
						onNodeHoverLeave();
					}}
				>
					{propLabel ? `${propLabel}: ` : ""}
					{getLabel(index)}
				</span>
			</div>
			{isExpanded && hasChildren && (
				<div className="ml-4">
					{children.map((child, index) => {
						const [label, childIndex] = child;
						if (typeof childIndex !== "number") {
							const className = "text-xs ml-1";
							switch (typeof childIndex) {
								case "string":
									return (
										<span key={index} className={className}>
											{label}: {childIndex}{" "}
										</span>
									);
								case "boolean":
									return (
										<span key={index} className={className}>
											{label}: {childIndex ? "true" : "false"}
										</span>
									);
								case "object":
									return (
										<span key={index} className={className}>
											{label}: {JSON.stringify(childIndex)}
										</span>
									);
								default:
									return (
										<span key={index} className={className}>
											{label}
										</span>
									);
							}
						}
						return (
							<GenericNode
								key={index}
								index={childIndex}
								getChildren={getChildren}
								depth={depth + 1}
								onNodeClick={onNodeClick}
								onNodeHoverEnter={onNodeHoverEnter}
								onNodeHoverLeave={onNodeHoverLeave}
								getLabel={getLabel}
								propLabel={label}
							/>
						);
					})}
				</div>
			)}
		</div>
	);
};
