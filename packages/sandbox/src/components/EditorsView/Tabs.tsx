import path from "path";
import {
	type Uuid,
	getEditorState,
	useEditorState,
	useEditorStore,
} from "@/lib/store";
import { cn } from "@/lib/utils";
import {
	DndContext,
	DragOverlay,
	KeyboardSensor,
	PointerSensor,
	closestCenter,
	useDraggable,
	useDroppable,
	useSensor,
	useSensors,
} from "@dnd-kit/core";
import { restrictToHorizontalAxis } from "@dnd-kit/modifiers";
import {
	SortableContext,
	arrayMove,
	horizontalListSortingStrategy,
	useSortable,
} from "@dnd-kit/sortable";
import { CSS } from "@dnd-kit/utilities";
import { get } from "lodash";
import {
	CircleX,
	Dot,
	FileCode,
	FileImage,
	FileText,
	Plus,
	X,
} from "lucide-react";
import React from "react";
import {
	type ComponentProps,
	Fragment,
	type PropsWithChildren,
	cloneElement,
	createContext,
	use,
	useContext,
} from "react";
import { useWebContainer } from "../WebContainer";
// interface Buffer {
// 	id: string;
// 	name: string;
// 	icon?: React.ReactNode;
// 	// content: string;
// }

// interface IDETabsProps {
// 	buffers: string[];
// 	activeBuffer: string | null;
// 	onBufferClick: (id: string) => void;
// 	onBufferClose: (id: string) => void;
// 	onBufferAdd: () => void;
// 	onDragEnd: (activeId: string, overId: string) => void;
// }

// type SortableTabProps = {
// 	buffer: string;
// 	isActive: boolean;
// 	onClose: (id: string) => void;
// 	onClick: (id: string) => void;
// };
// const SortableTab = ({
// 	buffer,
// 	isActive,
// 	onClose,
// 	onClick,
// }: SortableTabProps) => {
// 	const {
// 		attributes,
// 		listeners,
// 		setNodeRef,
// 		transform,
// 		transition,
// 		isDragging,
// 	} = useSortable({ id: buffer });

// 	const style = {
// 		transform: CSS.Transform.toString({
// 			x: transform?.x ?? 0,
// 			y: 0,
// 			scaleX: 1,
// 			scaleY: 1,
// 		}),
// 		transition,
// 		zIndex: isDragging ? 100 : "auto",
// 	};
// 	const icon = <FileCode className="w-3 h-3" />;

// 	return (
// 		<div
// 			ref={setNodeRef}
// 			style={style}
// 			{...attributes}
// 			{...listeners}
// 			className={`flex items-center px-2 py-1 text-xs font-medium transition-colors duration-150 ease-in-out border-r border-neutral-700 relative group cursor-move ${
// 				isActive
// 					? "bg-neutral-700 text-white"
// 					: "bg-neutral-950 text-neutral-400 hover:bg-neutral-700 hover:text-white"
// 			} ${isDragging ? "shadow-lg" : ""}`}
// 			onClick={() => onClick(buffer)}
// 		>
// 			{icon}
// 			<span className="ml-1 mr-4">{path.basename(buffer)}</span>
// 			<span className="absolute block right-1 p-0.5 group-hover:opacity-0">
// 				{/* <SavedIcon path={buffer} /> */}
// 			</span>
// 			<button
// 				type="button"
// 				className="absolute right-1 opacity-0 group-hover:opacity-100 transition-opacity duration-150 ease-in-out hover:bg-neutral-500 rounded-full p-0.5"
// 				onClick={(e) => {
// 					e.preventDefault();
// 					e.stopPropagation();
// 					onClose(buffer);
// 				}}
// 			>
// 				<X className="w-2 h-2" />
// 			</button>
// 		</div>
// 	);
// };

// const SavedIcon = ({ path }: { path: Uuid }) => {
// 	// const editorState = useEditorState(path);
// 	// const editorState = store.editorStates[path];
// 	// if (!editorState) return null;
// 	// return !editorState.saved ? <Dot className="size-2 scale-[3]" /> : null;
// };
// export function Tabs({
// 	buffers,
// 	activeBuffer,
// 	onBufferClick,
// 	onBufferClose,
// 	onBufferAdd,
// 	onDragEnd,
// }: IDETabsProps) {
// 	const sensors = useSensors(
// 		useSensor(PointerSensor, {
// 			activationConstraint: {
// 				distance: 2,
// 			},
// 		}),
// 		useSensor(KeyboardSensor),
// 	);

// 	return (
// 		<div className="flex flex-col h-full bg-neutral-950 text-white">
// 			<DndContext
// 				sensors={sensors}
// 				collisionDetection={closestCenter}
// 				modifiers={[restrictToHorizontalAxis]}
// 				onDragStart={(event) => {
// 					console.log("drag start", event);
// 				}}
// 				onDragEnd={(event) => {
// 					console.log("drag end", event);
// 					// if (event.delta.x < 2) onBufferClick(event.active.id as string);
// 					const { active, over } = event;
// 					if (active.id !== over?.id) {
// 						onDragEnd(active.id as string, over?.id as string);
// 					}
// 				}}
// 			>
// 				{/* activationConstraint={{
//       delay: 250,
//       distance: 3,
//       tolerance: 10,
//     }} */}
// 				<div className="flex items-center overflow-x-auto bg-neutral-950 border-b border-neutral-700">
// 					<SortableContext
// 						items={buffers}
// 						strategy={horizontalListSortingStrategy}
// 					>
// 						{buffers.map((buffer) => (
// 							<SortableTab
// 								key={buffer}
// 								buffer={buffer}
// 								isActive={activeBuffer === buffer}
// 								onClose={onBufferClose}
// 								onClick={onBufferClick}
// 							/>
// 						))}
// 					</SortableContext>
// 					<button
// 						type="button"
// 						onClick={onBufferAdd}
// 						className="flex items-center justify-center w-6 h-6 ml-1 text-neutral-400 hover:text-white hover:bg-neutral-700 rounded-full transition-colors duration-150 ease-in-out"
// 					>
// 						<Plus className="w-4 h-4" />
// 					</button>
// 				</div>
// 			</DndContext>
// 		</div>
// 	);
// }

export const TabContext = (props: PropsWithChildren) => {
	const sensors = useSensors(
		useSensor(PointerSensor, {
			activationConstraint: {
				distance: 2,
			},
		}),
		useSensor(KeyboardSensor),
	);
	const store = useEditorStore();

	return (
		<DndContext
			sensors={sensors}
			collisionDetection={closestCenter}
			// modifiers={[restrictToHorizontalAxis]}
			onDragStart={(event) => {
				// console.log("drag start", event);
			}}
			onDragEnd={(event) => {
				const { active, over } = event;
				const fromTabId = active.data.current?.tabId;
				const toTabId = over?.data.current?.tabId;
				const fromBufferId = active.data.current?.id;
				const toIndex = over?.data.current?.id;
				if (
					typeof fromTabId !== "string" ||
					typeof toTabId !== "string" ||
					typeof fromBufferId !== "string" ||
					typeof toIndex !== "number"
				) {
					throw new Error("Invalid drag end event");
				}
				const srcTabView = store.views[fromTabId];
				const dstTabView = store.views[toTabId];
				if (srcTabView?.type !== "tabs" || dstTabView?.type !== "tabs") {
					throw new Error("Invalid drag end event");
				}
				const fromIndex = srcTabView.tabs.indexOf(fromBufferId);
				if (fromIndex === -1) {
					throw new Error("Invalid drag end event");
				}
				console.log({
					fromTabId,
					fromIndex,
					toTabId,
					toIndex,
				});
				if (srcTabView === dstTabView) {
					const newTabs = arrayMove(srcTabView.tabs, fromIndex, toIndex);
					store.updateViews({
						[fromTabId]: {
							...srcTabView,
							tabs: newTabs,
							focusedTab: fromBufferId,
						},
					});
					return;
				}
				store.openFile(fromBufferId, toTabId);
				store.closeFile(fromBufferId, fromTabId);
			}}
		>
			{props.children}
		</DndContext>
	);
};
const TabRowContext = createContext<{ id: string } | null>(null);
export const TabRow = ({
	children,
	id,
	className,
	...props
}: { id: string } & ComponentProps<"div">) => {
	console.log("tabrow", id);
	const childrenArray = React.Children.toArray(children);
	return (
		<TabRowContext.Provider value={{ id }}>
			<div className={cn("flex relative", className)} {...props}>
				{childrenArray.map((child, index) => {
					return (
						<Fragment key={`${id}-${index}`}>
							<DroppableHandle id={index} tabId={id} />
							{child}
							{index === childrenArray.length - 1 && (
								<DroppableHandle id={index + 1} tabId={id} />
							)}
						</Fragment>
					);
				})}
			</div>
		</TabRowContext.Provider>
	);
};

export const Tab = ({
	children,
	id,
	onClick,
	className,
	active,
	...props
}: ComponentProps<"button"> & {
	id: string;
	active?: boolean;
}) => {
	const tabRow = useContext(TabRowContext);
	if (tabRow === null) {
		throw new Error("Tab must be wrapped in a TabRow");
	}

	const draggable = useDraggable({
		id: `[${tabRow.id}]-[${id}]-draggable`,
		data: {
			tabId: tabRow.id,
			id,
		},
	});

	const classname = cn(
		"px-2 py-2 flex items-center gap-1 text-xs cursor-pointer",
		{
			"font-bold text-white": active,
			" text-neutral-400 hover:text-white": !active,
		},
		className,
	);
	return (
		<>
			<div
				type="button"
				className={cn(classname, {
					" outline-1 outline-dotted outline-neutral-200/20":
						draggable.isDragging,
				})}
				ref={draggable.setNodeRef}
				onClick={onClick}
				{...draggable.listeners}
				{...props}
			>
				{children}
			</div>
			<DragOverlay dropAnimation={null}>
				{draggable.isDragging ? (
					<div
						className={cn(
							classname,
							"bg-neutral-950 rounded-lg opacity-50 outline-1 outline-dotted outline-pink-400/50",
						)}
					>
						{children}
					</div>
				) : null}
			</DragOverlay>
		</>
	);
};
const DroppableHandle = ({ id, tabId }: { id: number; tabId: string }) => {
	const droppable = useDroppable({
		id: `${tabId}-${id}-handle`,
		data: {
			tabId,
			id,
		},
	});
	return (
		<div
			ref={droppable.setNodeRef}
			className={cn(
				"w-0.5 rounded self-center relative z-10 transition-[color,height,opacity] duration-300 bg-pink-300",
				{
					"opacity-0 h-0": !droppable.isOver,
					" h-3 opacity-100": droppable.isOver,
				},
			)}
		/>
	);
};
