"use client";
import { cn } from "@/lib/utils";
import * as ResizablePrimitive from "react-resizable-panels";

const ResizablePanelGroup = ({
	className,
	...props
}: React.ComponentProps<typeof ResizablePrimitive.PanelGroup>) => (
	<ResizablePrimitive.PanelGroup
		className={cn(
			"flex h-full w-full data-[panel-group-direction=vertical]:flex-col",
			className,
		)}
		{...props}
	/>
);
