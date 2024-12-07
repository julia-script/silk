"use client";

import path from "path";
import { Button } from "@/components/ui/button";
import { cn } from "@/lib/utils";
import { ChevronRight, Folder, FolderOpen } from "lucide-react";
import { type PropsWithChildren, useState } from "react";
import { Suspense } from "react";
import { DirectoryList } from "./DirectoryList";

export const FileTreeButton = ({
	collapsed,
	collapsable = false,
	onClick,
	icon,
	active,
	children,
}: PropsWithChildren<{
	collapsed?: boolean;
	onClick: () => void;
	collapsable?: boolean;
	active?: boolean;
	icon?: React.FunctionComponent<React.SVGProps<SVGSVGElement>>;
}>) => {
	const Icon = icon;
	return (
		<div
			role="button"
			tabIndex={0}
			className={cn(
				"flex gap-1 w-full justify-start py-1 px-1 text-xs font-normal m-0 h-auto hover:bg-neutral-200/5",
				"items-center",
				active && "font-bold",
			)}
			onClick={onClick}
		>
			{collapsable ? (
				<ChevronRight
					className={cn("size-1 shrink-0 transition-transform size-3", {
						"transform rotate-90": !collapsed,
					})}
				/>
			) : null}
			{Icon && <Icon className="text-white opacity-50 size-3" />}
			{children}
		</div>
	);
};
