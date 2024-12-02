"use client";

import path from "path";
import { Button } from "@/components/ui/button";
import { cn } from "@/lib/utils";
import { ChevronRight, Folder, FolderOpen } from "lucide-react";
import { type PropsWithChildren, useState } from "react";
import { Suspense } from "react";
import { DirectoryList } from "./DirectoryList";
import { FileTreeButton } from "./FileTreeButton";
import { useDirectoryList } from "./useDirectoryList";
export const Directory = ({ dirPath }: { dirPath: string }) => {
	const [collapsed, setCollapsed] = useState(true);
	const name = path.basename(dirPath);
	// Prefetch the directory list
	useDirectoryList({ dirPath });

	return (
		<div className="">
			<FileTreeButton
				collapsable={true}
				collapsed={collapsed}
				onClick={() => setCollapsed(!collapsed)}
				icon={collapsed ? Folder : FolderOpen}
			>
				{name}
			</FileTreeButton>
			{!collapsed && (
				<div className="pl-2 border-l border-neutral-200/5 ml-3">
					<Suspense
						fallback={
							<div className="text-xs text-muted-foreground">Loading...</div>
						}
					>
						<DirectoryList dirPath={dirPath} />
					</Suspense>
				</div>
			)}
		</div>
	);
};
