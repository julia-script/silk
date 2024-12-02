"use client";

import path from "path";
import { useSuspenseQuery } from "@tanstack/react-query";
import { useWebContainer } from "../WebContainer";
import { Entry } from "./Entry";
import { useDirectoryList } from "./useDirectoryList";

export const DirectoryList = ({ dirPath }: { dirPath: string }) => {
	const { container } = useWebContainer();

	const { data } = useDirectoryList({ dirPath });

	return (
		<div className="space-y-0">
			{data.map((entry) => {
				const entryPath = path.join(dirPath, entry.name);
				return (
					<Entry
						key={entryPath}
						entryPath={entryPath}
						isDirectory={entry.isDirectory()}
					/>
				);
			})}
		</div>
	);
};
