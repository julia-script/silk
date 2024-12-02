import { useSuspenseQuery } from "@tanstack/react-query";
import { useWebContainer } from "../WebContainer";

export const useDirectoryList = ({ dirPath }: { dirPath: string }) => {
	const { container } = useWebContainer();
	return useSuspenseQuery({
		queryKey: ["fileTree", dirPath],

		queryFn: async () => {
			const fs = container.fs;
			const tree = await fs.readdir(dirPath, { withFileTypes: true });
			return tree.sort((a, b) => {
				if (a.isDirectory() && !b.isDirectory()) return -1;
				if (!a.isDirectory() && b.isDirectory()) return 1;
				if (a.name === "main.slk") return -1;
				if (b.name.toLocaleLowerCase() < a.name.toLocaleLowerCase()) return 1;
				return 0;
			});
		},
	});
};
