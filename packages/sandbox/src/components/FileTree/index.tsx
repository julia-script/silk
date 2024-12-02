"use client";

import { Card, CardContent } from "@/components/ui/card";
import { Suspense } from "react";
import { Directory } from "./Directory";

export const FileTree = () => {
	return (
		<div className="">
			<Suspense fallback={<div className="text-xs">Loading file tree...</div>}>
				<Directory dirPath="./src" />
			</Suspense>
		</div>
	);
};
