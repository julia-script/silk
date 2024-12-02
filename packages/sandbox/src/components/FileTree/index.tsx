"use client";

import { Card, CardContent } from "@/components/ui/card";
import dynamic from "next/dynamic";
import { Suspense } from "react";

const Directory = dynamic(
	() => import("./Directory").then((mod) => mod.Directory),
	{
		ssr: false,
	},
);
export const FileTree = () => {
	return (
		<div className="">
			<Suspense fallback={<div className="text-xs">Loading file tree...</div>}>
				<Directory dirPath="./src" startOpen={true} />
			</Suspense>
		</div>
	);
};
