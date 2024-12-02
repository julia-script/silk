"use client";

import { inspect } from "util";
import type { Uuid, View } from "@/lib/store";

export type Panel = {
	id: Uuid;
	size: Size;
	splitType: "vertical" | "horizontal";
	children: Panel[];
};

export type Size = { width: number; height: number };

export const toAbsoluteSize = (size: Size, parentSize: Size) => {
	return {
		width: size.width * parentSize.width,
		height: size.height * parentSize.height,
	};
};

export const toRelativeSize = (size: Size, parentSize: Size) => {
	return {
		width: size.width / parentSize.width,
		height: size.height / parentSize.height,
	};
};
export const getMainSize = (size: Size, type: "vertical" | "horizontal") => {
	if (type === "vertical") {
		return size.width;
	}
	return size.height;
};
const toMainSize = (size: number, type: "vertical" | "horizontal") => {
	if (type === "vertical") {
		return { width: size, height: 1 };
	}
	return { width: 1, height: size };
};

export const max = (a: Size, b: Size) => {
	return {
		width: Math.max(a.width, b.width),
		height: Math.max(a.height, b.height),
	};
};
export const min = (a: Size, b: Size) => {
	return {
		width: Math.min(a.width, b.width),
		height: Math.min(a.height, b.height),
	};
};
export const clamp = (size: Size, minSize: Size, maxSize: Size) => {
	return max(min(size, maxSize), minSize);
};
const maxCrossAxis = (
	size: Size,
	parentSize: Size,
	splitType: "vertical" | "horizontal",
) => {
	if (splitType === "vertical") {
		return {
			width: size.width,
			height: Math.max(size.height, parentSize.height),
		};
	}
	return { width: Math.max(size.width, parentSize.width), height: size.height };
};
const maxMainAxis = (
	size: Size,
	minSize: Size,
	splitType: "vertical" | "horizontal",
) => {
	if (splitType === "vertical") {
		return { width: Math.max(size.width, minSize.width), height: size.height };
	}
	return { width: size.width, height: Math.max(size.height, minSize.height) };
};
const minCrossAxis = (
	size: Size,
	parentSize: Size,
	splitType: "vertical" | "horizontal",
) => {
	if (splitType === "vertical") {
		return {
			width: size.width,
			height: Math.min(size.height, parentSize.height),
		};
	}
	return {
		width: Math.min(size.width, parentSize.width),
		height: size.height,
	};
};

const sumMainAxis = (size: Panel[], splitType: "vertical" | "horizontal") => {
	if (splitType === "vertical") {
		return size.reduce((acc, child) => acc + child.size.width, 0);
	}
	return size.reduce((acc, child) => acc + child.size.height, 0);
};
export const resize = (panel: Panel, minSize: Size, size: Size) => {
	// const { size, splitType, children } = panel;
	const resizeMap: Record<Uuid, { from: Panel; to: Panel }> = {};
	const resized = resizeAndMeasure(panel, minSize, size, resizeMap);

	return resizeMap;
};
export const resizeAndMeasure = (
	panel: Panel,
	minSize: Size,
	maxSize: Size,
	resizeMap: Record<Uuid, { from: Panel; to: Panel }>,
): Panel => {
	console.log(panel.id, maxSize);

	if (panel.children.length === 0) {
		return setIfDifferent(resizeMap, panel, {
			...panel,
			size: clamp(panel.size, minSize, maxSize),
		});
	}

	const children: Panel[] = [];
	const size: Size = {
		width: 0,
		height: 0,
	};
	const availableSpace = clamp(panel.size, minSize, maxSize);

	for (const child of panel.children) {
		const childResized = resizeAndMeasure(
			child,
			minSize,
			availableSpace,
			resizeMap,
		);

		children.push(childResized);

		if (panel.splitType === "vertical") {
			console.log(availableSpace.width, childResized.size.width);
			availableSpace.width -= childResized.size.width;
			size.width += childResized.size.width;
			size.height = Math.max(size.height, childResized.size.height);
		} else {
			availableSpace.height -= childResized.size.height;
			size.height += childResized.size.height;
			size.width = Math.max(size.width, childResized.size.width);
		}
	}

	console.log(
		getMainSize(size, panel.splitType) / getMainSize(maxSize, panel.splitType),
	);

	return setIfDifferent(resizeMap, panel, {
		...panel,
		size,
		children,
	});
	// throw new Error("Not implemented");
};

const setIfDifferent = (
	map: Record<Uuid, { from: Panel; to: Panel }>,
	original: Panel,
	updated: Panel,
) => {
	if (
		original.size.width !== updated.size.width ||
		original.size.height !== updated.size.height
	) {
		map[original.id] = {
			from: original,
			to: updated,
		};
		return updated;
	}
	return original;
};

export const viewToPanel = (
	viewMap: Record<Uuid, View>,
	root: Uuid,
	absolute: Size,
): Panel => {
	const view = viewMap[root];
	if (view.type === "split") {
		// const absoluteSize = toAbsoluteSize(
		// 	view.splitType === "vertical"
		// 		? { width: 1, height: view.splits.length }
		// 		: { width: view.splits.length, height: 1 },
		// 	absolute,
		// );
		return {
			id: view.id,
			size: absolute,
			splitType: view.splitType,
			children: view.splits.map((split) =>
				viewToPanel(
					viewMap,
					split.id,
					toAbsoluteSize(
						view.splitType === "vertical"
							? { width: split.size, height: 1 }
							: { width: 1, height: split.size },
						absolute,
					),
				),
			),
		};
	}
	return {
		id: view.id,
		size: absolute,
		splitType: "vertical",
		children: [],
	};
};

export const remapSplits = (
	viewMap: Record<Uuid, View>,
	root: Uuid,
	absolute: Size,
): Record<Uuid, View> => {
	const asPanels = viewToPanel(viewMap, root, absolute);
	const resized = resize(asPanels, { width: 30, height: 10 }, absolute);
	console.log("resized", inspect(resized, false, 10, true));
	const entries = Object.entries(viewMap).map(([id, view]) => {
		// const panel = resized[view.id];
		if (view.type !== "split") return [id, view];

		let isResized = false;
		const splits = view.splits.map((split) => {
			if (!(split.id in resized)) return split;
			isResized = true;
			const { from, to } = resized[split.id];
			const multiplier =
				getMainSize(to.size, view.splitType) /
				getMainSize(from.size, view.splitType);
			return {
				id: split.id,
				size: split.size * multiplier,
			};
		});
		if (!isResized) return [id, view];
		return [
			id,
			{
				...view,
				splits,
			},
		];
		// return [id, view];
	});
	return Object.fromEntries(entries);
};
export const countSubDivisions = (
	viewMap: Record<Uuid, View>,
	viewId: Uuid,
	splitType: "vertical" | "horizontal",
): number => {
	const view = viewMap[viewId];
	if (view.type !== "split") return 1;

	if (view.splitType !== splitType) {
		return view.splits.reduce((acc, split) => {
			return Math.max(acc, countSubDivisions(viewMap, split.id, splitType));
		}, 0);
	}
	return view.splits.reduce((acc, split) => {
		return acc + countSubDivisions(viewMap, split.id, splitType);
	}, 0);
};

export const adjustChildren = (
	viewMap: Record<Uuid, View>,
	viewId: Uuid,
	minSize: number,
) => {
	const view = viewMap[viewId];
	if (view.type !== "split") return;
};
