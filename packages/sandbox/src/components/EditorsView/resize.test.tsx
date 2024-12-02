import { describe, expect, it } from "bun:test";
import { inspect } from "util";
import type { Uuid } from "@/lib/store";
import type { View } from "@/lib/store";
import { uniqueId } from "lodash";
import {
	type Panel,
	countSubDivisions,
	remapSplits,
	resize,
	viewToPanel,
} from "./resize";

describe("resize", () => {
	it("should resize", () => {
		const views: Record<Uuid, View> = {
			root: {
				id: "root",
				type: "split",
				splits: [
					{ id: "a", size: 1 / 3 },
					{ id: "b", size: 1 / 3 },
					{ id: "c", size: 1 / 3 },
				],
				splitType: "vertical",
			},
			a: {
				id: "a",
				type: "view",
				buffer: null,
			},
			b: {
				id: "b",
				type: "view",
				buffer: null,
			},
			c: {
				id: "c",
				type: "split",
				splits: [
					{ id: "c1", size: 1 / 2 },
					{ id: "c2", size: 1 / 2 },
				],
				splitType: "horizontal",
			},
			c1: {
				id: "c1",
				type: "view",
				buffer: null,
			},
			c2: {
				id: "c2",
				type: "split",
				splits: [
					{ id: "c21", size: 1 / 2 },
					{ id: "c22", size: 1 / 2 },
				],
				splitType: "vertical",
			},
			c21: {
				id: "c21",
				type: "view",
				buffer: null,
			},
			c22: {
				id: "c22",
				type: "view",
				buffer: null,
			},
		};
		console.log(countSubDivisions(views, "root", "vertical"));

		// console.log(
		// 	inspect(
		// 		viewToPanel(views, "root", { width: 200, height: 100 }),
		// 		false,
		// 		10,
		// 		true,
		// 	),
		// );
		// const rootAbsoluteSize = { width: 100, height: 100 };

		const unique = () => uniqueId("id");
		const rootPanel: Panel = {
			id: unique(),
			size: { width: 10, height: 10 },
			splitType: "vertical",
			children: [
				{
					id: unique(),
					size: { width: 5, height: 10 },
					splitType: "vertical",
					children: [],
				},
				{
					id: unique(),
					size: { width: 3, height: 10 },
					splitType: "vertical",
					children: [],
				},
				{
					id: unique(),
					size: { width: 2, height: 10 },
					splitType: "horizontal",
					children: [
						{
							id: unique(),
							size: { width: 1, height: 5 },
							splitType: "vertical",
							children: [],
						},
						{
							id: unique(),
							size: { width: 1, height: 5 },
							splitType: "vertical",
							children: [
								{
									id: unique(),
									size: { width: 2, height: 5 },
									splitType: "vertical",
									children: [],
								},
								{
									id: unique(),
									size: { width: 1, height: 5 },
									splitType: "vertical",
									children: [],
								},
							],
						},
					],
				},
			],
		};
		// const resizeMap = resize(
		// 	rootPanel,
		// 	{ width: 1, height: 1 },
		// 	rootPanel.size,
		// );
		// console.log(resizeMap);
		// console.log(
		// 	inspect(
		// 		remapSplits(
		// 			{
		// 				a: {
		// 					id: "a",
		// 					type: "split",
		// 					splits: [
		// 						{ id: "a1", size: 1 / 4 },
		// 						{ id: "a2", size: 1 / 4 },
		// 						{ id: "a3", size: 2 / 4 },
		// 					],
		// 					splitType: "vertical",
		// 				},
		// 				a1: {
		// 					id: "a1",
		// 					type: "view",
		// 					buffer: null,
		// 				},
		// 				a2: {
		// 					id: "a2",
		// 					type: "view",
		// 					buffer: null,
		// 				},
		// 				a3: {
		// 					id: "a3",
		// 					type: "view",
		// 					buffer: null,
		// 				},
		// 			},
		// 			"a",
		// 			{ width: 100, height: 100 },
		// 		),
		// 		false,
		// 		10,
		// 		true,
		// 	),
		// );
	});
});
