//!trackNewline

import { ContextTracker } from "@lezer/lr";
import { spaces, newline, BlockComment, LineComment } from "./gen/parser.terms";

export const trackNewline = new ContextTracker({
	start: false,
	shift(context, term) {
		return term == LineComment || term == BlockComment || term == spaces
			? context
			: term == newline;
	},
	strict: false,
});

//!externalTokenizers

import { ExternalTokenizer } from "@lezer/lr";
import { insertSemi, noSemi } from "./gen/parser.terms";

const space = [9, 10, 11, 12, 13, 32, 133, 160];
const braceR = 125,
	semicolon = 59,
	slash = 47,
	star = 42,
	plus = 43,
	minus = 45;

export const insertSemicolon = new ExternalTokenizer(
	(input, stack) => {
		const { next } = input;
		if (next == braceR || next == -1 || stack.context)
			input.acceptToken(insertSemi);
	},
	{ contextual: true, fallback: true },
);

export const noSemicolon = new ExternalTokenizer(
	(input, stack) => {
		let { next } = input,
			after;
		if (space.indexOf(next) > -1) return;
		if (next == slash && ((after = input.peek(1)) == slash || after == star))
			return;
		if (next != braceR && next != semicolon && next != -1 && !stack.context)
			input.acceptToken(noSemi);
	},
	{ contextual: true },
);
