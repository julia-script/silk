import { parser } from "./gen/parser";
import { foldNodeProp, foldInside, indentNodeProp } from "@codemirror/language";
import { styleTags, tags as t } from "@lezer/highlight";
import { LRLanguage } from "@codemirror/language";

const keywords = [
	"if",
	"else",
	"const",
	"var",
	"return",
	"true",
	"false",
	"export",
	"extern",
	"pub",
	"fn",
	"while",
];
export const parserWithMetadata = parser.configure({
	props: [
		styleTags({
			// Identifier: t.definition(t.name),
			// FunctionDeclaration: t.definition(t.name),
			PropertyName: t.variableName,
			"FnProto/Identifier": t.function(t.name),

			[keywords.join(" ")]: t.keyword,
			Number: t.number,
			Boolean: t.bool,
			String: t.string,
			// LineComment: t.lineComment,
			"TypeName/...": t.typeName,
			"( )": t.paren,
		}),
		indentNodeProp.add({
			Application: (context) =>
				context.column(context.node.from) + context.unit,
		}),
		foldNodeProp.add({
			Application: foldInside,
		}),
	],
});
// console.log(parserWithMetadata.parse("function foo() { return 1; }"));
export const langLanguage = LRLanguage.define({
	parser: parserWithMetadata,
	languageData: {
		commentTokens: { line: "//" },
	},
});
