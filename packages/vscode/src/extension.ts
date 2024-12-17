import * as vscode from "vscode";

export function activate(context: vscode.ExtensionContext) {
	console.log(
		'Congratulations, your extension "helloworld-sample" is now active!',
	);

	context.subscriptions.push(
		vscode.commands.registerCommand("extension.helloWorld", () => {
			vscode.window.showInformationMessage("Hello World!");
		}),
	);
	const re = /^[\s│]*(├|└)/;

	const getLineIndent = (line: string) => {
		const [match] = line.match(re) ?? [];
		return match?.length ?? 0;
	};

	const parseRange = (document: vscode.TextDocument) => {
		const stack = [{ indent: 0, line: 1 }];
		const FR: vscode.FoldingRange[] = [];

		for (let i = 1; i < document.lineCount; i++) {
			const line = document.lineAt(i);
			const currentLineIndent = getLineIndent(line.text);
			let stackTop = stack.pop();
			while (stackTop) {
				if (currentLineIndent > stackTop.indent) {
					stack.push(stackTop);
					break;
				}
				if (i - stackTop.line > 0)
					FR.push(
						new vscode.FoldingRange(
							stackTop.line - 1,
							i - 1,
							vscode.FoldingRangeKind.Region,
						),
					);
				stackTop = stack.pop();
			}
			stack.push({ indent: currentLineIndent, line: i + 1 });
		}
		return FR;
	};
	context.subscriptions.push(
		vscode.languages.registerFoldingRangeProvider("silk-tree", {
			provideFoldingRanges(document) {
				return parseRange(document);
			},
		}),
	);
}
