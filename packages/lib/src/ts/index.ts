import type { AstNode, HirInstruction, Token } from "./types";
import { type HostString, type Instance, instantiate } from "./wasm";

export class Silk {
	instance: Instance;

	constructor(instance: Instance) {
		this.instance = instance;
	}
	static async fromBytes(bytes: Uint8Array) {
		return new Silk(await instantiate(bytes.buffer));
	}
	source(source: string) {
		return this.instance.createString(source);
	}
	lexHostString(source: HostString) {
		const lexedPointer = this.instance.lex(source.pointer);
		const lexedString = this.instance.stringFromPointer(lexedPointer);
		lexedString.dispose();
		return lexedString.json() as Token[];
	}

	lex(source: string) {
		const hostString = this.source(source);
		const lexed = this.lexHostString(hostString);
		hostString.dispose();
		return lexed;
	}
	parseAstFromHostString(source: HostString) {
		const astPointer = this.instance.parseAst(source.pointer);
		const astString = this.instance.stringFromPointer(astPointer);
		astString.dispose();
		return astString.json() as {
			nodes: AstNode[];
		};
	}

	parseAst(source: string) {
		const hostString = this.source(source);
		const ast = this.parseAstFromHostString(hostString);
		hostString.dispose();
		return ast;
	}

	parseHirFromHostString(source: HostString) {
		const hirPointer = this.instance.parseHir(source.pointer);
		const hirString = this.instance.stringFromPointer(hirPointer);
		hirString.dispose();
		return hirString.json() as {
			instructions: HirInstruction[];
		};
	}
	parseHir(source: string) {
		const hostString = this.source(source);
		const hir = this.parseHirFromHostString(hostString);
		hostString.dispose();
		return hir;
	}
}
