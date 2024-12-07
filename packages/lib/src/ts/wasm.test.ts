import { inspect } from "bun";
import { HostString, instantiate } from "./wasm";
import { describe, it, expect } from "bun:test";
const source = `
export fn fib(n: i32): i32 {
  var a:i32 = 0;
  var b:i32 = 1;
  if (n > 0) {
    while (n > 0) {
      n = n - 1;
      var t:i32 = a + b;
      a = b;
      b = t;
    }
    return b
  }
  return a
}
`;
const bytes = await Bun.file("./dist/bin/lang.wasm").arrayBuffer();
describe("instantiate", () => {
	it("should instantiate", async () => {
		const lang = await instantiate(bytes);
		// console.log(instance);
		const hostString = lang.createString(source);
		const lexedPointer = lang.lex(hostString.pointer);
		const lexedString = lang.stringFromPointer(lexedPointer);
		console.log(lexedString);
		// const writer = lang.createReader();
		// lang.lex(writer.id, hostString.pointer);
		// const astReader = lang.createReader();
		// const result = await writer.promise;
		// lang.generateAst(astReader.id, hostString.pointer);
		// const astResult = await astReader.promise;
		// lang.compile(writer.id, hostString.pointer);
		// const compileResult = await writer.promise;

		// console.log(JSON.parse(result));
		// const fs = lang.createFs();
		// // fs.makeDir("./a/b/c");
		// fs.makeFile("./test.txt");
		// const tree = fs.getFileTree();
		// console.log(inspect(tree, { depth: 10, colors: true }));
		// const fsPointer = lang.createFs();
		// const treePointer = lang.getFileTree(fsPointer);
		// const view = new DataView(lang.memory.buffer);
		// const treeLength = view.getUint32(treePointer, true);
		// const treeBuf = new Uint8Array(
		// 	lang.memory.buffer,
		// 	treePointer + 4,
		// 	treeLength,
		// );
		// const tree_str = new TextDecoder().decode(treeBuf);
		// lang.free(treePointer, treeLength + 4);
		// console.log(tree_str);
		// lang.destroyFs(fsPointer);
	});
});
