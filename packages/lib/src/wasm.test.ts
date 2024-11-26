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
		const writer = lang.createReader();
		// lang.lex(writer.id, hostString.pointer);
		// const astReader = lang.createReader();
		// const result = await writer.promise;
		// lang.generateAst(astReader.id, hostString.pointer);
		// const astResult = await astReader.promise;
		lang.compile(writer.id, hostString.pointer);
		const compileResult = await writer.promise;

		// console.log(JSON.parse(result));
		console.log(JSON.parse(compileResult));
	});
});
