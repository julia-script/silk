import { lang } from "@/bindings/lang";
import { CodeMirror } from "@/components/code-mirror";
import Image from "next/image";
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
export default function Home() {
	// useWasmBinary()
	return (
		<div className="grid grid-rows-[20px_1fr_20px] items-center justify-items-center min-h-screen p-8 pb-20 gap-16 sm:p-20 font-[family-name:var(--font-geist-sans)]">
			<CodeMirror defaultState={source} />
		</div>
	);
}
