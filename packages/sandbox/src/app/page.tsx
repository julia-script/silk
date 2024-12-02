import { lang } from "@/bindings/lang";
import { CodeMirror } from "@/components/code-mirror";
import Image from "next/image";
import { EditorsView } from "../components/EditorsView";
import { FileTree } from "../components/FileTree";
import { Terminal } from "../components/Terminal";
import { WebContainerProvider } from "../components/WebContainer";
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
		// <div className="ring-1 ring-red-500">
		<WebContainerProvider>
			{/* <FileTree /> */}
			<EditorsView />
			{/* <Terminal /> */}
		</WebContainerProvider>
		// </div>
	);
}
