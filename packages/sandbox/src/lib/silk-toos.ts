import { Silk } from "@lang/lib/dist/index";
import bytes from "@lang/lib/dist/bin/lang.wasm";
import { use } from "react";

const promise = Silk.fromBytes(bytes);

export const useSilkTools = () => {
	return use(promise);
};
