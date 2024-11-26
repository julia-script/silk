import bytes from "@lang/lib/dist/bin/lang.wasm";
import { instantiate } from "@lang/lib/src/wasm";
export const lang = await instantiate(bytes.buffer);

// const encoder = new TextEncoder();
// const encode = encoder.encode.bind(encoder);
// const decoder = new TextDecoder();
// const decode = decoder.decode.bind(decoder);

// class Reader {
// 	data: number[] = [];
// 	promise: Promise<string>;
// 	resolve: (value: string | PromiseLike<string>) => void = () => {};

// 	reject: (err: Error) => void = () => {};
// 	signal: AbortSignal;
// 	constructor(
// 		public id: number,
// 		signal: AbortSignal,
// 	) {
// 		this.signal = signal;
// 		this.promise = new Promise((resolve, reject) => {
// 			this.resolve = resolve;
// 			this.reject = reject;
// 		});
// 		this.signal.addEventListener("abort", () => {
// 			this.reject(new Error("Aborted"));
// 		});
// 	}
// 	writeByte(byte: number) {
// 		this.data.push(byte);
// 	}
// 	toString() {
// 		return decode(new Uint8Array(this.data));
// 	}
// }
// class ReaderManager {
// 	static id = 0;
// 	static readers = new Map<number, Reader>();

// 	static registerWriter = (id: number, writer: Reader) => {
// 		this.readers.set(id, writer);
// 	};

// 	static invokeWriter = (id: number, byte: number) => {
// 		const writer = ReaderManager.readers.get(id);
// 		if (!writer) throw new Error("Writer not found");
// 		writer.writeByte(byte);
// 	};

// 	static destroyWriter = (id: number) => {
// 		// WriterManager.writers.delete(id);
// 		const reader = ReaderManager.readers.get(id);
// 		if (!reader) throw new Error("Reader not found");
// 		reader.resolve(reader.toString());
// 	};

// 	static create = (
// 		signal: AbortSignal = AbortSignal.timeout(5000),
// 		id: number = ReaderManager.id++,
// 	) => {
// 		// const id = id ?? WriterManager.id;
// 		// WriterManager.id += 1;
// 		const writer = new Reader(id, signal);
// 		ReaderManager.registerWriter(id, writer);
// 		return writer;
// 	};
// }

// export const wasm = await WebAssembly.instantiate(lang, {
// 	env: {
// 		throw: (pointer: number, length: number) => {
// 			const message = decode(new Uint8Array(memory.buffer, pointer, length));
// 			throw new Error(message);
// 		},

// 		write: (pointer: number, length: number) => {
// 			const message = decode(new Uint8Array(memory.buffer, pointer, length));
// 			console.log(message);
// 		},

// 		invokeWriter: ReaderManager.invokeWriter,
// 		destroyWriter: ReaderManager.destroyWriter,
// 		memory,
// 	},
// });

// const instance = wasm.instance.exports as {
// 	wasmAlloc: (length: number) => number;
// 	wasmAllocZ: (length: number) => number;
// 	wasmFree: (pointer: number, length: number) => void;
// 	lex: (writerId: number, source: Uint8Array) => void;
// };
// const U32_SIZE = 4;

// class HostString {
// 	pointer: number;
// 	length: number;
// 	constructor(public value: string) {
// 		const buf = encode(value);
// 		const pointer = instance.wasmAlloc(buf.length + U32_SIZE);

// 		const view = new DataView(memory.buffer);
// 		view.setUint32(pointer, buf.length, true);

// 		const array = new Uint8Array(memory.buffer, pointer + U32_SIZE, buf.length);
// 		array.set(buf);
// 		this.pointer = pointer;
// 		this.length = buf.length;
// 	}

// 	dispose() {
// 		instance.wasmFree(this.pointer, this.length);
// 	}
// 	[Symbol.dispose]() {
// 		this.dispose();
// 	}
// }
// const lex = (source: string) => {
// 	const sourceAtHost = new HostString(source);
// 	const writer = ReaderManager.create();
// 	instance.lex(
// 		writer.id,
// 		new Uint8Array(
// 			memory.buffer,
// 			sourceAtHost.pointer,
// 			sourceAtHost.value.length,
// 		),
// 	);

// 	return writer.promise;
// };

// console.log("lexed", await lex("1 + 1"));
