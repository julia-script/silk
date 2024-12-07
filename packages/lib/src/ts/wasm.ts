const encoder = new TextEncoder();
const encode = encoder.encode.bind(encoder);
const decoder = new TextDecoder();
const decode = decoder.decode.bind(decoder);
type Export = {
	memory: WebAssembly.Memory;
	alloc: (length: number) => number;
	free: (pointer: number, length: number) => void;

	lex: (stringPointer: number) => number;
	parseAst: (stringPointer: number) => number;
	parseHir: (stringPointer: number) => number;
};
const U32_SIZE = 4;

export class HostString {
	pointer: number;
	length: number;
	constructor(
		private instance: {
			memory: WebAssembly.Memory;
			alloc: (length: number) => number;
			free: (pointer: number, length: number) => void;
		},
		public value: string,
	) {
		const src = encode(value);
		const pointer = this.instance.alloc(src.length + U32_SIZE);
		const view = new DataView(this.instance.memory.buffer);
		view.setUint32(pointer, src.length, true);
		const dest = new Uint8Array(
			this.instance.memory.buffer,
			pointer + U32_SIZE,
			src.length,
		);
		dest.set(src);
		this.pointer = pointer;
		this.length = src.length;
	}

	dispose() {
		this.instance.free(this.pointer, this.length + U32_SIZE);
	}
	[Symbol.dispose]() {
		this.dispose();
	}
}
const stringFromPointer = (exports: Export, string_pointer: number) => {
	const view = new DataView(exports.memory.buffer);
	const length = view.getUint32(string_pointer, true);
	const slice = new Uint8Array(
		exports.memory.buffer,
		string_pointer + U32_SIZE,
		length,
	);
	const decoded = decode(slice);
	return {
		pointer: string_pointer,
		string: decoded,
		json: () => JSON.parse(decoded),
		dispose: () => {
			exports.free(string_pointer, length + U32_SIZE);
		},
	};
};

export const instantiate = async (
	buffer: ArrayBufferLike,
	memory: WebAssembly.Memory = new WebAssembly.Memory({ initial: 1024 }),
) => {
	const instance = await WebAssembly.instantiate(buffer, {
		env: {
			memory,
			throw: (pointer: number, length: number) => {
				const message = decode(new Uint8Array(memory.buffer, pointer, length));
				throw new Error(message);
			},

			write: (pointer: number, length: number) => {
				const message = decode(new Uint8Array(memory.buffer, pointer, length));
				console.log(message);
			},

			// invokeWriter: ReaderManager.invokeWriter,
			// destroyWriter: ReaderManager.destroyWriter,
		},
	});

	const exports = {
		...(instance.instance.exports as Export),
		memory,
	};

	return {
		...exports,
		memory,
		// createReader: ReaderManager.create,
		stringFromPointer: (pointer: number) => stringFromPointer(exports, pointer),
		createString: (value: string) => new HostString(exports, value),
	};
};

export type Instance = Awaited<ReturnType<typeof instantiate>>;
