const encoder = new TextEncoder();
const encode = encoder.encode.bind(encoder);
const decoder = new TextDecoder();
const decode = decoder.decode.bind(decoder);
type Export = {
	memory: WebAssembly.Memory;
	lex: (writerId: number, pointer: number) => void;
	alloc: (length: number) => number;
	free: (pointer: number, length: number) => void;
	generateAst: (pointer: number, length: number) => void;
	compile: (writerId: number, pointer: number) => void;
	getFileTree: (fsPointer: number) => number;
	createFs: () => number;
	destroyFs: (fsPointer: number) => void;
	makeFile: (fsPointer: number, pointer: number) => number;
	makeDir: (fsPointer: number, pointer: number) => number;
	deleteFile: (fsPointer: number, pointer: number) => void;
	deleteDir: (fsPointer: number, pointer: number) => void;
};
const U32_SIZE = 4;

type FsDir = {
	path: string;
	kind: "dir";
	hash: number;
	children: FsEntry[];
};
type FsFile = {
	path: string;
	kind: "file";
	hash: number;
};
type FsEntry = FsDir | FsFile;
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
class Fs {
	pointer: number;
	constructor(private exports: Export) {
		this.pointer = exports.createFs();
	}
	getFileTree() {
		const treePointer = this.exports.getFileTree(this.pointer);
		const treeLength = new DataView(this.exports.memory.buffer).getUint32(
			treePointer,
			true,
		);
		const treeBuf = new Uint8Array(
			this.exports.memory.buffer,
			treePointer + 4,
			treeLength,
		);
		const decoded = decode(treeBuf);
		this.exports.free(treePointer, treeLength + 4);

		return JSON.parse(decoded) as FsDir;
	}

	makeFile(path: string) {
		const hostString = new HostString(
			{
				memory: this.exports.memory,
				alloc: this.exports.alloc,
				free: this.exports.free,
			},
			path,
		);
		const hash = this.exports.makeFile(this.pointer, hostString.pointer);
		hostString.dispose();
		return hash;
	}
	makeDir(path: string) {
		const hostString = new HostString(
			{
				memory: this.exports.memory,
				alloc: this.exports.alloc,
				free: this.exports.free,
			},
			path,
		);
		const hash = this.exports.makeDir(this.pointer, hostString.pointer);
		hostString.dispose();
		return hash;
	}
	deleteFile(path: string) {
		const hostString = new HostString(
			{
				memory: this.exports.memory,
				alloc: this.exports.alloc,
				free: this.exports.free,
			},
			path,
		);
		this.exports.deleteFile(this.pointer, hostString.pointer);
		hostString.dispose();
	}
	deleteDir(path: string) {
		const hostString = new HostString(
			{
				memory: this.exports.memory,
				alloc: this.exports.alloc,
				free: this.exports.free,
			},
			path,
		);
		this.exports.deleteDir(this.pointer, hostString.pointer);
		hostString.dispose();
	}
	dispose() {
		this.exports.destroyFs(this.pointer);
	}
	[Symbol.dispose]() {
		this.dispose();
	}
}

class Reader {
	data: number[] = [];
	promise: Promise<string>;
	resolve: (value: string | PromiseLike<string>) => void = () => {};

	reject: (err: Error) => void = () => {};
	signal: AbortSignal;
	constructor(
		public id: number,
		signal: AbortSignal,
	) {
		this.signal = signal;
		this.promise = new Promise((resolve, reject) => {
			this.resolve = resolve;
			this.reject = reject;
		});
		this.signal.addEventListener("abort", () => {
			this.reject(new Error("Aborted"));
		});
	}
	writeByte(byte: number) {
		this.data.push(byte);
	}
	toString() {
		return decode(new Uint8Array(this.data));
	}
}
class ReaderManager {
	// 0 is reserved for std.io.getStdErr()
	// 1 is reserved for std.io.getStdOut()
	static id = 2;
	static readers = new Map<number, Reader>();

	static registerWriter = (id: number, writer: Reader) => {
		this.readers.set(id, writer);
	};

	static invokeWriter = (id: number, byte: number) => {
		const writer = ReaderManager.readers.get(id);
		if (!writer) throw new Error("Writer not found");
		writer.writeByte(byte);
	};

	static destroyWriter = (id: number) => {
		// WriterManager.writers.delete(id);
		const reader = ReaderManager.readers.get(id);
		if (!reader) throw new Error("Reader not found");
		reader.resolve(reader.toString());
	};

	static create = (
		signal: AbortSignal = AbortSignal.timeout(5000),
		id: number = ReaderManager.id++,
	) => {
		// const id = id ?? WriterManager.id;
		// WriterManager.id += 1;
		const writer = new Reader(id, signal);
		ReaderManager.registerWriter(id, writer);
		return writer;
	};
}
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

			invokeWriter: ReaderManager.invokeWriter,
			destroyWriter: ReaderManager.destroyWriter,
		},
	});

	const exports = {
		...(instance.instance.exports as Export),
		memory,
	};

	return {
		...exports,
		memory,
		createReader: ReaderManager.create,
		createString: (value: string) => new HostString(exports, value),
		createFs: () =>
			new Fs({
				...exports,
				memory,
			}),
	};
};
