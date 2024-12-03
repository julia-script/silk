"use client";

import { Terminal as XTermJs } from "@xterm/xterm";
import "@xterm/xterm/css/xterm.css";

import type { WebContainer } from "@webcontainer/api";
import type { FitAddon } from "@xterm/addon-fit";
import { use, useEffect, useRef, useState, useSyncExternalStore } from "react";
import { useWebContainer } from "./WebContainer";
import { ResizablePanel } from "./ui/resizable";

class Shell extends EventTarget {
	term = new XTermJs({
		convertEol: true,

		theme: {
			foreground: "#eff0eb",
			background: "#00000000",
			selection: "#97979b33",
			black: "#282a36",
			brightBlack: "#686868",
			red: "#ff5c57",
			brightRed: "#ff5c57",
			green: "#5af78e",
			brightGreen: "#5af78e",
			yellow: "#f3f99d",
			brightYellow: "#f3f99d",
			blue: "#57c7ff",
			brightBlue: "#57c7ff",
			magenta: "#ff6ac1",
			brightMagenta: "#ff6ac1",
			cyan: "#9aedfe",
			brightCyan: "#9aedfe",
			white: "#f1f1f0",
			brightWhite: "#eff0eb",
		},
	});
	// fitAddon = new FitAddon();
	subscribers = new Set<() => void>();
	startPromise: Promise<void> | null = null;
	fitAddon: FitAddon | null = null;
	constructor(private container: WebContainer) {
		super();
		// this.term.loadAddon(this.fitAddon);
	}

	private start = async (terminalRef: HTMLDivElement) => {
		if (typeof window === "undefined") return;
		const { FitAddon } = await import("@xterm/addon-fit");
		this.fitAddon = new FitAddon();
		this.term.open(terminalRef);
		this.term.loadAddon(this.fitAddon);
		const shellProcess = await this.container.spawn("jsh", {
			cwd: "project",

			terminal: {
				cols: this.term.cols,
				rows: this.term.rows,
			},
		});

		const input = shellProcess.input.getWriter();
		const sourceCmd = "source /home/silk/.jshrc && clear\n";
		await input.write(sourceCmd);

		const onData = this.term.onData((data) => {
			input.write(data);
		});

		const onDispose = () => {
			onData.dispose();
			shellProcess.kill();
			this.startPromise = null;
			console.log("disposed");
			this.removeEventListener("dispose", onDispose);
		};
		this.addEventListener("dispose", onDispose);
		await shellProcess.output.pipeTo(
			new WritableStream({
				write: (data) => {
					this.term.write(data);
					this.fitAddon?.fit();
				},
			}),
		);
		this.fitAddon?.fit();
	};
	fit = () => {
		this.fitAddon?.fit();
	};
	open = async (terminalRef: HTMLDivElement) => {
		if (this.startPromise) return this.startPromise;
		this.startPromise = this.start(terminalRef);
		return this.startPromise;
	};
	getSnapshot = () => {
		return null;
	};

	subscribe = (fn: () => void) => {
		this.addEventListener("update", fn);
		return () => {
			this.removeEventListener("update", fn);
		};
	};
	dispose = () => {
		this.term.dispose();
		this.dispatchEvent(new Event("update"));
	};
}

export const Terminal = () => {
	const { container } = useWebContainer();
	const [height, setHeight] = useState(200);
	const [shell] = useState(() => new Shell(container));
	useSyncExternalStore(shell.subscribe, shell.getSnapshot, shell.getSnapshot);
	const terminalRef = useRef<HTMLDivElement>(null);

	useEffect(() => {
		if (!terminalRef.current) return;
		shell.open(terminalRef.current);

		return () => {
			shell.dispose();
		};
	}, [shell]);

	return (
		<ResizablePanel
			defaultSize={30}
			onResize={(size) => {
				console.log("size", size);
				shell.fit();
				setHeight(document.documentElement.clientHeight * (size / 100));
			}}
		>
			<div ref={terminalRef} className="h-full px-4" style={{ height }} />
		</ResizablePanel>
	);
};
