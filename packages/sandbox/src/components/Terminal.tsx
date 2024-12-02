"use client";

import { Terminal as XTermJs } from "@xterm/xterm";
import "@xterm/xterm/css/xterm.css";

import type { WebContainer } from "@webcontainer/api";
import { memoize } from "lodash";
import { use, useEffect, useRef, useState, useSyncExternalStore } from "react";
import { useWebContainer } from "./WebContainer";
import { ResizablePanel } from "./ui/resizable";

class Shell extends EventTarget {
	term = new XTermJs({
		convertEol: true,
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
			terminal: {
				cols: this.term.cols,
				rows: this.term.rows,
			},
		});

		const input = shellProcess.input.getWriter();
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

export const Terminal = ({ height }: { height?: number }) => {
	const { container } = useWebContainer();
	const [shell] = useState(() => new Shell(container));
	useSyncExternalStore(shell.subscribe, shell.getSnapshot, shell.getSnapshot);
	const terminalRef = useRef<HTMLDivElement>(null);

	useEffect(() => {
		if (!terminalRef.current) return;
		shell.open(terminalRef.current);
		console.log("shell opened");

		return () => {
			shell.dispose();
		};
	}, [shell]);

	return (
		<ResizablePanel
			defaultSize={30}
			onResize={(size) => {
				shell.fit();
			}}
		>
			<div
				className="h-full w-full relative overflow-hidden grow"
				style={{ height }}
				ref={terminalRef}
			/>
		</ResizablePanel>
	);
};
