"""Cold-build ablation of the real self-hosted lexer/parser, not reimplementations.

Run from any directory after pnpm build. macOS /usr/bin/time -l supplies peak RSS.
All snapshot setup, oracle work, and native execution occur outside build timing.
"""

import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import platform
import re
import shutil
import statistics
import subprocess
import sys
import tempfile
import time


DIRECTORY = Path(__file__).resolve().parent
ROOT = DIRECTORY.parent.parent
STAGES = ("lexer", "parser", "print", "cli")
ENTRIES = {"lexer": "Lex.silk", "parser": "Parse.silk", "print": "Print.silk"}
REMOVED_ENVIRONMENT = (
    "NODE_OPTIONS", "NODE_COMPILE_CACHE", "NODE_V8_COVERAGE", "SILK_NATIVE_CACHE_DIR",
)
ORACLE = """
import { readFileSync } from 'node:fs'
import * as Lexer from './packages/compiler/dist/Lexer.js'
import * as Parser from './packages/compiler/dist/Parser.js'
import * as SourceFile from './packages/compiler/dist/SourceFile.js'
import * as SyntaxTree from './packages/compiler/dist/SyntaxTree.js'
const lexical = Lexer.lex(SourceFile.make(process.argv[1], readFileSync(process.argv[1])))
const tree = Parser.parse(lexical)
const count = node => 1 + node.children.filter(SyntaxTree.isNode).reduce((n, child) => n + count(child), 0)
if (tree.parserDiagnostics.length !== 0) throw new Error('input must parse without diagnostics')
console.log(JSON.stringify({tokens: lexical.tokens.length, nodes: count(tree.root)}))
"""


def run(command, env, timeout=180):
    completed = subprocess.run([str(part) for part in command], cwd=ROOT, env=env,
                               capture_output=True, text=True, timeout=timeout)
    if completed.returncode != 0:
        raise RuntimeError(f"command failed ({completed.returncode}): {command}\n"
                           f"{completed.stdout}\n{completed.stderr}")
    return completed


def digest(data):
    return hashlib.sha256(data).hexdigest()


def source_hashes():
    paths = sorted((ROOT / "compiler/src").rglob("*.silk"))
    paths += sorted(DIRECTORY.glob("*.silk"))
    paths += [Path(__file__).resolve()]
    return {str(path.relative_to(ROOT)): digest(path.read_bytes()) for path in paths}


def prepare(scratch, env, node):
    source = ROOT / "compiler/src/main.silk"
    input_path = scratch / "input.silk"
    input_path.write_bytes(source.read_bytes())
    oracle = json.loads(run([node, "--input-type=module", "-e", ORACLE, input_path], env).stdout)
    if not oracle["tokens"] or not oracle["nodes"]:
        raise RuntimeError("empty oracle result")
    snapshot = scratch / "src"
    shutil.copytree(ROOT / "compiler/src", snapshot)
    bench = snapshot / "bench"
    bench.mkdir()
    for filename in ("Support.silk", "TreeCheck.silk"):
        shutil.copyfile(DIRECTORY / filename, bench / filename)
    literal = json.dumps(input_path.read_text(), ensure_ascii=True)
    generated = (
        f"pub fn source() -> string<'static> {{return {literal}}}\n"
        f"pub fn tokenCount() -> usize {{return {oracle['tokens']}}}\n"
        f"pub fn nodeCount() -> usize {{return {oracle['nodes']}}}\n"
    )
    (bench / "Input.silk").write_text(generated)
    for stage, filename in ENTRIES.items():
        shutil.copyfile(DIRECTORY / filename, snapshot / filename)
    return snapshot, input_path, oracle, digest(generated.encode())


def measure(stage, round_index, position, scratch, snapshot, input_path, oracle, env, node):
    sample = scratch / f"{round_index}-{stage}"
    sample.mkdir()
    binary = sample / "program"
    entry = snapshot / ENTRIES.get(stage, "main.silk")
    command = [node, ROOT / "packages/cli/dist/bin.js", "build-exe", entry,
               "--source-root", snapshot, "--optimization", "debug", "--output", binary, "--timings"]
    load_before = os.getloadavg()
    start = time.perf_counter_ns()
    compiled = run(["/usr/bin/time", "-l", *command], env)
    wall_ms = (time.perf_counter_ns() - start) / 1_000_000
    report = compiled.stdout + "\n" + compiled.stderr
    peak = re.search(r"^\s*(\d+)\s+maximum resident set size$", report, re.MULTILINE)
    symbols = re.search(r", (\d+) symbols\)", report)
    phases = [{"name": name, "wallMs": float(ms), "inputs": int(inputs),
               "outputs": int(outputs), "diagnostics": int(diagnostics)}
              for name, ms, inputs, outputs, diagnostics in re.findall(
                  r"^\s+([\w-]+)\s+([\d.]+)ms\s+(\d+) in / (\d+) out\s+(\d+) diag",
                  report, re.MULTILINE)]
    if peak is None or symbols is None or not phases:
        raise RuntimeError(f"missing build metrics\n{report}")
    execute = [binary]
    if stage == "cli":
        execute.append(input_path.relative_to(ROOT))
    result = run(execute, env, timeout=15)
    if result.stderr:
        raise RuntimeError(f"unexpected runtime stderr: {result.stderr}")
    if stage in ("lexer", "parser"):
        if result.stdout:
            raise RuntimeError("unexpected output from non-printing stage")
    else:
        expected_header = f"root #{oracle['nodes'] - 1}\n\nnodes {oracle['nodes']}\n"
        if not result.stdout.startswith(expected_header) or not result.stdout.endswith("\ndiagnostics 0\n"):
            raise RuntimeError("unexpected AST header or diagnostics")
    return {
        "stage": stage, "round": round_index, "position": position,
        "command": [str(part) for part in command], "wallMs": wall_ms,
        "peakRssBytes": int(peak.group(1)), "symbols": int(symbols.group(1)),
        "phases": phases, "stdout": compiled.stdout, "stderr": compiled.stderr,
        "verifiedExitCode": result.returncode, "runtimeStdoutSha256": digest(result.stdout.encode()),
        "runtimeStdoutBytes": len(result.stdout.encode()),
        "loadAverageBefore": load_before, "loadAverageAfter": os.getloadavg(),
    }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--samples", type=int, default=3)
    parser.add_argument("--stages", nargs="+", choices=STAGES, default=list(STAGES))
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    if sys.platform != "darwin" or args.samples < 1:
        parser.error("requires macOS and --samples >= 1")
    if len(set(args.stages)) != len(args.stages):
        parser.error("duplicate stages")
    if args.output.exists():
        parser.error("output already exists; choose a new path")
    node = shutil.which("node")
    if node is None:
        parser.error("node must be on PATH")
    env = {key: value for key, value in os.environ.items() if key not in REMOVED_ENVIRONMENT}
    env["NODE_DISABLE_COMPILE_CACHE"] = "1"
    metadata = {
        "startedAt": datetime.now(timezone.utc).isoformat(), "host": platform.platform(),
        "cpu": run(["sysctl", "-n", "machdep.cpu.brand_string"], env).stdout.strip(),
        "node": run([node, "--version"], env).stdout.strip(),
        "clang": run(["clang", "--version"], env).stdout.strip(),
        "revision": run(["git", "rev-parse", "HEAD"], env).stdout.strip(),
        "compilerDiffSha256": digest(run(["git", "diff", "HEAD", "--", "packages/compiler/src",
            "packages/llvm/src", "packages/cli/src"], env).stdout.encode()),
        "cliSha256": digest((ROOT / "packages/cli/dist/bin.js").read_bytes()),
        "toolchainIdentitySha256": digest((ROOT / "packages/compiler/src/ToolchainIntegrity.generated.ts").read_bytes()),
        "sourceHashes": source_hashes(), "samples": args.samples, "stages": args.stages,
        "policy": "fresh Node and output per sample; native and Node compile caches disabled; OS page cache not flushed; serial rotating stage order; --timings included; snapshot/oracle/execution excluded",
    }
    rows = []
    # Keep the snapshot within the working-directory filesystem provider's root.
    # Cleanup is limited to this owned temporary directory, on all exits.
    with tempfile.TemporaryDirectory(prefix=".selfhost-stages-", dir=ROOT / "benchmarks") as temporary:
        scratch = Path(temporary)
        snapshot, input_path, oracle, generated_hash = prepare(scratch, env, node)
        metadata.update(oracle=oracle, generatedInputSha256=generated_hash)
        ast_hash = None
        for round_index in range(args.samples):
            for position in range(len(args.stages)):
                stage = args.stages[(round_index + position) % len(args.stages)]
                row = measure(stage, round_index, position, scratch, snapshot, input_path, oracle, env, node)
                if stage in ("print", "cli"):
                    if ast_hash is not None and row["runtimeStdoutSha256"] != ast_hash:
                        raise RuntimeError("AST output differs between builds/stages")
                    ast_hash = row["runtimeStdoutSha256"]
                if stage == "cli" and round_index == 0:
                    # Reuse the existing differential oracle: complete bootstrap AST shape,
                    # postorder IDs, spans, token ownership, and no orphan nodes.
                    checked = run([node, ROOT / "compiler/scripts/test-parser.mjs",
                                   scratch / f"{round_index}-{stage}" / "program", input_path], env)
                    metadata["differentialCheck"] = checked.stdout
                rows.append(row)
                print(f"{stage} {round_index + 1}/{args.samples}: {row['wallMs']/1000:.2f}s, "
                      f"{row['symbols']} symbols, {row['peakRssBytes']/1024**2:.0f} MiB", flush=True)
    if source_hashes() != metadata["sourceHashes"]:
        raise RuntimeError("sources changed during measurement; refusing mixed-revision results")
    summaries = []
    for stage in args.stages:
        selected = [row for row in rows if row["stage"] == stage]
        times = [row["wallMs"] for row in selected]
        names = sorted({phase["name"] for row in selected for phase in row["phases"]})
        summaries.append({
            "stage": stage, "medianMs": statistics.median(times), "minMs": min(times), "maxMs": max(times),
            "medianPeakRssBytes": statistics.median(row["peakRssBytes"] for row in selected),
            "symbols": sorted({row["symbols"] for row in selected}),
            "phaseMediansMs": {name: statistics.median(sum(p["wallMs"] for p in row["phases"] if p["name"] == name)
                for row in selected) for name in names},
        })
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("x") as stream:
        json.dump({"metadata": metadata, "summaries": summaries, "rows": rows}, stream, indent=2)
        stream.write("\n")
    print(f"Results: {args.output.resolve()}")


if __name__ == "__main__":
    main()
