"""Opt-in macOS cold-build comparison. Uses only Python's standard library.

This is a measurement harness, not a timing assertion in the correctness suite.
Each compiler runs as a fresh subprocess, independently of the harness runtime.
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
LANGUAGES = ("rust", "zig", "zig-seeded", "silk")
EXTENSIONS = {"rust": "rs", "zig": "zig", "zig-seeded": "zig", "silk": "silk"}
WORKLOADS = ("minimal", "parser", "ast")
REMOVED_ENVIRONMENT = (
    "NODE_OPTIONS", "NODE_COMPILE_CACHE", "NODE_V8_COVERAGE",
    "SILK_NATIVE_CACHE_DIR", "RUSTFLAGS", "CARGO_ENCODED_RUSTFLAGS",
    "RUSTC_WRAPPER", "RUSTC_WORKSPACE_WRAPPER", "RUSTC_FORCE_INCREMENTAL",
    "ZIG_GLOBAL_CACHE_DIR", "ZIG_LOCAL_CACHE_DIR",
)


def run(command, env, *, timeout=120, check=True):
    return subprocess.run(
        [str(part) for part in command], cwd=ROOT, env=env,
        capture_output=True, text=True, timeout=timeout, check=check,
    )


def output(command, env):
    return run(command, env).stdout.strip()


def digest(data):
    return hashlib.sha256(data).hexdigest()


def compile_command(language, source, binary, scratch, tools):
    if language == "rust":
        return [tools["rust"], "--edition=2024", "-C", "opt-level=0", "-C",
                "debuginfo=2", source, "-o", binary]
    if language in ("zig", "zig-seeded"):
        return [tools["zig"], "build-exe", source, "-O", "debug", "-fno-incremental",
                "--cache-dir", scratch / "local", "--global-cache-dir", scratch / "global",
                f"-femit-bin={binary}"]
    return [tools["node"], ROOT / "packages/cli/dist/bin.js", "build-exe", source,
            "--source-root", DIRECTORY, "--optimization", "debug", "--output", binary]


def measure(workload, language, round_index, position, env, tools):
    # The context manager removes only this sample's owned scratch directory,
    # including caches and outputs, on success or failure. No shared cache is deleted.
    with tempfile.TemporaryDirectory(prefix="silk-cold-comparison-") as temporary:
        scratch = Path(temporary)
        binary = scratch / "program"
        source = DIRECTORY / f"{workload}.{EXTENSIONS[language]}"
        seed = None
        if language == "zig-seeded":
            # Every sample gets an independent seed, so this global cache has
            # never contained any measured fixture. Local caches are distinct too.
            seed_scratch = scratch / "seed"
            seed_scratch.mkdir()
            seed_command = [str(part) for part in compile_command(
                "zig", DIRECTORY / "toolchain-seed.zig", seed_scratch / "program",
                seed_scratch, tools)]
            seed_command[seed_command.index("--global-cache-dir") + 1] = str(scratch / "global")
            seed_start = time.perf_counter_ns()
            seeded = run(seed_command, env)
            seed_ms = (time.perf_counter_ns() - seed_start) / 1_000_000
            seed_execution = run([seed_scratch / "program"], env, timeout=10)
            if seed_execution.stdout or seed_execution.stderr:
                raise RuntimeError("unexpected output from toolchain seed")
            seed = {"command": seed_command, "wallMs": seed_ms,
                    "stdout": seeded.stdout, "stderr": seeded.stderr,
                    "verifiedExitCode": seed_execution.returncode}
        command = [str(part) for part in compile_command(language, source, binary, scratch, tools)]
        load_before = os.getloadavg()
        # Directory setup and program verification are outside the timed interval.
        start = time.perf_counter_ns()
        compiled = run(["/usr/bin/time", "-l", *command], env)
        wall_ms = (time.perf_counter_ns() - start) / 1_000_000
        peak = re.search(r"^\s*(\d+)\s+maximum resident set size$", compiled.stderr, re.MULTILINE)
        if peak is None:
            raise RuntimeError("missing macOS peak RSS observation")
        executed = run([binary], env, timeout=10)
        if executed.stdout or executed.stderr:
            raise RuntimeError("unexpected output from in-memory fixture")
        return {
            "workload": workload, "language": language, "round": round_index,
            "position": position, "command": command, "wallMs": wall_ms,
            "peakRssBytes": int(peak.group(1)), "binaryBytes": binary.stat().st_size,
            "stdout": compiled.stdout, "stderr": compiled.stderr,
            "toolchainSeedOutsideTiming": seed,
            "verifiedExitCode": executed.returncode,
            "loadAverageBefore": load_before, "loadAverageAfter": os.getloadavg(),
        }


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--samples", type=int, default=6)
    parser.add_argument("--workloads", nargs="+", choices=WORKLOADS, default=list(WORKLOADS))
    parser.add_argument("--output", type=Path, help="new JSON result file; refuses overwrite")
    args = parser.parse_args()
    if sys.platform != "darwin":
        parser.error("RSS collection uses macOS /usr/bin/time -l")
    if args.samples < 1:
        parser.error("--samples must be positive")
    if len(set(args.workloads)) != len(args.workloads):
        parser.error("--workloads must not contain duplicates")
    if args.output is not None and args.output.exists():
        parser.error("output already exists; choose a new result path")

    env = {key: value for key, value in os.environ.items() if key not in REMOVED_ENVIRONMENT}
    env.update(CARGO_INCREMENTAL="0", NODE_DISABLE_COMPILE_CACHE="1")
    tools = {name: shutil.which(command) for name, command in
             (("rust", "rustc"), ("zig", "zig"), ("node", "node"))}
    if any(path is None for path in tools.values()):
        parser.error("rustc, zig, and node must be installed and available on PATH")
    # Metadata failure is recorded, not silently discarded; a broken SDK-version
    # probe does not invalidate successful compilation by the installed toolchains.
    sdk = run(["xcrun", "--show-sdk-version"], env, check=False)
    metadata = {
        "startedAt": datetime.now(timezone.utc).isoformat(),
        "host": {
            "platform": platform.platform(), "arch": platform.machine(),
            "cpu": output(["sysctl", "-n", "machdep.cpu.brand_string"], env),
            "memoryBytes": int(output(["sysctl", "-n", "hw.memsize"], env)),
            "loadAverageBefore": os.getloadavg(),
        },
        "versions": {
            "rust": output([tools["rust"], "-vV"], env),
            "zig": output([tools["zig"], "version"], env),
            "zigBackend": next(line for line in output(
                [tools["zig"], "build-exe", "--show-builtin", "-O", "debug"], env
            ).splitlines() if line.startswith("pub const zig_backend")),
            "node": output([tools["node"], "--version"], env),
            "clang": output(["clang", "--version"], env),
            "python": sys.version,
            "sdkProbe": {"status": sdk.returncode, "stdout": sdk.stdout, "stderr": sdk.stderr},
            "silkRevision": output(["git", "rev-parse", "HEAD"], env),
            "silkSourceDiffSha256": digest(run(["git", "diff", "HEAD", "--",
                "packages/compiler/src", "packages/llvm/src", "packages/cli/src"], env).stdout.encode()),
            "silkCliSha256": digest((ROOT / "packages/cli/dist/bin.js").read_bytes()),
            "silkToolchainIdentitySha256": digest((ROOT / "packages/compiler/src/ToolchainIntegrity.generated.ts").read_bytes()),
        },
        "sourceHashes": {f"{workload}.{EXTENSIONS[language]}": digest(
            (DIRECTORY / f"{workload}.{EXTENSIONS[language]}").read_bytes())
            for workload in args.workloads for language in LANGUAGES} | {
                "toolchain-seed.zig": digest((DIRECTORY / "toolchain-seed.zig").read_bytes())},
        "harnessSha256": digest(Path(__file__).read_bytes()),
        "policy": {
            "samples": args.samples,
            "workloads": args.workloads,
            "timed": "fresh compiler process through linked executable; /usr/bin/time wrapper included",
            "caches": "zig: unique empty local AND global caches; zig-seeded: unique local cache and independent global cache seeded only by toolchain-seed.zig outside timing; Silk fresh process without persistent cache; rustc without incremental; Node compile cache disabled",
            "filesystem": "OS page cache is not flushed; installed toolchain/SDK assets remain available",
            "profile": "debug/unoptimized; Rust debuginfo=2, Zig default backend with -O debug, Silk --optimization debug",
            "ordering": "serial builds; rotate language order each round; no warmup samples discarded",
            "correctness": "execute every output outside timed interval; require exit 0 and no stdout/stderr",
            "removedEnvironment": REMOVED_ENVIRONMENT,
        },
    }

    rows = []
    for workload in args.workloads:
        for round_index in range(args.samples):
            for position in range(len(LANGUAGES)):
                language = LANGUAGES[(round_index + position) % len(LANGUAGES)]
                row = measure(workload, language, round_index, position, env, tools)
                rows.append(row)
                print(f"{workload} {language} {round_index + 1}/{args.samples}: {row['wallMs']:.1f} ms",
                      file=sys.stderr, flush=True)

    summaries = []
    for workload in args.workloads:
        for language in LANGUAGES:
            selected = [row for row in rows if row["workload"] == workload and row["language"] == language]
            times = [row["wallMs"] for row in selected]
            summaries.append({
                "workload": workload, "language": language, "samples": len(times),
                "medianMs": statistics.median(times), "minMs": min(times), "maxMs": max(times),
                "medianPeakRssBytes": statistics.median(row["peakRssBytes"] for row in selected),
            })
    targets = []
    for workload in args.workloads:
        times = {row["language"]: row["medianMs"] for row in summaries if row["workload"] == workload}
        targets.append({
            "workload": workload, "silkOverRust": times["silk"] / times["rust"],
            "silkOverZig": times["silk"] / times["zig"],
            "silkOverZigSeeded": times["silk"] / times["zig-seeded"],
            "tenTimesFastestNativeMs": 10 * min(times["rust"], times["zig"], times["zig-seeded"]),
        })
    metadata["host"]["loadAverageAfter"] = os.getloadavg()
    encoded = json.dumps({"metadata": metadata, "summaries": summaries, "targets": targets, "rows": rows}, indent=2)
    if args.output is None:
        print(encoded)
    else:
        args.output.parent.mkdir(parents=True, exist_ok=True)
        with args.output.open("x") as stream:
            stream.write(encoded + "\n")
        print(f"Results: {args.output.resolve()}", file=sys.stderr)


if __name__ == "__main__":
    main()
