"""Cold scalar/string/slice controls using the existing macOS measurement contract.

This is Silk-only: descriptor.silk has no claimed Rust/Zig counterpart. Every binary
must pass its runtime oracle. Timing is opt-in evidence, never a correctness assertion.
"""

import argparse
from datetime import datetime, timezone
import json
import os
from pathlib import Path
import shutil
import statistics
import sys

sys.dont_write_bytecode = True
import run as benchmark


def compiler_fingerprint(directory, suffix):
    # Include untracked actors: git diff alone does not identify new implementation files.
    paths = sorted(path for package in ("compiler", "llvm", "cli")
                   for path in (benchmark.ROOT / "packages" / package / directory).rglob(f"*{suffix}"))
    return benchmark.digest("\n".join(
        f"{path.relative_to(benchmark.ROOT)}:{benchmark.digest(path.read_bytes())}"
        for path in paths).encode())


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--samples", type=int, default=3)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    if sys.platform != "darwin" or args.samples < 1:
        parser.error("requires macOS and a positive sample count")
    if args.output.exists():
        parser.error("output already exists; choose a fresh path")
    node = shutil.which("node")
    if node is None:
        parser.error("node must be on PATH")
    env = {key: value for key, value in os.environ.items()
           if key not in benchmark.REMOVED_ENVIRONMENT}
    env["NODE_DISABLE_COMPILE_CACHE"] = "1"
    workloads = ("minimal", "descriptor")
    metadata = {
        "startedAt": datetime.now(timezone.utc).isoformat(),
        "cpu": benchmark.output(["sysctl", "-n", "machdep.cpu.brand_string"], env),
        "node": benchmark.output([node, "--version"], env),
        "clang": benchmark.output(["clang", "--version"], env),
        "revision": benchmark.output(["git", "rev-parse", "HEAD"], env),
        "compilerSourcesSha256": compiler_fingerprint("src", ".ts"),
        "compilerModulesSha256": compiler_fingerprint("dist", ".js"),
        "compilerDiffSha256": benchmark.digest(benchmark.run([
            "git", "diff", "HEAD", "--", "packages/compiler/src",
            "packages/llvm/src", "packages/cli/src"], env).stdout.encode()),
        "toolchainSha256": benchmark.digest((benchmark.ROOT /
            "packages/compiler/src/ToolchainIntegrity.generated.ts").read_bytes()),
        "sourceHashes": {name: benchmark.digest((benchmark.DIRECTORY /
            f"{name}.silk").read_bytes()) for name in workloads},
        "policy": "fresh Node process and output per sample; native and Node caches disabled; OS page cache retained; serial rotating order; debug; runtime oracle outside timing",
    }
    rows = []
    for round_index in range(args.samples):
        for position in range(len(workloads)):
            workload = workloads[(round_index + position) % len(workloads)]
            row = benchmark.measure(workload, "silk", round_index, position, env, {"node": node})
            rows.append(row)
            print(f"{workload} {round_index + 1}/{args.samples}: {row['wallMs']:.1f}ms", flush=True)
    if (compiler_fingerprint("src", ".ts") != metadata["compilerSourcesSha256"] or
            compiler_fingerprint("dist", ".js") != metadata["compilerModulesSha256"]):
        raise RuntimeError("compiler changed during measurement; refusing mixed-revision results")
    summaries = []
    for workload in workloads:
        selected = [row for row in rows if row["workload"] == workload]
        times = [row["wallMs"] for row in selected]
        summaries.append({"workload": workload, "medianMs": statistics.median(times),
                          "minMs": min(times), "maxMs": max(times),
                          "medianPeakRssBytes": statistics.median(
                              row["peakRssBytes"] for row in selected)})
    args.output.parent.mkdir(parents=True, exist_ok=True)
    with args.output.open("x") as stream:
        json.dump({"metadata": metadata, "summaries": summaries, "rows": rows}, stream, indent=2)
        stream.write("\n")


if __name__ == "__main__":
    main()
