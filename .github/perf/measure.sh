#!/usr/bin/env bash
# Runs the selfhost workflow's TypeScript CLI commands against pinned compiler sources and prints
# one SELFHOST_CLI_TIMING line per command. Mirrors .github/workflows/selfhost.yml.
set -uo pipefail
out=${RUNNER_TEMP:-/tmp}/selfhost-perf
mkdir -p "$out"
run() {
  local name=$1 heap=$2; shift 2
  if [ "$heap" = 1 ]; then export NODE_OPTIONS=--max-old-space-size=8192; else unset NODE_OPTIONS; fi
  local start end rc
  start=$(date +%s.%N)
  /usr/bin/time -v -o "$out/$name.time" node packages/cli/dist/bin.js "$@" > "$out/$name.log" 2>&1
  rc=$?
  end=$(date +%s.%N)
  local rss compiled tests
  rss=$(grep 'Maximum resident' "$out/$name.time" | awk '{print $NF}')
  compiled=$(grep -m1 '^Compiled ' "$out/$name.log" | sed -E 's/.*\(llvm, [^,]+, ([0-9]+) symbols\)/\1/')
  tests=$(grep -m1 '^Tests ' "$out/$name.log" | sed -E 's/^Tests +//')
  echo "SELFHOST_CLI_TIMING step=$name rc=$rc seconds=$(echo "$end - $start" | bc) maxrss_kb=$rss symbols=$compiled tests=\"$tests\""
  if [ "$rc" != 0 ]; then tail -40 "$out/$name.log"; fi
  if grep -q '^Tests ' "$out/$name.log"; then node .github/scripts/check-selfhost-test-times.mjs "$out/$name.log" || echo "SELFHOST_TEST_TIME_LIMIT_FAILED step=$name"; fi
}
run build 1 build --manifest-path compiler/silk.toml --optimization release-with-debug
run m1 1 test --manifest-path compiler/silk.toml --root src/M1Cases.silk --no-cache
run semantic 1 test --manifest-path compiler/silk.toml --root src/semantic/SemanticCases.silk --no-cache
run target 1 test --manifest-path compiler/silk.toml --root src/semantic/TargetCases.silk --no-cache
run hir 0 test --manifest-path compiler/silk.toml --root src/hir/LoweringCases.silk --no-cache
