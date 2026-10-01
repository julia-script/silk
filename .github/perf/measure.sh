#!/usr/bin/env bash
# Runs the selfhost workflow's TypeScript CLI commands against pinned compiler sources and prints
# one SELFHOST_CLI_TIMING line per command. Mirrors .github/workflows/selfhost.yml.
#
# usage: measure.sh <label>=<checkout> [<label>=<checkout>] -- <step>...
# Every step runs once per checkout on this runner, alternating which checkout goes first, so
# paired variants share hardware and drift.
set -uo pipefail
out=${RUNNER_TEMP:-/tmp}/selfhost-perf
mkdir -p "$out"
variants=()
while [ "$1" != "--" ]; do variants+=("$1"); shift; done
shift
run() {
  local label=$1 dir=$2 name=$3 heap=$4; shift 4
  if [ "$heap" = 1 ]; then export NODE_OPTIONS=--max-old-space-size=8192; else unset NODE_OPTIONS; fi
  local start end rc log="$out/$label-$name.log"
  start=$(date +%s.%N)
  (cd "$dir" && /usr/bin/time -v -o "$out/$label-$name.time" node packages/cli/dist/bin.js "$@") > "$log" 2>&1
  rc=$?
  end=$(date +%s.%N)
  local rss compiled tests
  rss=$(grep 'Maximum resident' "$out/$label-$name.time" | awk '{print $NF}')
  compiled=$(grep -m1 '^Compiled ' "$log" | sed -E 's/.*\(llvm, [^,]+, ([0-9]+) symbols\)/\1/')
  tests=$(grep -m1 '^Tests ' "$log" | sed -E 's/^Tests +//')
  echo "SELFHOST_CLI_TIMING variant=$label step=$name rc=$rc seconds=$(echo "$end - $start" | bc) maxrss_kb=$rss symbols=$compiled tests=\"$tests\""
  if [ "$rc" != 0 ]; then tail -40 "$log"; fi
  if grep -q '^Tests ' "$log"; then
    (cd "$dir" && node .github/scripts/check-selfhost-test-times.mjs "$log") || echo "SELFHOST_TEST_TIME_LIMIT_FAILED variant=$label step=$name"
  fi
}
step() {
  local label=$1 dir=$2 name=$3
  case $name in
    build) run "$label" "$dir" build 1 build --manifest-path compiler/silk.toml --optimization release-with-debug ;;
    m1) run "$label" "$dir" m1 1 test --manifest-path compiler/silk.toml --root src/M1Cases.silk --no-cache ;;
    semantic) run "$label" "$dir" semantic 1 test --manifest-path compiler/silk.toml --root src/semantic/SemanticCases.silk --no-cache ;;
    target) run "$label" "$dir" target 1 test --manifest-path compiler/silk.toml --root src/semantic/TargetCases.silk --no-cache ;;
    hir) run "$label" "$dir" hir 0 test --manifest-path compiler/silk.toml --root src/hir/LoweringCases.silk --no-cache ;;
  esac
}
round=0
for name in "$@"; do
  order=("${variants[@]}")
  if [ $((round % 2)) = 1 ]; then
    order=()
    for ((i = ${#variants[@]} - 1; i >= 0; i--)); do order+=("${variants[i]}"); done
  fi
  for variant in "${order[@]}"; do step "${variant%%=*}" "${variant#*=}" "$name"; done
  round=$((round + 1))
done
