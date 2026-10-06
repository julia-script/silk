#!/usr/bin/env bash
set -euo pipefail

workload=${1:?workload required}
mode=${2:?cache mode required}
workload_dir=$(dirname "$SELFHOST_MANIFEST")
cache_dir="$RUNNER_TEMP/semantic-cache-$workload"
logs="$RUNNER_TEMP/selfhost-logs"
log="$logs/$workload-$mode.log"
timing="$logs/$workload-$mode.time"

export NODE_OPTIONS="$SELFHOST_NODE_OPTIONS"
# Emission/final caches stay process-local, isolating the semantic cache's benefit.
export SILK_NATIVE_CACHE_DIR=''
export SILK_SEMANTIC_CACHE_DIR="$cache_dir"
case "$mode" in
  disabled) export SILK_SEMANTIC_CACHE=false ;;
  cold) export SILK_SEMANTIC_CACHE=true; test ! -e "$cache_dir" ;;
  warm) export SILK_SEMANTIC_CACHE=true; test -d "$cache_dir" ;;
  *) exit 2 ;;
esac

printf 'Starting %s / %s at %s\n' "$workload" "$mode" "$(date -u +%FT%TZ)"
case "$workload" in
  build)
    /usr/bin/time -v -o "$timing" node packages/cli/dist/bin.js build --manifest-path "$SELFHOST_MANIFEST" --optimization release-with-debug --timings 2>&1 | tee "$log"
    artifact="$workload_dir/build/llvm/x86_64-unknown-linux-gnu/release-with-debug/silk-compiler"
    ;;
  semantic)
    /usr/bin/time -v -o "$timing" node packages/cli/dist/bin.js test --manifest-path "$SELFHOST_MANIFEST" --root src/semantic/SemanticCases.silk --no-cache --timings 2>&1 | tee "$log"
    node "$SELFHOST_TIME_CHECK" "$log"
    artifact="$workload_dir/build/test/llvm/x86_64-unknown-linux-gnu/debug/silk-compiler"
    ;;
  *) exit 2 ;;
esac

sha256sum "$artifact" | cut -d' ' -f1 > "$logs/$workload-$mode.sha256"
if [ "$mode" = disabled ]; then
  # A mislabeled baseline invalidates the experiment even when compilation succeeds.
  if grep -q '^Semantic cache:' "$log"; then
    echo 'Disabled control unexpectedly used semantic persistence' >&2
    exit 1
  fi
  test ! -e "$cache_dir"
elif [ "$mode" = warm ]; then
  grep -Eq 'Semantic cache: [1-9][0-9]* candidates loaded' "$log"
  cmp "$logs/$workload-disabled.sha256" "$logs/$workload-warm.sha256"
  cmp "$logs/$workload-cold.sha256" "$logs/$workload-warm.sha256"
  du -sh "$cache_dir" | tee "$logs/$workload-cache-size.txt"
fi
cat "$timing"
printf 'Completed %s / %s at %s\n' "$workload" "$mode" "$(date -u +%FT%TZ)"
