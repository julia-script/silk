#!/bin/bash
# shimrun.sh <N0> <src> <label> <thresholdsKB...>
B=$(dirname $0); N0=$1; SRC=$2; L=$3; shift 3
LD_PRELOAD=$B/shim.so "$N0" build "$SRC" -o $B/out/$L.s.ll --stdlib /home/user/silk/packages/compiler/stdlib --emit llvm-ir > /dev/null 2> $B/out/$L.s.err &
pid=$!; sleep 1; cp /proc/$pid/maps $B/shim/$L.maps
for th in "$@"; do
  while kill -0 $pid 2>/dev/null; do
    r=$(awk '/VmRSS/{print $2}' /proc/$pid/status 2>/dev/null); r=${r:-0}
    [ $r -ge $th ] && break; sleep 2
  done
  kill -0 $pid 2>/dev/null || break
  kill -USR1 $pid; sleep 3; mv $B/shim/dump.$pid $B/shim/$L.$th; echo "dumped $th at rss $r"
done
kill -9 $pid 2>/dev/null; echo done
