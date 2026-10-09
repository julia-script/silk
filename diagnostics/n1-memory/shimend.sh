#!/bin/bash
# shimend.sh <src> <label>: dump live sites right before exit by polling until the .ll is written
B=$(dirname $0); S=$(dirname $B)
rm -f $B/out/$2.e.ll
LD_PRELOAD=$B/shim.so $S/N0 build "$1" -o $B/out/$2.e.ll --stdlib /home/user/silk/packages/compiler/stdlib --emit llvm-ir >/dev/null 2>&1 &
pid=$!; sleep 0.3; cp /proc/$pid/maps $B/shim/$2.maps
peak=0
while kill -0 $pid 2>/dev/null; do r=$(awk '/VmRSS/{print $2}' /proc/$pid/status 2>/dev/null); r=${r:-0}; if [ $r -gt $peak ]; then peak=$r; kill -USR1 $pid; sleep 0.5; [ -f $B/shim/dump.$pid ] && mv $B/shim/dump.$pid $B/shim/$2.peak; fi; sleep 0.5; done
echo "$2 peak=$peak"
