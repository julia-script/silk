#!/bin/bash
# usage: run.sh <N0> <entry.silk> <label> [limitKB]
N0=$1; SRC=$2; L=$3; LIMIT=${4:-10000000}
B=$(dirname $0); OUT=$B/out/$L; mkdir -p $B/out
STD=/home/user/silk/packages/compiler/stdlib
start=$(date +%s)
"$N0" build "$SRC" -o $OUT.ll --stdlib $STD --emit llvm-ir > $OUT.stdout 2> $OUT.stderr &
pid=$!
peak=0; : > $OUT.rss
while kill -0 $pid 2>/dev/null; do
  r=$(awk '/VmRSS/{print $2}' /proc/$pid/status 2>/dev/null); r=${r:-0}
  [ $r -gt $peak ] && peak=$r
  t=$(( $(date +%s) - start )); echo "$t $r" >> $OUT.rss
  if [ $r -gt $LIMIT ]; then kill -9 $pid; echo "KILLED" >> $OUT.rss; fi
  sleep ${INTERVAL:-15}
done
wait $pid; st=$?
echo "$L exit=$st peak_kb=$peak secs=$(( $(date +%s) - start ))" | tee -a $B/results.txt
