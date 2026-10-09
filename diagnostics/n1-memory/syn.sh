#!/bin/bash
# syn.sh kind n...
B=$(dirname $0); S=$(dirname $B); k=$1; shift
for n in "$@"; do
  d=$B/syn/$k$n; mkdir -p $d; python3 $B/gen.py $k $n > $d/main.silk; sed "s#src/main.silk#main.silk#" /home/user/silk/compiler/silk.toml > $d/silk.toml
  python3 $B/measure.py $d/time $S/N0 build $d/main.silk -o $d/out.ll --stdlib /home/user/silk/packages/compiler/stdlib --emit llvm-ir
  read m e st < $d/time; cp $d/time.err $d/stderr
  echo "$k n=$n exit=$st peakMB=$((${m:-0}/1024)) secs=$e irKB=$(( $(wc -c < $d/out.ll 2>/dev/null || echo 0) / 1024)) $(grep -o 'SILK_[A-Z_]*={"code":"[A-Za-z]*' $d/stderr | head -1)"
done
