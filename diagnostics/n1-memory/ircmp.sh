#!/bin/bash
# ircmp.sh <N0a> <N0b> <tag>: emit llvm-ir for every example with both compilers and compare
S=$(dirname $0); A=$1; Bc=$2; T=$3; same=0; diff=0
for f in $(find /home/user/silk/examples -name "main.silk" | sort); do
  n=$(echo $f | sed 's|/home/user/silk/examples/||; s|/main.silk||; s|/|_|g')
  rm -f $S/ir/$n.base.ll $S/ir/$n.$T.ll
  $A build $f -o $S/ir/$n.base.ll --stdlib /home/user/silk/packages/compiler/stdlib --emit llvm-ir >/dev/null 2>$S/ir/$n.base.err; ea=$?
  $Bc build $f -o $S/ir/$n.$T.ll --stdlib /home/user/silk/packages/compiler/stdlib --emit llvm-ir >/dev/null 2>$S/ir/$n.$T.err; eb=$?
  if [ $ea = $eb ] && { { [ ! -e $S/ir/$n.base.ll ] && [ ! -e $S/ir/$n.$T.ll ]; } || cmp -s $S/ir/$n.base.ll $S/ir/$n.$T.ll; } && cmp -s $S/ir/$n.base.err $S/ir/$n.$T.err; then same=$((same+1)); else diff=$((diff+1)); echo "DIFF $n exit $ea/$eb"; fi
done
echo "$T: identical=$same different=$diff"
