#!/bin/bash
# count.sh <N0> <src> <label> [limitKB]
B=$(dirname $0); mkdir -p $B/out
COUNT_OUT=$B/out/$3.count2 LIMIT_KB=${4:-10000000} gdb -q -batch -x $B/count2.py --args "$1" build "$2" -o $B/out/$3.c.ll --stdlib /home/user/silk/packages/compiler/stdlib --emit llvm-ir > $B/out/$3.gdb.log 2>&1
