#!/bin/sh

# Run me with: ./run-bench.sh 2>&1 | tee bench.log.txt

set -ex

BENCH=$(stack path --dist-dir)/build/tdigest-simple/tdigest-simple
SIZE=${SIZE:-50000000}
DISTR=${DISTR:-exponent}

time $BENCH +RTS -s -N2 -RTS -s $SIZE -d $DISTR -m average
time $BENCH +RTS -s -N2 -RTS -s $SIZE -d $DISTR -m vector
time $BENCH +RTS -s -N2 -RTS -s $SIZE -d $DISTR -m sparking
time $BENCH +RTS -s -N2 -RTS -s $SIZE -d $DISTR -m buffered
time $BENCH +RTS -s -N2 -RTS -s $SIZE -d $DISTR -m digest
