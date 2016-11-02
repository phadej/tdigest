#!/bin/sh

# Run me with: ./run-bench.sh 2>&1 | tee bench.log.txt

set -ex

BENCH=$(stack path --dist-dir)/build/tdigest-simple/tdigest-simple

time $BENCH +RTS -s -N2 -RTS -naive-rand
time $BENCH +RTS -s -N2 -RTS -tdigest-rand
time $BENCH +RTS -s -N2 -RTS -tdigest-par-rand
