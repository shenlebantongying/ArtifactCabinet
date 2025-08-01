#!/bin/sh
set -e
name=$(basename "${1}" .s)
gcc "${name}.s"  -no-pie -static
./a.out

# TODO: What is ld for asm?
# ld "${name}.o"
# Diff between `gcc -c` or `gcc` ?
