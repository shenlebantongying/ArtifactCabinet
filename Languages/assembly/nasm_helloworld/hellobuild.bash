#!/bin/bash
set -e

nasm -felf64 hello.asm && ld hello.o && ./a.out
#    ^
#  this means format:elf:64 bits
