# A little bit assembly

## x86 assembly
+ `gas/as` -> Dominate the Unix world.
+ `nasm`/`masm` -> intel -> popular on windows

A 64 bits register can hold 2^64 = `18446744073709551616 (x*10^19) `-> maximum bytes a x64 CPU can access (32bits CPU can only access `~4GB`)

64 bits = 8 bytes (aka unsigned long long type in C)

Shorter reference card -> https://web.stanford.edu/class/cs107/resources/x86-64-reference.pdf

### Syscall numbers
`/usr/include/asm/unistd_64.h` -> Complete list of syscall for linux

or collected by chromium project -> <https://chromium.googlesource.com/chromiumos/docs/+/HEAD/constants/syscalls.md>

## wasm
## llvm assembly
