# Assembly

`gcc xxx.s && ./a.out`

ref

* all instructions https://www.felixcloutier.com/x86/
* ELF x64 abi https://gitlab.com/x86-psABIs/x86-64-ABI
* GNU assembler https://sourceware.org/binutils/docs/as/

# Debugger

```sh
gdb a.out
break main
run
stepi
disas main
print $rax    # -> print value
print /x $rax # -> print address
x /gx %rax # -> print addr + val like (address: value)
```

```
gdb -tui a.out

layout asm
layout reg
```