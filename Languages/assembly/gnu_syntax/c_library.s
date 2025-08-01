    .global main
    .text

.text

main:
    mov $slbmsg, %rdi
    call puts
    ret
slbmsg:
    .asciz "This is a suppppppppppppppppppppppppppppper looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong message!"
    #    ^ 0 trimed str in C
