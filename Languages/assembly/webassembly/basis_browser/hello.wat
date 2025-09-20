(module
    (func $my_print (import "m" "pprint") (param i32))

    (func (export "printer") (param i32)
        local.get 0
        call $my_print
    )

    (func (export "hello") (result i32)
        i32.const 32
    )
)
