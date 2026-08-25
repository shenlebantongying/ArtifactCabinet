(module
    (import "typst_env" "wasm_minimal_protocol_write_args_to_buffer" (func (param i32)))
    (import "typst_env" "wasm_minimal_protocol_send_result_to_host" (func $typst_send (param i32 i32)))

    (memory (export "memory") 1)
    (data (i32.const 0) "Hello from WebAssembly!")
    (global $hello_pos (mut i32) (i32.const 0))
    (global $hello_len (mut i32) (i32.const 23))

    (type $simple_func (func (result i32)))

    (func (export "hello_via_wasm") (type $simple_func)
        (call
            $typst_send
            (global.get $hello_pos)
            (global.get $hello_len))
        i32.const 0
        return
    )
)
