#include "file1.h"

CAMLprim value
fancy_plus(value v1, value v2)
{
    CAMLparam2(v1, v2);

    int v3 = Int_val(v1); // conversion macros are in the headers.s
    int v4 = Int_val(v2);

    CAMLreturn(Val_int(v3+v4));
}
