#include <iostream>
#include <libguile.h>


static SCM
nicex(){
 return scm_from_int(3);
}

int main() {
    scm_init_guile();

    scm_c_define_gsubr("nice", 0, 0, 0,  reinterpret_cast<void*>(nicex));

    SCM a =  scm_c_eval_string("(nice)");
    std::cout << scm_to_int(a) << std::endl;;


    return 0;
}
