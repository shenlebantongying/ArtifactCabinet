#include <print>

// inovke an object's method via template

// TODO: arbitrary arg list with folding?

template <class OBJ, class RET, class ARG0>
auto invoke(OBJ& it, RET (OBJ::*func_name)(ARG0), ARG0 arg0) -> RET { return (it.*func_name)(arg0); }

struct C {
    template <class T>
    auto func(T arg1) -> T { return arg1; }
};

int main()
{
    C obj;

    auto str = std::string_view("hello, world!");

    auto v = invoke(obj, &C::func<decltype(str)>, str);

    std::println("{}", v);
}
