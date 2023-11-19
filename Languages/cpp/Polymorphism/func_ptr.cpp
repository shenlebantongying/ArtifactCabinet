#include <cstdio>
#include <functional>

template <class T>
T add(T a, T b)
{
    return a + b;
}

template <class T>
T mul(T a, T b)
{
    return a * b;
}

using biOp_int = std::function<int(int, int)>;

int main()
{
    // this name can be many things
    biOp_int Op = add<int>;
    printf("%d\n", Op(2, 3));

    // swap impl
    Op = mul<int>;
    printf("%d\n", Op(2, 3));
}