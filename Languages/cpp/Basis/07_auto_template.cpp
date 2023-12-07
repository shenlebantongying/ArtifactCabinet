#include <iostream>
#include <functional>

// --------------------------------------------------------------------
template<auto A, auto B>
// can only deduce values this way
auto doSth(decltype(A)) -> decltype(A) {
    return A + B;
}

// --------------------------------------------------------------------

template<typename A, typename B>
auto doSth2(A a, B b) {
    static_assert(std::is_same<A, B>::value == true
                  && std::is_integral_v<A> == true
                  && std::is_integral_v<B> == true);
    return a + b;
}

// --------------------------------------------------------------------
// concept requires

// check if a type has a field named v
template<class T>
concept HasV = requires(T a) { T::v; };

template<typename T, T V>
struct p {
    T v = V;
};

template<typename T, typename T2>
requires HasV<T> && HasV<T2>
auto doSth3(T a, T2 b) -> decltype(a.v) {
    return a.v + b.v;
}

// --------------------------------------------------------------------
// uses with function pointer
template<typename T, T (*f)(T, T)>
auto doSth4(T a, T b) {
    return f(a, b);
}

template<typename T>
T a(T x, T y) {
    return x + y;
}

int main() {
    std::cout << doSth<10, 100>(1) << std::endl;

    // note that there is no need to specify types here.
    std::cout << doSth2(10, 100) << std::endl;

    // 2 different types
    std::cout << doSth3(p<int, 10>(), p<int, 100>()) << std::endl;

    // func ptr
    std::cout << doSth4<int, [](int a, int b) { return a + b; }>(10, 100) << std::endl;
}
