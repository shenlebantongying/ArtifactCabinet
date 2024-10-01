#include <functional>
#include <print>

// Pre-cpp 17
template <typename H>
auto minus_old(H head)
{
    return head;
}

template <typename H, typename... T>
auto minus_old(H head, T... tail)
{
    return (head - minus_old(tail...));
}

// Post-cpp 17

template <typename H, typename... E>
auto minus_folding(H h, E... t)
{
    return h - (t - ...);
}

// expanding ternary operator via folding:
template <typename... E>
auto ternary_folding(
    std::function<bool(std::common_type_t<E...>)> f,
    E... t)
-> std::tuple<bool, std::common_type_t<E...>>
requires std::is_arithmetic_v<std::common_type_t<E...>>
{
    std::common_type_t<E...> result;
    return {((f(t) ? (result = t, true) : false) || ...), result};
}


int main()
{
    std::println(" 1-(2-(3-(4-5))) -> {}", minus_old(1, 2, 3, 4, 5));

    std::println(" 1-(2-(3-(4-5))) -> {}", minus_folding(1, 2, 3, 4, 5));

    std::println("Firsts even num -> {}", ternary_folding([](int i) { return i % 2 == 0; }, 1, 3, 5, 7, 8));
}
