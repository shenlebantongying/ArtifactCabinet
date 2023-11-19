#include <iostream>

// The Policy will determine the behavior of this class at compile time
// Note how type T is propagated into Policy

template <typename T, template <class> class Policy>
struct Worker : private Policy<T> {
    T doSth(T t)
    {
        return Policy<T>::doThing(t);
    }
};

template <typename T>
struct f {
    T doThing(T t)
    {
        std::cout << "doThing of f" << std::endl;
        return t * 2;
    }
};

template <typename T>
struct y {
    T doThing(T t)
    {
        std::cout << "doThing of y" << std::endl;
        return t + 2;
    }
};

int main()
{
    Worker<int, f>* fw = new Worker<int, f>;
    auto fr = fw->doSth(10);

    Worker<int, y>* yw = new Worker<int, y>;
    auto yr = yw->doSth(10);

    std::cout << fr << " " << yr << std::endl;
}