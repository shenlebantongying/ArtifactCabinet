// Curiously recurring template pattern

// This enables Base class to access methods of Derived class through template

#include <iostream>
#include <string>

template <class D>
struct Base {

    int x = 1; // Invisible for Derived

    Base(std::ostream& pstream)
        : pstream(pstream) {};

    void doSth()
    {
        std::cout << "Base's doSth -> ";
        static_cast<D*>(this)->doSthImpl(); // <- doSthImpl doesn't exist yet yet
    }

    // This method is implemented for Derived to use,
    // To make it chainable,
    // returns this as derived class
    template <typename T>
    D& chainPrint(T&& t)
    {
        pstream << t;
        return static_cast<D&>(*this);
    }

    void chainEnd()
    {
        pstream << std::endl;
    }

private:
    std::ostream& pstream;
};

struct Derived : Base<Derived> {

    int x = 2;

    void doSthImpl()
    {
        std::cout << "x -> " << this->x << std::endl;
    };

    template <class T>
    Derived& setX(T&& t)
    {
        x = t;
        return *this; // compare this with Base::chainPrint's return
    }
};

int main()
{
    // Ok
    auto poly = new Derived(std::cout);

    poly->chainPrint("ok.")
        .chainPrint("what.")
        .setX(3)
        .chainPrint("nice.")
        .chainEnd();

    poly->doSth();

    // !! WRONG -> access x is invalid!
    auto poly2 = new Base<Derived>(std::cout);
    poly2->doSth();
}
