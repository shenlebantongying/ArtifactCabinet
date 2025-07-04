// Curiously recurring template pattern

// This enables Base class to access methods of Derived class through template

#include <iostream>
#include <string>
struct IntWrap {
  int v = -1;
};

template <class D>
struct Base {

    IntWrap x = {1}; // Invisible for Derived

     Base(std::ostream& pstream) : pstream(pstream) {};

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

protected: // disable creation of the Base Class
  Base() = default;

private:
    std::ostream& pstream;
};

struct Derived : Base<Derived> {

    IntWrap c = {2}; // Invisible for Derived

    void doSthImpl()
    {
        std::cout << "Derived, x -> " << this->x.v << " c-> " << this->c.v << std::endl;
    };

    template <class T>
    Derived& setX(T&& t)
    {
        c.v = t;
        return *this; // compare this with Base::chainPrint's return
    }
};

int main()
{
    // Ok
    Derived* poly = new Derived(std::cout);

    poly->chainPrint("ok.")
        .chainPrint("what.")
        .setX(3)
        .chainPrint("nice.")
        .chainEnd();

    poly->doSth();

    // !! WRONG -> creating Base is UB!
    Base<Derived>* poly2 = new Base<Derived>(std::cout);
    poly2->doSth();
}
