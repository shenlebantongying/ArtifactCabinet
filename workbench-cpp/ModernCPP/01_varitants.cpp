#include <iostream>
#include <variant>
#include <string>

struct tripletVisitor{
    void operator()(int i) {
        std::cout<<"int "<< i<< std::endl;
    }

    void operator()(float f) {
        std::cout<<"float " << f <<std::endl;
    }

    void operator()(const std::string& s) {
        std::cout<<"str " << s << std::endl;
    }
};

int main() {

    std::variant<int,float,std::string> triplet;

    // get the index info
    std::cout<<std::variant_size_v<decltype(triplet)><<std::endl;

    // AKA -> patter matching in other languages
    std::visit(tripletVisitor{},triplet);
    triplet=123.0f;
    std::visit(tripletVisitor{},triplet);
    std::cout<< triplet.index()<<std::endl;
    // the index is 1 based...

    triplet="Good";
    std::visit(tripletVisitor{},triplet);
    std::cout<< triplet.index()<<std::endl;

    return 0;
}
