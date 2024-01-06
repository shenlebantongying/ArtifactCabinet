#include <vector>
#include <memory>
#include <string>
#include <iostream>

// This template provides a common internal structure and an algorithm -> for_each
template<class Thing>
class OwingSeq {
private:
    std::vector<std::unique_ptr<Thing>> things;
protected:
    // TODO: check concept in func parameters
    void for_each(std::invocable<Thing const&> auto f) const{
        for (std::unique_ptr<Thing> const& t : things) {
            f(*t);
        }
    }
public:
    void insert(std::unique_ptr<Thing> && t){
        things.push_back(std::move(t));
    };
};

class Toys: public OwingSeq<std::string>{
public:
    void print_seq(){
        for_each([](std::string const& a){
            std::cout<< a << std::endl;
        });
    }
};

int main(){
    auto* y = new Toys();

    auto s1 = std::make_unique<std::string>("nice");

    std::cout<< *s1 << std::endl;
    y->insert(std::move(s1));

    y->print_seq();

    delete y;
}
