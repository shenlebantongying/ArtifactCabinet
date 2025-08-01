#include <vector>
#include <memory>
#include <string>

class S{
public:
    S(){};
protected:
    std::vector<int> a;
};

int main() {

    auto b = S();


    std::vector<std::shared_ptr<std::string>> a{};
    for (int i = 0; i < 5; ++i) {
        a.emplace_back(std::make_shared<std::string>(std::to_string(i)));
    }



}
