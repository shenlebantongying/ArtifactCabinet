#include <re2/re2.h>
#include <cassert>
#include <iostream>
int main(){

    int i;
    std::string s;
    assert(RE2::FullMatch("ruby:1234", "(\\w+):(\\d+)", &s, &i));
    assert(s == "ruby");
    assert(i == 1234);

    std::cout<<i<<" "<<s<<std::endl;

    return 0;
}