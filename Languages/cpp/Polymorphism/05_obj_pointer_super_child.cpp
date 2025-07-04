#include "05_obj_pointer_super_child.h"

void  Base::doSth()   {
    std::println("Base");
  }

void Derived::doSth() {
  std::println("Derived");
}

void Derived2::doSth() {
  std::println("Derived2");
}

int main() {
  Base * o = new Derived();
  o->doSth();
  Base * o2 = new Derived2();
  o2->doSth();
}
