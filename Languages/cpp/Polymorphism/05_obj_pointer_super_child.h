#pragma once

#import <print>
struct Base {
  virtual ~Base() = default;
  virtual void doSth();
};

struct Derived :  Base {
  void doSth() override;
};

struct Derived2 :  Base {
  void doSth() override;
};
