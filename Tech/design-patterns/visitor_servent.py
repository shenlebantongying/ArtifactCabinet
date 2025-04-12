#!/usr/bin/env python3
from abc import ABC, abstractmethod
from typing import List


class Servent(ABC):
    @abstractmethod
    def accept(self, visitor) -> None:
        pass


class ServentA(Servent):
    def accept(self, visitor) -> None:
        visitor.visitor_concrete_servent_a(self)

    def special_method_A(self) -> str:
        return "A"


class ServentB(Servent):
    def accept(self, visitor) -> None:
        visitor.visitor_concrete_servent_b(self)

    def special_method_A(self) -> str:
        return "n"


class Visitor(ABC):
    @abstractmethod
    def visitor_concrete_servent_a(self, element: ServentA):
        pass

    @abstractmethod
    def visitor_concrete_servent_b(self, element: ServentB):
        pass


class ConcreteVisitor1(Visitor):
    def visitor_concrete_servent_a(self, element) -> None:
        print("Visitr1: ", "-> A")

    def visitor_concrete_servent_b(self, element) -> None:
        print("Visitr1: ", "-> B")


class ConcreteVisitor2(Visitor):
    def visitor_concrete_servent_a(self, element) -> None:
        print("Visitr2: ", "-> A")

    def visitor_concrete_servent_b(self, element) -> None:
        print("Visitr2: ", "-> B")


def client_code(Servents: List[Servent], visitor: Visitor) -> None:
    for serv in Servents:
        serv.accept(visitor)


if __name__ == "__main__":
    servents = [ServentA(), ServentB()]
    visitor1 = ConcreteVisitor1()
    visitor2 = ConcreteVisitor2()

    client_code(servents, visitor1)
    client_code(servents, visitor2)


# original https://refactoring.guru/design-patterns/visitor/python/example
