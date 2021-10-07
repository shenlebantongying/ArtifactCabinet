from typing import Union

def check_result_code(code: str):
    match code:
        case "Ok":
            print("Code passed")
        case _:
            print("Sth wrong")


def type_checker(code: Union[str | int]):
    match code:
        case str():
            print("c is str")
        case int():
            print("c is int")

if __name__ == '__main__':
    check_result_code("Ok")
    check_result_code("What?")

    actions = [{"a": 1, "b": 2}, {"a": 4, "b": 5, "c": 6}, {}]
    for act in actions:
        print(act)
        match act:
            # Trap: the case order is from top to bottom
            case {"a": a, "b": b, 'c': c}:
                print(a, b, c)
            case {"a": a, "b": b}:
                print(a, b)
            case _:
                print("no pattern reached")

    print("type checker:")
    type_checker(1)
    type_checker("asdasd")
