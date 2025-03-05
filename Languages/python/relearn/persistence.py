def explode(n: int) -> list[int]:
    strfied_n = str(n)
    result = []
    for digit in strfied_n:
        result.append(int(digit))
    return result


def persistence(n: int):
    counter = 0

    single_digit_q = False

    print(n)

    if n < 10:
        return 0

    while not single_digit_q:
        temp_n = 1

        for x in explode(n):
            temp_n = x * temp_n

        if temp_n < 10:
            single_digit_q = True

        counter += 1
        n = temp_n

        print(n)

    return counter


print(persistence(86))
