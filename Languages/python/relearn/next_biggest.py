def next_biggest(n: int):
    nl = list(reversed(list(str(n))))

    cur = "0"
    counter = 0
    for x in nl:
        counter += 1
        if x > cur:
            cur = x
        else:
            break

    temp = nl[0]
    nl[0] = nl[counter - 1]
    nl[counter - 1] = temp

    nl[0 : counter - 1] = reversed(sorted(nl[0 : counter - 1]))

    return int("".join(list(reversed(nl))))


print(next_biggest(227543))
