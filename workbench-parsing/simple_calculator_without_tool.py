#!/usr/bin/env python3

# Description:
# Simple calculator that supports + - * /
# A practice on parsing without any tools

def slbEval(expr):

    # [Tokenization] ---------------------------------------------------
    tokens = []

    cur_num = ""
    for x in expr:
        if x.isnumeric():
            cur_num += x
        elif x in "+-*/":
            tokens += [int(cur_num)]
            tokens += x
            cur_num = ""
        else:
            print("invalid symbol")
            exit(1)
    tokens += [int(cur_num)]

    print(tokens)
    print(list(range(0, len(tokens))))

    result = tokens[0]
    tdTemp = 0

    # [Calc] ---------------------------------------------------
    # Use each operators as deliminator,
    for i in range(1, len(tokens), 2):

        cur = tokens[i]

        print("| pos->", i, " cur->", cur)

        if (i + 2) >= len(tokens):
            if cur == "*":
                result += tdTemp * tokens[i + 1]
                tdTemp = 0
            elif cur == "/":
                result += tdTemp / tokens[i + 1]
            elif cur == "+":
                result += tokens[i + 1]
            elif cur == "-":
                result -= tokens[i + 1]
            break

        nxt = tokens[i + 2]

        print("| nxt->", nxt)

        if nxt in "+-":
            if cur == "*":
                result += tdTemp * tokens[i + 1]
                tdTemp = 0
            elif cur == "/":
                result += tdTemp / tokens[i + 1]
            elif cur == "+":
                result += tokens[i + 1]
            elif cur == "-":
                result -= tokens[i + 1]

        if nxt in "*/":
            if cur in "+-":
                tdTemp = tokens[i + 1]
            elif cur == "*":
                tdTemp *= tokens[i + 1]
            elif cur == "/":
                tdTemp /= tokens[i + 1]
        print("result", result)
        print("tdTemp", tdTemp)
        print()
    print(result)


slbEval("12+23*32+52*2/3")  # =1066
