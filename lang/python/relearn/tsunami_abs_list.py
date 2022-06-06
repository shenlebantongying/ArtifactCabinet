def turn_cap_at(s: str, i: int):
    return s[:i] + s[i].capitalize() + s[i + 1:]


print(list(map(turn_cap_at, ["word"] * len("word"), range(len("word")))))
