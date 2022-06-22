def err(s = ""):
    print("Whoops! " + s)


def parse_tags(s: str):
    normal_txt = False
    begin_tag = False
    tag_txt = False
    end_tag = False

    temp_s = ""
    result = []

    for x in s:
        if x == '<':
            if tag_txt == True:
                tag_txt = False
                end_tag = True
                result.append(temp_s)
                temp_s = ""
            elif normal_txt == True:
                normal_txt = False
                begin_tag = True
            else:
                err("<")
        elif x == '>':
            if begin_tag == True:
                begin_tag = False
                tag_txt = True
            elif end_tag == True:
                end_tag = False
                normal_txt = True
            else:
                err(">")
        elif tag_txt == True:
            temp_s += x
        else:
            normal_txt = True

    return result

print(parse_tags("Hello <body>world!</body> Words: <b>monkeys</b>, oranges, conch"))
