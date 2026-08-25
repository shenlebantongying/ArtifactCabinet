from collections import Counter
import sys
import logging
import pymupdf
import re


def main():
    try:
        file_name = sys.argv[1]
    except IndexError:
        logging.critical("Must specify a filename.")
        exit(1)

    doc = pymupdf.open(file_name)
    total_counter = Counter()
    for page in doc:
        total_counter = total_counter + count_page_words(page)

    with open("output.txt", "w") as out:
        for k, v in total_counter.most_common():
            out.write(f"{v} -> {k}\n")


def count_page_words(page: pymupdf.Page) -> Counter[str]:
    text = page.get_text()
    segments = re.split(r"[^a-zA-Z0-9]", text)
    segments_no_space = filter(useful_q, segments)
    return Counter(segments_no_space)


def useful_q(s: str) -> bool:
    if len(s) == 0:
        return False

    if s.isdigit():
        return False

    return True


if __name__ == "__main__":
    main()
