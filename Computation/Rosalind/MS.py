# No recursion, Iterative, Button-Up Merge Sort.
# TODO: in-place without temp copying?
import numpy as np


def merge_sort(array: np.ndarray):
    def merge(begin, end):
        mid = np.floor_divide(begin + end, 2)
        if mid >= len(array):
            return
        if end > len(array):
            end = len(array)

        temp = np.zeros(end - begin, dtype=np.int64)
        i_a = begin
        i_b = mid
        i_t = 0

        while i_a < mid or i_b < end:
            if i_a == mid:
                temp[i_t:] = array[i_b:end]
                break
            elif i_b == end:
                temp[i_t:] = array[i_a:mid]
                break
            elif array[i_a] < array[i_b]:
                temp[i_t] = array[i_a]
                i_t += 1
                i_a += 1
            else:
                temp[i_t] = array[i_b]
                i_b += 1
                i_t += 1
        array[begin:end] = temp

    bucket_sizes = [2]
    while bucket_sizes[-1] < len(array):
        bucket_sizes.append(np.multiply(bucket_sizes[-1], 2))

    for bucket in bucket_sizes:
        for i in np.arange(0, bucket_sizes[-1], bucket):
            if i < len(array):
                merge(i, i + bucket)
            else:
                break
    return array


l = np.genfromtxt("./data/rosalind_ms.txt", skip_header=1, dtype=np.int64)
print(" ".join(map(str, merge_sort(l))))
