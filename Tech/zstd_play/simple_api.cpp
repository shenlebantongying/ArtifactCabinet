#include <iostream>
#include <vector>
#include "zstd.h"

using vc = std::vector<char>;

int main() {
    std::string original = "WHAT IS THIS SENTENCE?";

    vc compressed;
    vc decompressed;

    auto compressBound = ZSTD_compressBound(original.size());
    compressed.resize(compressBound);
    decompressed.resize(original.size());

    auto compressedSize = ZSTD_compress(compressed.data(), compressed.size(), original.data(), original.size(), 10);
    ZSTD_decompress(decompressed.data(), decompressed.size(), compressed.data(), compressedSize);

    std::cout << decompressed.data();

    return 0;
}
