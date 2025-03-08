// Objective:
// create a binary format with 3 fields with type str, number and hex

#include <QDebug>
#include <QFile>


using u32 = quint32;

using InfoStorageType = std::array<u32, 3>;
enum class InfoFields : std::size_t {
    name = 0,
    offset,
    whatever
};

template <size_t SIZE>
void read_all(auto* f_name, std::array<u32, SIZE>& a)
{
    QFile f(std::forward<decltype(f_name)>(f_name));
    f.open(QIODevice::ReadOnly);
    f.read((char*)a.data(), a.size() * sizeof(u32));
    f.close();
}

template <size_t SIZE>
void print_hex_all(std::array<u32, SIZE>& a)
{
    for (int i = 0; i < SIZE; ++i) {
        std::println("{:#010x}", a[i]);
    }
}

template <size_t SIZE>
void writeFields(auto f_name, std::array<u32, SIZE>& a)
{
    QFile f(std::forward<decltype(f_name)>(f_name));
    f.open(QIODevice::WriteOnly);
    f.write((char*)&a, a.size() * sizeof(quint32));
    f.close();
}

int main()
{
    // Create a file
    {
        QFile f("a.bin");
        f.open(QIODevice::WriteOnly);

        int offset = 100;
        int whatever = 0x199911;
        auto signature = "STAR";

        f.write(signature, sizeof(quint32));
        f.write((char*)&offset, sizeof(quint32));
        f.write((char*)&whatever, sizeof(quint32));

        f.close();
    }

    // Recover data from the file
    InfoStorageType internalArray {};
    read_all("a.bin", internalArray);

    // Print recovered data
    print_hex_all(internalArray);

    // Print as according to type
    std::println("\n{}", std::string((char*)&internalArray[static_cast<int>(InfoFields::name)], 4));
    std::println("{}", internalArray[static_cast<int>(InfoFields::offset)]);
    std::println("{:#x}\n", internalArray[static_cast<int>(InfoFields::whatever)]);

    // Output the Array to another file
    writeFields("b.bin", internalArray);

    // Recover again
    InfoStorageType internalArray2 {};
    read_all("b.bin", internalArray2);
    print_hex_all(internalArray2);
}
