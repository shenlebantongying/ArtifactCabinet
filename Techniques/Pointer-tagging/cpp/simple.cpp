#include <cstdlib>
#include <vector>

// TODO is `uintptr_t` used correctly?
// TODO review little endian again

enum T {
    tInt = 0x1,
    tStr = 0x2,
    tAny = 0x3,
};

void* tag(void* p, T mask)
{
    return (void*)(mask | ((uintptr_t)(p) << 16));
};

void* getPtr(void* p)
{
    return (void*)((uintptr_t)p >> 16);
}

T getTag(void* p)
{
    return static_cast<T>((uintptr_t)p & 0xf);
}

void* newInt(int n)
{
    int* a = (int*)malloc(sizeof(int));
    *a = n;
    return tag(a, tInt);
}

void* newStr(char* s)
{
    return tag(s, tStr);;
}

void visitTypes(const std::vector<void*>& i)
{
    printf("visit\n");
    for (const auto& a : i) {
        switch (getTag(a)) {
        case tInt: {
            auto* p = (int*)getPtr(a);
            printf("visit -> %d\n", *p);
        }
        break;
        case tStr: {
            auto* p = (char*)getPtr(a);
            printf("visit -> %s\n", p);
        }
        break;
        default:
            throw std::runtime_error("no idea");
        }
    }
}

struct dataTemp {
    int f1;
    int f2;
};

int main()
{
    auto* a = (dataTemp*)malloc(sizeof(dataTemp));

    a->f1 = 123;
    a->f2 = 456;

    printf("0x%lx uintptr_t\n", sizeof(uintptr_t));

    printf("0x%p original \n", a);
    printf("0x%lx sizeof original pointer\n", sizeof(dataTemp*));

    void* tagged = tag(a, tAny);
    printf("0x%p Tagged\n", tagged);
    printf("0x%lx sizeof tagged\n", sizeof(tagged));

    printf("0x%d Tag\n", getTag(tagged));

    printf("0x%p RemoveTag\n", getPtr(tagged));
    printf("%d %d data recovered\n", ((dataTemp*)getPtr(tagged))->f1, ((dataTemp*)getPtr(tagged))->f2);

    free(a);

    std::vector<void*> v{};
    v.push_back(newInt(123));
    v.push_back(newInt(456));
    char* ts = (char*)"nice";
    v.push_back(newStr(ts));
    v.push_back(newStr(ts));

    visitTypes(v);

    return 0;
}
