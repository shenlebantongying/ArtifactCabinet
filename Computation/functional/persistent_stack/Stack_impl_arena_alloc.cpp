#include <cstdio>
#include <list>
#include <optional>

// NOTE: std::vector may invalidate references if relocation happens

template <typename T>
class Arena {
public:
    std::list<T>* pool;
    explicit Arena()
    {
        pool = new std::list<T>();
    };
    ~Arena()
    {
        delete pool;
    };

    T* alloc()
    {
        return &pool->emplace_back();
    }
};

template <typename T>
class Stack {
public:
    std::optional<T> hd;
    Stack<T>* tl = nullptr;
    Arena<Stack<T>>* m_arena = nullptr;

    static Stack* empty(Arena<Stack<T>>* arena)
    {
        Stack<T>* ret = arena->alloc();
        ret->m_arena = arena;
        return ret;
    }

    static Stack<T>* push(T v, Stack<T>* s)
    {
        Stack<T>* t = s->m_arena->alloc();
        t->hd = v;
        t->tl = s;
        t->m_arena = s->m_arena;
        return t;
    }

    void pprint()
    {
        Stack* p = this;

        while (p->hd.has_value()) {
            printf("-> %d ", p->hd.value());
            p = p->tl;
        }

        printf("\n");
    }
};

int main()
{
    auto* myArena = new Arena<Stack<int>>();

    using intStack = Stack<int>;
    auto* base = intStack::push(2, intStack::push(1, intStack::empty(myArena)));

    intStack::push(3, base)->pprint();
    intStack::push(5, intStack::push(4, base))->pprint();

    delete myArena;

    return 0;
}
