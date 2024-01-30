#include <atomic>
#include <cassert>
#include <cstdio>

/// Safe persistent stack
class Stack {
private:
    std::atomic<int> child_count = 0;
    int hd;
    Stack* tl;

    static int recFree(Stack* s)
    {
        if (s == nullptr) {
            return 0;
        }

        printf("Try delete -> %d with child_count -> %d \n", Stack::val(s), s->child_count.load());

        if (s->child_count > 0) {
            s->child_count -= 1;
        }

        if (s->child_count == 0) {
            Stack* tl = s->tl;
            delete s;
            return 1 + Stack::recFree(tl);
        }

        return 0; // not deleting anything
    }

public:
    static Stack* empty()
    {
        return nullptr;
    }

    // ! [[nodiscard]] prevent dangling memory leak
    // push(1,s) -> without a name, it will leak
    [[nodiscard]] static Stack* push(int v, Stack* s)
    {
        auto* t = new Stack {};
        if (s != nullptr) {
            s->child_count++;
        }
        t->hd = v;
        t->tl = s;
        return t;
    };

    static Stack* pop(Stack* s)
    {
        assert(s->tl->child_count > 0);

        s->tl->child_count--;

        if (s->child_count == 0) {
            delete s;
        }

        return s->tl;
    }

    static int val(Stack* s)
    {
        return s->hd;
    }

    static void pprint(Stack* s)
    {
        if (s != nullptr) {
            printf("-> %d ", Stack::val(s));
            pprint(s->tl);

        } else {
            printf("\n");
        }
    }
    // return total number freed.
    static int tryFree(Stack* s)
    {
        if (s->child_count >= 1) {
            return 0;
        } else {
            return recFree(s);
        }
    }
};

int main()
{
    /*

    Two stacks share the same ancestor.
          /-> 3 -> 4 | a
     1-> 2 -> 5 -> 6 | b
          \-> 7      | c

    */

    auto* baseStack = Stack::push(2, Stack::push(1, Stack::empty()));
    auto* aStack = Stack::push(3, Stack::push(4, baseStack));

    // Jump back and adding new data to old stack
    auto* bStack = Stack::push(5, Stack::push(6, baseStack));

    printf("Free aStack -> %d freed\n", Stack::tryFree(aStack));

    printf("Free baseStack -> %d freed\n", Stack::tryFree(baseStack));

    Stack::pprint(bStack);

    return 0;
}
