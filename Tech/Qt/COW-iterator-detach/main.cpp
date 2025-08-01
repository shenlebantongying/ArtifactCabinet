#include <QCoreApplication>
#include <QDebug>
#include <QList>

struct C
{
    C(int a)
    {
    };
    ~C() = default;

    C(const C&)
    {
        qDebug() << "Copy Construct";
    }

    C& operator=(const C&)
    {
        qDebug() << "Copy Assign";
    };
};

int main(int argc, char* argv[])
{
    QList<C> a({C{0}, C{0}, C{0}});

    qDebug() << "Copy B";

    auto b = a; // increase interal ref count

    qDebug() << "For loop";

    // NOTE: without as_const, this causes copying
    for (const auto& o : std::as_const(a))
    {
    }
}
