#include <QCoreApplication>
#include "mynice.h"

int main(int argc, char *argv[]) {
    QCoreApplication app(argc, argv);
    auto ok = new mynice();

    ok->test();
    QCoreApplication::exec();
}
