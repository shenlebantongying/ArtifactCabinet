#include <QApplication>
#include <QDebug>

#include "cent.h"

int main(int argc, char *argv[]) {
    QApplication a(argc, argv);
    qDebug() << "Hello World";

    auto c = new cent();

    return QApplication::exec();
}
