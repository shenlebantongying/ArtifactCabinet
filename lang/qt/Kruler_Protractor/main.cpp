#include <QApplication>
#include "protractor.h"

int main(int argc, char *argv[]) {
    QApplication a(argc, argv);
    Protractor c(nullptr);
    c.show();
    return QApplication::exec();
}
