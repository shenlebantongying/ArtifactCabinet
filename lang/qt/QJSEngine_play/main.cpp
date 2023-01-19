#include <QCoreApplication>
#include <QPushButton>
#include <QDebug>
#include <QJSEngine>

int main(int argc, char *argv[]) {
    QCoreApplication a(argc, argv);

    QJSEngine engine;

    auto *myQObject = new QObject();
    myQObject->setProperty("dynamicProperty", 3);

    QJSValue myScriptQObject = engine.newQObject(myQObject);
    engine.globalObject().setProperty("myObject", myScriptQObject);

    qDebug() << engine.evaluate("myObject.dynamicProperty").toInt();

    myScriptQObject.setProperty("dynamicProperty", 3);

    qDebug() << engine.evaluate("myObject.dynamicProperty").toInt();

    return 0;
}
