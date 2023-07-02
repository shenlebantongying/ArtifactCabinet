#include <QCoreApplication>
#include <QDebug>
#include <QtConcurrent/QtConcurrent>
#include <chrono>
#include <thread>


QByteArray convert(std::string_view str){
    qDebug()<< "starts";
    std::this_thread::sleep_for(std::chrono::seconds (3));
    qDebug()<< "sleep ends";
    return QByteArray::fromStdString(str.data());
}

QByteArray appendHello(QByteArray ba){
    ba.append("hello");
    return ba;
}

int main(int argc, char *argv[]) {
    QCoreApplication a(argc, argv);

    auto f = QtConcurrent::run(convert, "holy")
            .then(appendHello);


    qDebug()<<f.result();

    std::exit(0);

    return QCoreApplication::exec();
}
