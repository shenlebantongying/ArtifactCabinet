#include <QApplication>
#include "task1.h"

Task1::Task1(QWidget *parent)
    : QWidget(parent)
{
    number = 0;
    hbox = new QHBoxLayout;
    counter_label = new QLabel(QString::number(number));

    this -> setWindowTitle("Task1");
    this -> resize(300,100);
    this->setLayout(hbox);

    counter = new QPushButton("count");

    connect(counter,SIGNAL(pressed()),this,SLOT(slotUpdateNumber()));

    hbox -> addWidget(counter_label);
    hbox -> addWidget(counter);
}

void Task1::slotUpdateNumber(){
    number=number+1;
    this->counter_label->setText(QString::number(number));
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    Task1 w;
    w.show();
    return a.exec();
}
