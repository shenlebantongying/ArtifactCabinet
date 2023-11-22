#include "task2.h"
#include <QApplication>
#include <QDoubleValidator>

task2::task2(QWidget *parent) : QWidget(parent)
{
    this -> setWindowTitle("Task2");
    this -> resize(300,100);
    QHBoxLayout *hbox = new QHBoxLayout;
    this->setLayout(hbox);

    c_label = new QLabel("Celsius =");
    t_label = new QLabel("Fahrenheit");

    c_edit = new QLineEdit;
    f_edit = new QLineEdit;

    connect(c_edit,SIGNAL(textEdited(QString)),this,SLOT(update_F(QString)));
    connect(f_edit,SIGNAL(textEdited(QString)),this,SLOT(update_C(QString)));


    // Number only :)
    QDoubleValidator *number_only = new QDoubleValidator(__FLT_MIN__,__FLT_MAX__,5, this);

    c_edit->setValidator(number_only);
    f_edit->setValidator(number_only);

    hbox->addWidget(c_edit);
    hbox->addWidget(c_label);
    hbox->addWidget(f_edit);
    hbox->addWidget(t_label);
}

void task2::update_C(QString F_s){
    float F = F_s.toDouble();
    this->c_edit->setText(QString::number((F-32.0)*(5.0/9.0)));
}

void task2::update_F(QString C_s){
    float C = C_s.toDouble();
    this->f_edit->setText(QString::number(C*(9.0/5.0)+32.0));
}

int main(int argc, char *argv[])
{
    QApplication a(argc, argv);
    task2 w;
    w.show();
    return a.exec();
}
