#ifndef TASK1_H
#define TASK1_H

#include <QWidget>
#include <QLabel>
#include <QPushButton>
#include <QHBoxLayout>


class Task1 : public QWidget
{
    Q_OBJECT

public slots:
    void slotUpdateNumber();

public:
    Task1(QWidget *parent = nullptr);

private:
    QLabel *counter_label;
    QPushButton *counter;
    QHBoxLayout *hbox;
    int number;
};

int main(int argc, char *argv[]);
#endif // TASK1_H
