#ifndef TASK2_H
#define TASK2_H

#include <QWidget>
#include <QLabel>
#include <QPushButton>
#include <QLineEdit>
#include <QHBoxLayout>
class task2 : public QWidget
{
    Q_OBJECT

public:
    explicit task2(QWidget *parent = nullptr);

private:
    QLabel *c_label;
    QLabel *t_label;
    QLineEdit *c_edit;
    QLineEdit *f_edit;
    QHBoxLayout *hbox;

public slots:
    void update_C(QString F);
    void update_F(QString C);
};

#endif // TASK2_H
