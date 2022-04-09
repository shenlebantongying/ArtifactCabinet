#ifndef LABELPAGE_H
#define LABELPAGE_H

#include <QWidget>
#include <QLabel>
#include <QString>

class labelPage : public QLabel
{
    Q_OBJECT
public:
    labelPage(QWidget *parent = nullptr, QString s = "hello");

signals:
};

#endif // LABELPAGE_H
