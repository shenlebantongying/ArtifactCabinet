#ifndef CIRCLE_H
#define CIRCLE_H

#include <QWidget>
#include <QWindow>
#include <QPainter>
#include <QLayout>


class Protractor : public QWidget
{
    Q_OBJECT
protected:
    void resizeEvent(QResizeEvent *event) override;

    void mouseMoveEvent(QMouseEvent *event) override;

public:
    explicit Protractor(QWidget *parent = nullptr);
    void paintEvent(QPaintEvent *) override;

private:
    QColor mColor;
    void drawCircle(QPainter &painter);
};

#endif // CIRCLE_H
