#include "protractor.h"
#include <QList>
#include <QtMath>

const int TICK_WIDTH = 20;

Protractor::Protractor(QWidget *parent)
        : QWidget{parent} {
    setAttribute(Qt::WA_TranslucentBackground);
    setMouseTracking(true);
    setMinimumSize(200,200);
    mColor = QColor(255,200,80);
}

void Protractor::drawCircle(QPainter &painter) {

    auto diamater = (float)this->width();
    float radius = diamater / 2;
    float inner_radius = radius - TICK_WIDTH;


    int n = 4 * 30;


    QList<float> m_rads(n);

    for (int i = 0; i < m_rads.size(); ++i) {
        m_rads[i] = 2 * M_PI * i / n;
    }

    QList <QLineF> mCircleTicks(m_rads.size());

    for (int i = 0; i < m_rads.size(); ++i) {
        mCircleTicks[i] = QLineF(inner_radius*qCos(m_rads[i])+radius,
                                 inner_radius*qSin(m_rads[i])+radius,
                                 radius*qCos(m_rads[i])+radius,
                                 radius*qSin(m_rads[i])+radius);
    }

    painter.drawLines(mCircleTicks);
}


double inline qLineLength(QLine l){
    return qSqrt(qPow(l.dx(),2) + qPow(l.dy(),2));
}

void Protractor::paintEvent(QPaintEvent *inEvent)
{
    Q_UNUSED(inEvent);



    int w = this->width();
    int r = w/2;
    QPoint curPos = mapFromGlobal(QCursor::pos());

    QPainter painter(this);

    painter.setRenderHint(QPainter::Antialiasing);

    auto *pen = new QPen();
    auto *brush = new QBrush();
    painter.setPen(*pen);
    painter.setBrush(*brush);

    // draw background
    pen->setWidth(0);
    painter.setPen(*pen);
    painter.setBrush(QBrush(mColor));
    painter.setOpacity(0.8);
    painter.drawEllipse(0,0,this->width(),this->width());

    painter.setOpacity(1);

    pen->setWidth(2);
    pen->setColor(Qt::red);

    painter.setPen(*pen);


    auto tLength = qLineLength(QLine(QPoint(w/2,w/2),curPos));

    if (curPos.x() != 0 && curPos.y() != 0 && tLength!=0) {
        painter.drawLine(QPoint(r,r),
                         QPoint((curPos.x()-r)*r/tLength+r,(curPos.y()-r)*r/tLength+r));
    }

    // draw ticks
    pen->setColor(Qt::black);
    pen->setWidth(1);
    painter.setPen(*pen);
    drawCircle(painter);
}

void Protractor::resizeEvent(QResizeEvent *event) {
    Q_UNUSED(event);
    int h = this->height();
    int w = this->width();
    if (h!=w){
        resize(w,w);
    }
}

void Protractor::mouseMoveEvent(QMouseEvent *event) {
    Q_UNUSED(event);

    update();
}
