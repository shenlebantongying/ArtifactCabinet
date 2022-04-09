#include "mainwindow.h"
#include <QPainter>
MainWindow::MainWindow(QWidget *parent)
    : QWidget(parent)
{
}

MainWindow::~MainWindow()
{
}


void MainWindow::paintEvent(QPaintEvent *event){
    Q_UNUSED(event);

    QPainter qp(this);

    QPen pen(Qt::black, 2 , Qt::SolidLine);
    qp.setPen(pen);
    qp.drawLine(0,10,300,400);
}
