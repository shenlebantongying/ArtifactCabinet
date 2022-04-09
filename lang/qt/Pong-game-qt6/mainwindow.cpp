#include "mainwindow.h"
#include <QBrush>
#include <QCursor>
#include <QGraphicsScene>
#include <QGraphicsView>
#include <QMouseEvent>
#include <QPainter>
#include <QRectF>
#include <QShortcut>
#include <iostream>

const int WIDTH = 800;
const int HEIGHT = 300;
const int Paddle_w = 150;
const int Paddle_h = 10;
const int velocity = 2;
const int ball_d = 20;

MainWindow::MainWindow(QWidget *parent) : QMainWindow(parent), count(0) {

  // [Global settings]
  // ===========================================================
  count = 0;
  // Esc to quit
  auto *shortcut = new QShortcut(QKeySequence(Qt::Key_Escape), this);
  connect(shortcut, &QShortcut::activated, []() { exit(0); });

  // Center mainland
  move(screen()->geometry().center() - frameGeometry().center());

  // Hide Cursor
  QCursor cursor(Qt::BlankCursor);
  setCursor(cursor);

  this->setFixedSize(WIDTH + 10, HEIGHT + 10);

  timer = new QTimer(this);
  timer->start(5);
  connect(timer, SIGNAL(timeout()), this, SLOT(moveBall()));

  // todo: C++ throw away variable?
  auto blackBrush = QBrush(Qt::SolidPattern);
  blackBrush.setColor(QColor(3, 3, 3));
  auto scene = new QGraphicsScene(this);

  scene->setBackgroundBrush(blackBrush);
  scene->setSceneRect(0, 0, WIDTH, HEIGHT);

  // [Add ball and paddle]
  // =======================================================

  QBrush tmpBrush(Qt::SolidPattern);
  tmpBrush.setColor(Qt::white);
  QPen tmpPen;
  tmpPen.setColor(Qt::blue);
  tmpPen.setWidth(2);

  paddle =
      scene->addRect(0, HEIGHT - Paddle_h, Paddle_w, HEIGHT, tmpPen, tmpBrush);
  ball = scene->addEllipse(0, 0, ball_d, ball_d, tmpPen, tmpBrush);

  // [setup view]
  // ================================================================
  auto view = new QGraphicsView();
  view->setMouseTracking(true);
  view->setViewportUpdateMode(QGraphicsView::FullViewportUpdate);
  view->setRenderHint(QPainter::Antialiasing);
  view->setScene(scene);
  view->viewport()->installEventFilter(this);

  this->setCentralWidget(view);
}

void MainWindow::moveBall() {

  // todo: optimize this
  if ((ball->y() + ball_d) > HEIGHT) {
    std::cout << "End, Your score ->" << count << std::endl;
    ball->setPos(WIDTH / 2.0, 0);
    count = 0;
  } else if (ball->collidesWithItem(paddle)) {
    dir_y *= -1;
    count += 1;
  } else if (ball->y() < 0) {
    dir_y *= -1;

  } else if (ball->x() < 0 || ball->x() > WIDTH) {
    dir_x *= -1;
  }

  ball->moveBy(1 * dir_x * velocity, 1 * dir_y * velocity);
}

MainWindow::~MainWindow() = default;

bool MainWindow::eventFilter(QObject *obj, QEvent *event) {
  if (event->type() == QEvent::MouseMove) {
    auto *mouseEvent = dynamic_cast<QMouseEvent *>(event);
    if (mouseEvent->pos().x() <= WIDTH - Paddle_w)
      paddle->setPos(mouseEvent->pos().x(), Paddle_h - 10);
  }
  return false;
}
