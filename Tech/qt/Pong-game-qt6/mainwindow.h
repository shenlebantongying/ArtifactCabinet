#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QGraphicsItem>
#include <QGraphicsView>
#include <QMainWindow>
#include <QTimer>
class MainWindow : public QMainWindow {
  Q_OBJECT

public:
  explicit MainWindow(QWidget *parent = nullptr);
  ~MainWindow() override;

public slots:
  void moveBall();

private:
  QGraphicsItem *paddle{};
  QGraphicsItem *ball{};
  QTimer *timer{};
  unsigned int count;
  int dir_x{1};
  int dir_y{1};
  bool eventFilter(QObject *obj, QEvent *event) override;
};
#endif // MAINWINDOW_H
