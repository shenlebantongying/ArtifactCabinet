#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QTabWidget>
#include <QTabBar>
#include <QToolButton>
#include <QDebug>
#include <iostream>
#include <QMainWindow>
class MainWindow : public QMainWindow
{
    Q_OBJECT

public:
    MainWindow(QWidget *parent = nullptr);
    ~MainWindow();
public slots:
    void tabClose(int index);
    void tabAdd();


private:
    QTabWidget m_tabber;
    QToolButton tab;

};
#endif // MAINWINDOW_H
