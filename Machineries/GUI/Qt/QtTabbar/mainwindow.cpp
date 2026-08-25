#include "mainwindow.h"
#include "labelpage.h"

MainWindow::MainWindow(QWidget *parent)
    : QMainWindow(parent),
      m_tabber(new QTabWidget(this)),
      tab(new QToolButton)
{

    resize(800,600);

    m_tabber.tabBar()->setExpanding(false);

    tab.setIcon(QIcon::fromTheme(QStringLiteral("tab-new")));
    tab.setAutoRaise(true);

    m_tabber.setTabsClosable(true);

    m_tabber.addTab(new labelPage(nullptr ,"ok"),"tab");
    m_tabber.addTab(new labelPage(nullptr ,"123"),"tab2");

    m_tabber.setCornerWidget(&tab,Qt::BottomLeftCorner);
    setCentralWidget(&m_tabber);

    connect(&m_tabber,&QTabWidget::tabCloseRequested,this, &MainWindow::tabClose);
    connect(&tab,&QToolButton::clicked,this,&MainWindow::tabAdd);
}

void MainWindow::tabClose(int index){
    m_tabber.removeTab(index);
}

void MainWindow::tabAdd(){
    m_tabber.addTab(new labelPage(nullptr ,"ok"),"tab1");
}

MainWindow::~MainWindow()
{
}
