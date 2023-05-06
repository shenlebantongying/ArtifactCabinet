#include "StringListModel.h"

#include <QApplication>
#include <QFileSystemModel>
#include <QSplitter>
#include <QTableView>
#include <QWidget>

int
main (int argc, char *argv[])
{
  QApplication a (argc, argv);

  auto *model = new StringListModel ();

  model->appendData ("1", "Darwin");
  model->appendData ("2", "Lorance");
  model->appendData ("3", "Morty");

  auto *view = new QTableView ();

  view->setModel (model);

  model->appendRow ("4", "Junk");

  view->show ();

  return QApplication::exec ();
}
