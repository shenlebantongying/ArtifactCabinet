#ifndef STRINGLISTMODEL_H
#define STRINGLISTMODEL_H

#include <QAbstractListModel>
#include <QList>
#include <QString>

/* It maybe looks werid that we use a QList for a QTable model.
 * Use List as a container for a bunch of Mystrings which consist of 2 elements.
 *
 * TODO: actual difference between QAbstracListModel vs QAbstractTableModel?
 */

class Mystring
{
public:
  Mystring (const QString &id, const QString &content)
      : id (id), content (content)
  {
    QList<Mystring> *stringData = new QList<Mystring>;
  };
  QString id;
  QString content;
};

class StringListModel : public QAbstractListModel
{
  Q_OBJECT

public:
  StringListModel (QObject *parent = nullptr){};

  int rowCount (const QModelIndex &parent = QModelIndex ()) const override;
  int columnCount (const QModelIndex &parent = QModelIndex ()) const override;
  QVariant data (const QModelIndex &index, int role) const override;
  QVariant headerData (int section, Qt::Orientation orientation,
                       int role = Qt::DisplayRole) const override;

  bool appendRow (const QString &id, const QString &content);
  void appendData (const QString &id, const QString &content);

private:
  QList<Mystring> stringData;
};

#endif // STRINGLISTMODEL_H
