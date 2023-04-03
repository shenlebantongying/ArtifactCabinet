#include "StringListModel.h"

// TODO: Lean details of QModelIndex

int
StringListModel::rowCount (const QModelIndex &parent) const
{
  return stringData.size ();
}

int
StringListModel::columnCount (const QModelIndex &parent) const
{
  return 2;
}

QVariant
StringListModel::data (const QModelIndex &index, int role) const
{
  if (!index.isValid ())
    {
      return {};
    };

  if (role == Qt::DisplayRole)
    {
      if (index.column () == 0)
        {
          return stringData[index.row ()].id;
        };

      if (index.column () == 1)
        {
          return stringData[index.row ()].content;
        };
    }
  return {};
}

QVariant
StringListModel::headerData (int section, Qt::Orientation orientation,
                             int role) const
{
  if (role != Qt::DisplayRole)
    return {};

  if (orientation == Qt::Horizontal)
    {
      if (0 == section)
        return "id";
      if (1 == section)
        return "content";
    }
  else
    {
      return QStringLiteral ("Row %1").arg (section + 1);
    }
  return {};
}

void
StringListModel::appendData (const QString &id, const QString &content)
{
  stringData.append (Mystring (id, content));
}

bool
StringListModel::appendRow (const QString &id, const QString &contentt)
{
  beginResetModel ();
  stringData.append (Mystring (id, contentt));
  endResetModel ();
  return true;
}
