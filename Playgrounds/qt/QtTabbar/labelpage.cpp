#include "labelpage.h"

labelPage::labelPage(QWidget *parent, QString s) : QLabel(parent)
{
    setText(s);
}
