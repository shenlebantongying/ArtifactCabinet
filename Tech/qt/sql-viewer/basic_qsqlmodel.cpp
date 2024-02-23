#include <QApplication>
#include <QPushButton>
#include <QSqlQuery>
#include <QSqlRecord>
#include <QSqlTableModel>
#include <QTableView>
#include <source_location>

void no(const std::source_location& loc = std::source_location::current())
{
    qFatal() << "Error -> line " << loc.line();
}

#define CHECK_RET(statement) if (not statement) no();

int main(int argc, char* argv[])
{
    QApplication a(argc, argv);

    CHECK_RET(QSqlDatabase::drivers().contains("QSQLITE"))

    QSqlDatabase db = QSqlDatabase::addDatabase("QSQLITE");
    db.setDatabaseName("what");
    CHECK_RET(db.open())


    {
        QSqlQuery drop_table(db);
        CHECK_RET(drop_table.exec(R"(DROP TABLE if exists mytable)"))
    }

    {
        QSqlQuery create_table(db);
        CHECK_RET(create_table.exec(R"(CREATE TABLE if not exists mytable(id integer primary key, data text))"))
    }


    auto* model = new QSqlTableModel(nullptr, db);
    model->setTable("mytable");


    auto N = 10;
    for (int i = 0; i < N; ++i)
    {
        QSqlRecord t_rec = model->record();
        t_rec.setValue("id", i);
        t_rec.setValue("data",QStringLiteral("Ok ->") + QString::number(i));
        CHECK_RET(model->insertRecord(-1, t_rec))
    }


    CHECK_RET(model->submitAll())

    model->select();

    auto* table = new QTableView(nullptr);

    table->setModel(model);
    table->show();

    return QApplication::exec();
}
