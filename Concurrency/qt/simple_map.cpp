#include <QCoreApplication>
#include <QDebug>
#include <QtConcurrent/QtConcurrentMap>

auto square_inplace(double &i) { i = std::pow(i, 2); }

double square_ret(const double &i) { return std::pow(i, 2); }

void sum(double &result, const double &temp) { result += temp; }

int main() {
  QList<double> origin;
  for (int i = 0; i < 100; ++i) {
    origin.emplace_back(i);
  }

  auto origin2 = origin;

  QFuture<void> f = QtConcurrent::map(origin, square_inplace);
  f.waitForFinished();
  qDebug() << origin;

  f.waitForFinished();

  QFuture<double> f2 = QtConcurrent::mapped(origin2, square_ret);
  qDebug() << f2.results<>();

  QFuture<double> f3 = QtConcurrent::mappedReduced(origin2, square_ret, sum);
  qDebug() << f3.results<>();
}
