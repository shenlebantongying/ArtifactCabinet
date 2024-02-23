#include <benchmark/benchmark.h>
#include <fmt/format.h>
#include <fmt/compile.h>
#include <QString>

// Observations as 2024 Feb
// * Simple QString formatting is fast enough that you generally don't need to care
// * But, the overhead of mixing std::string and QString is very high (could be as high as more than 50% slower)
// * Better to uses QString and QString only if you need QString (avoid mix std::strings & QString)

// * FMT_COMPILE may leads to slower code in Debug build

std::string a{
    R"(To use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers from)"
};

std::string b{
    R"(To use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers fromTo use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers from)"
};
std::string c{
    R"(To use the {fmt} library, add fmt/core.h, fmt/format.h, fmt/format-inl.h, src/format.cc and optionally other headers from)"
};

QString qa = QString::fromStdString(a);
QString qb = QString::fromStdString(b);
QString qc = QString::fromStdString(c);


static void fmt_no_compile(benchmark::State& state)
{
    // Perform setup here
    for (auto _ : state)
    {
        std::string r = fmt::format(
            "{}Long text Long text Long text Long text{}Long text Long textLong text Long text{}", a, b,
            c);
        benchmark::DoNotOptimize(r);
    }
}

static void fmt_compile(benchmark::State& state)
{
    // Perform setup here
    for (auto _ : state)
    {
        std::string r = fmt::format(
            FMT_COMPILE(R"({}Long text Long text Long text Long text{}Long text Long textLong text Long text{})"),
            a, b, c);
        benchmark::DoNotOptimize(r);
    }
}

static void fmt_format_to(benchmark::State& state)
{
    // Perform setup here
    for (auto _ : state)
    {
        std::string r;
        fmt::format_to(std::back_inserter(r),
                       R"({}Long text Long text Long text Long text{}Long text Long textLong text Long text{})",
                       a, b, c);
        benchmark::DoNotOptimize(r);
    }
}


static void qstr_arg(benchmark::State& state)
{
    for (auto _ : state)
    {
        QString r = QString(R"(%1Long text Long text Long text Long text%2Long text Long text Long text Long text%3)").
            arg(
                qa, qb, qc);
        benchmark::DoNotOptimize(r);
    }
}

static void qstr_back_to_utf8(benchmark::State& state)
{
    for (auto _ : state)
    {
        QString r = QString(R"(%1Long text Long text Long text Long text%2Long text Long text Long text Long text%3)").
            arg(
                qa, qb, qc);
        std::string rr = r.toStdString();
        benchmark::DoNotOptimize(rr);
    }
}

static void qstr_but_insert_stdstr(benchmark::State& state)
{
    for (auto _ : state)
    {
        QString r = QString(R"(%1Long text Long text Long text Long text%2Long text Long text Long text Long text%3)").
            arg(a.c_str(), b.c_str(), c.c_str());
        std::string rr;
        rr = r.toStdString();
        benchmark::DoNotOptimize(rr);
    }
}

// Register the function as a benchmark
BENCHMARK(fmt_no_compile);
BENCHMARK(fmt_compile);
BENCHMARK(fmt_format_to);
BENCHMARK(qstr_arg);
BENCHMARK(qstr_back_to_utf8);
BENCHMARK(qstr_but_insert_stdstr);

// Run the benchmark
BENCHMARK_MAIN();
