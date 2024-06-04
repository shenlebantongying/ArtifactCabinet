#include <fmt/format.h>

#include "pdf_outline.h"

int main(int argc, char** argv)
{
    if (argc == 2)
    {
        print_outline(argv[1]);
    }
}
