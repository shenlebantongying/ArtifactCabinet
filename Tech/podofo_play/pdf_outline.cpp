#include "pdf_outline.h"
#include "fmt/printf.h"

void print_outline(const std::string_view& path)
{
    auto doc = new PoDoFo::PdfMemDocument;
    doc->Load(path);
    if (doc->GetInfo() != nullptr)
    {
        fmt::print("{}!\n", "Not null");
        fmt::print("{}\n", doc->GetInfo()->GetCreator()->ToString());
    }
    auto root = doc->GetOrCreateOutlines().First();

    auto o = root;
    int counter = 0;

    while (o != nullptr)
    {
        fmt::print("{}-> {}\n", std::string(counter, '\t'), o->GetTitle().ToString());
        if (o->First() != nullptr)
        {
            o = o->First();
            counter += 1;
        }
        else if (o->Next() == nullptr && o->GetParentOutline() != root)
        {
            o = o->GetParentOutline()->Next();
            counter -= 1;
        }
        else
        {
            o = o->Next();
        }
    }
}
