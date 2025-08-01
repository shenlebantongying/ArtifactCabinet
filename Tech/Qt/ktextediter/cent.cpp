#include "cent.h"
#include <KTextEditor/Document>
#include <KTextEditor/Editor>
#include <KTextEditor/View>

cent::cent(QWidget *parent) {
    KTextEditor::Editor *editor = KTextEditor::Editor::instance();
    KTextEditor::Document *doc = editor->createDocument(this);
    KTextEditor::View *view = doc->createView(this->centralWidget());
    this->setCentralWidget(view);

    this->show();
}

cent::~cent() = default;
