
#include <QApplication>

#include "imageviewer.h"

int main(int argc, char *argv[])
{
    QApplication app(argc, argv);
    QGuiApplication::setApplicationDisplayName(ImageViewer::tr("Image Viewer"));
    ImageViewer imageViewer;
    imageViewer.show();
    return QApplication::exec();
}
