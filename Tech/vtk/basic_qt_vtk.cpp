#include <QVTKOpenGLNativeWidget.h>
#include <vtkActor.h>
#include <vtkCamera.h>
#include <vtkCylinderSource.h>
#include <vtkDoubleArray.h>
#include <vtkGenericOpenGLRenderWindow.h>
#include <vtkPointData.h>
#include <vtkAxesActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkRenderer.h>
#include <QPushButton>
#include <QStatusBar>
#include <QApplication>
#include <QDockWidget>
#include <QGridLayout>
#include <QLabel>
#include <QMainWindow>
#include <QPointer>
#include <vtkNamedColors.h>

int main(int argc, char *argv[]) {
    QSurfaceFormat::setDefaultFormat(QVTKOpenGLNativeWidget::defaultFormat());

    QApplication app(argc, argv);

    // Main window.
    QMainWindow mainWindow;
    mainWindow.resize(800, 600);

    QPointer<QStatusBar> statusBar = new QStatusBar();

    mainWindow.setStatusBar(statusBar);
    statusBar->showMessage("nice");

    QPointer<QWidget> mainWidget = new QWidget();
    mainWidget->setLayout(new QVBoxLayout());

    // Render
    QPointer<QVTKOpenGLNativeWidget> vtkRenderWidget =
            new QVTKOpenGLNativeWidget(mainWidget);
    mainWindow.setCentralWidget(mainWidget);

    mainWidget->layout()->addWidget(vtkRenderWidget);

    // VTK
    vtkNew<vtkGenericOpenGLRenderWindow> m_renderWindow;
    vtkRenderWidget->setRenderWindow(m_renderWindow.Get());

    vtkNew<vtkNamedColors> colors;
    std::array<unsigned char, 4> bkg{{30, 30, 100, 255}};
    colors->SetColor("BkgColor", bkg.data());

    vtkNew<vtkCylinderSource> cylinder;
    cylinder->SetResolution(8);

    vtkNew<vtkPolyDataMapper> cylinderMapper;
    cylinderMapper->SetInputConnection(cylinder->GetOutputPort());

    vtkNew<vtkActor> cylinderActor;
    cylinderActor->SetMapper(cylinderMapper);
    cylinderActor->GetProperty()->SetColor(
            colors->GetColor4d("Tomato").GetData());

    vtkNew<vtkRenderer> renderer;
    renderer->AddActor(cylinderActor);
    renderer->SetBackground(colors->GetColor3d("BkgColor").GetData());


    // axes
    vtkNew<vtkAxesActor> axes;

    renderer->AddActor(axes);

    m_renderWindow->AddRenderer(renderer);


    // custom staffs
    m_renderWindow->GetInteractor()->RemoveAllObservers();


    QPointer<QPushButton> rorate1_btn = new QPushButton("Rotate1", mainWidget);
    mainWidget->layout()->addWidget(rorate1_btn);


    QPointer<QPushButton> rorate2_btn = new QPushButton("Rotate2", mainWidget);
    mainWidget->layout()->addWidget(rorate2_btn);


    mainWindow.show();

    vtkCamera *camera = renderer->GetActiveCamera();

    QObject::connect(rorate2_btn, &QPushButton::pressed, [&m_renderWindow, &camera] {
        camera->Elevation(10);
        m_renderWindow->Render();
    });


    QObject::connect(rorate1_btn, &QPushButton::pressed, [&camera, &m_renderWindow] {
        camera->Roll(10);
        m_renderWindow->Render();
    });

    return QApplication::exec();
}
