cmake_minimum_required(VERSION 3.0)

project (kSimpleEdit)

set(QT_MIN_VERSION "5.3.0")
set(KF5_MIN_VERSION "5.2.0")

find_package(ECM 1.0.0 REQUIRED NO_MODULE)
set(CMAKE_MODULE_PATH ${ECM_MODULE_PATH} ${ECM_KDE_MODULE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

include(KDEInstallDirs)
include(KDECMakeSettings)
include(KDECompilerSettings NO_POLICY_SCOPE)
include(FeatureSummary)

# Find Qt modules
find_package(Qt5 ${QT_MIN_VERSION} CONFIG REQUIRED COMPONENTS
        Core    # QCommandLineParser, QStringLiteral
        Widgets # QApplication
        )

# Find KDE modules
find_package(KF5 ${KF5_MIN_VERSION} REQUIRED COMPONENTS
        CoreAddons      # KAboutData
        I18n            # KLocalizedString
        XmlGui          # KXmlGuiWindow, KActionCollection
        TextWidgets     # KTextEdit
        ConfigWidgets   # KStandardActions
        WidgetsAddons   # KMessageBox
        KIO             # KIO
        )

feature_summary(WHAT ALL INCLUDE_QUIET_PACKAGES FATAL_ON_MISSING_REQUIRED_PACKAGES)

set(kSimpleEdit main.cpp mainwindow.cpp mainwindow.h)
add_executable(kSimpleEditExe ${kSimpleEdit})
target_link_libraries(kSimpleEditExe
        Qt5::Widgets
        KF5::CoreAddons
        KF5::I18n
        KF5::XmlGui
        KF5::TextWidgets
        KF5::ConfigWidgets
        KF5::WidgetsAddons
        KF5::KIOCore
        )

install(TARGETS kSimpleEditExe ${KDE_INSTALL_TARGETS_DEFAULT_ARGS})
install(FILES kSimpleEdit.rc DESTINATION ${KDE_INSTALL_KXMLGUI5DIR}/kSimpleEdit)
