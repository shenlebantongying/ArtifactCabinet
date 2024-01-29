import QtQuick
import QtQuick3D

Window {
    visible: true
    title: qsTr("Hello World")
    width: 600
    height: 600
    Row {
        Rectangle {
            width: 300
            height: 600
            View3D {
                anchors.fill: parent

                environment: SceneEnvironment {
                    clearColor: "#555555"
                    backgroundMode: SceneEnvironment.Color
                }

                Node {
                    y: 30
                    z: 50

                    // Note how 2D content is inserted here
                    Rectangle {
                        anchors.centerIn: parent

                        color: "green"
                        width: the_text.width + 10
                        height: the_text.height + 10
                        Text {
                            id: the_text
                            color: "white"
                            text: what_do_you_mean
                        }
                    }
                }

                Model {
                    position: Qt.vector3d(0, 0, 0)
                    scale: Qt.vector3d(1, 1.25, 1)
                    source: "#Cylinder"
                    materials: [
                        PrincipledMaterial {
                            baseColor: "yellow"
                            metalness: 0.5
                            roughness: 0.6
                        }
                    ]
                }

                PointLight {
                    z: 300
                }

                PerspectiveCamera {
                    position: Qt.vector3d(0, 200, 300)
                    Component.onCompleted: lookAt(Qt.vector3d(0, 0, 0))
                }
            }
        }

        // TODO: set bg here? and pass it to frag
        ShaderEffect {
            width: 300
            height: 600
            fragmentShader: "qrc:/hello.frag.qsb"
        }
    }
}
