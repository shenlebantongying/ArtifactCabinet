// swift-tools-version: 5.10

import PackageDescription

let package = Package(
    name: "play",
    platforms: [.macOS(SupportedPlatform.MacOSVersion.v14)],
    targets: [
        .executableTarget(
            name: "bidir-sliders",
            path: "./bidir-sliders/"),
    ]
)
