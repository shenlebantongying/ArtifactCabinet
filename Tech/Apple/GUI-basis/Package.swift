// swift-tools-version: 6.0

import PackageDescription

let package = Package(
  name: "play",
  platforms: [.macOS(SupportedPlatform.MacOSVersion.v14)],
  targets: [
    .executableTarget(
      name: "bidir-sliders",
      path: "./bidir-sliders/"),
    .executableTarget(
      name: "window-with-appkit",
      path: "./window-with-appkit/"),
  ]
)
