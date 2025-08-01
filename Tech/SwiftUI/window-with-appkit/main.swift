import AppKit

final class ApplicationController: NSObject, NSApplicationDelegate {
  var mainWindow: NSWindow?

  func applicationDidFinishLaunching(_ aNotification: Notification) {

    mainWindow = NSWindow(
      contentRect: NSRect(x: 0, y: 0, width: 300, height: 300),
      styleMask: [.titled, .closable, .resizable, .miniaturizable],
      backing: NSWindow.BackingStoreType.buffered,
      defer: false)
    mainWindow!.orderFrontRegardless()
    mainWindow!.title = "Hello World"

    let myBtn = NSButton(title: "Say Hi", target: self, action: #selector(self.sayHi))

    mainWindow!.contentView!.addSubview(myBtn)

    NSApp.activate(ignoringOtherApps: true)
  }

  @objc
  func sayHi() {
    print("Hi!")
  }

  func applicationWillTerminate(_ aNotification: Notification) {
    print("Terminating")
  }

  func applicationShouldTerminateAfterLastWindowClosed(_ app: NSApplication) -> Bool {
    return true
  }

}

let app = NSApplication.shared

NSApp.setActivationPolicy(.regular)

let controller = ApplicationController()

app.delegate = controller

app.run()
