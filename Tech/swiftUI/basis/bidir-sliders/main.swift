import SwiftUI

// TODO: magic Observable.
@Observable
class appStates {
    let value_min = 0.0
    let value_max = 100.0
    let range: ClosedRange<Double>
    
    var value_1 = 50.0
    var value_2 = 50.0
    var isEditing = false
    
    init() {
        range = value_min...value_max
    }
}

struct ContentView: View {
    
    @State private var S = appStates()
    
    var body: some View {
        VStack {
            Slider(
                value: $S.value_1,
                in: S.range,
                onEditingChanged: { editing in
                    S.isEditing = editing
                }
            )
            
            Slider(
                value: $S.value_2,
                in: S.range,
                onEditingChanged: { editing in
                    S.isEditing = editing
                }
            )
            
            Text("""
                 Slider 1: \(S.value_1)
                 Slider 2: \(S.value_2)
                 """
            )
            .foregroundColor(S.isEditing ? .red : .blue)
        }.onChange(of: S.value_1) { _, newValue in
            S.value_2 = S.value_max - newValue
        }.onChange(of: S.value_2) { _, newValue in
            S.value_1 = S.value_max - newValue
        }
    }
}

struct MyApp: App {
    var body: some Scene {
        WindowGroup {
            ContentView()
                .onDisappear {
                    exit(EXIT_SUCCESS)
                }
        }
    }
}

MyApp.main()
