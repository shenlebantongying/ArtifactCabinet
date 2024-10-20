As of June 12, 2024, due to bugs related to the entry point,
the code used for startup must be named exactly as `main.swift`,
so that both CLI-only and xcode build both works.

# Build

## CMake

To generate XCode project
```
cmake -G 'Xcode' ../
```

## SwiftPM

```
swift run ${.executableTarget's name}
```

edit in XCode

```
xed .
```

# Format Staff

```
swift-format format -i -r .
```