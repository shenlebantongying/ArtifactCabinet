# haskell-workbench

# Batteries
<https://hackage.haskell.org/packages/top>
+ lens

# Reference

+ prelude (base) library <https://hackage.haskell.org/package/base>

# Tooling notes

Use `ghcup` -> global and `stack` -> per-project only.

`stack runghc ./nice.hs` -> run single file

ghcup's `ghci` => REPL


Setup VS code's code-runner
```json
 "code-runner.executorMapByFileExtension": {
        ".hs": "stack runhaskell",
        }
```