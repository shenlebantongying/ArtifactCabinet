# Resources

CIS 552: Advanced Programming
https://www.seas.upenn.edu/~cis552/current/resources.html

# Batteries
<https://hackage.haskell.org/packages/top>
+ lens

# Reference

+ prelude (base) library <https://hackage.haskell.org/package/base>

# How to run a haskell program?

Use `ghcup` -> For Everything Haskell related

Install packages within `stack`

`stack runghc ./nice.hs` -> run single file

## Setup VS code's code-runner
```json
 "code-runner.executorMapByFileExtension": {
        ".hs": "stack runhaskell",
        }
```

## Embed comment in vscode
https://github.com/haskell/haskell-language-server/blob/master/plugins/hls-eval-plugin/README.md
