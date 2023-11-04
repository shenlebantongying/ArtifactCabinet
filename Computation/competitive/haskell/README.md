# Doc

Prelude <https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html>

# Snippets

```haskell
-- print lengths of each liens
main = interact (unlines . map inputLength  . lines)
inputLength input = show $ length input
```