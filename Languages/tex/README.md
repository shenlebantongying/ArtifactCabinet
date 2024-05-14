# Current No brainer -> latexmk -pdf

- microtype
- mathtools

Citation in a good way -> Biblatex + Biber

Graphics PGF/TikZ > MetaPost > others and probably asymptote

The minted 3.0 unreleased  https://github.com/gpoore/minted?tab=readme-ov-file#development-status

# latexmk no-brainer

https://ctan.org/pkg/latexmk

```sh
latexmk -c # clean
```

`.latexmkrc`


```perl
$pdf_mode = 

1 -> pdflatex
4 -> lualatex
```

# Frequently used packages

- hyperref -\> links
- fancyhdr -\> headers & footer
- fancyref (cleverref?)
- gensymb -\> the degree symbol
- xcolor
- pdfpages -\> to automate combining pdfs
- datatool -\> load csv directly to file
- todonotes \# For fun

# LaTeX3 & expl3

# Math

mathpartir for induction rules

