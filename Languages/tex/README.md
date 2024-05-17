# Current No brainers

- mathtools
- hyperref

Citation in a good way -> Biblatex + Biber

Graphics PGF/TikZ > MetaPost > others and probably asymptote

## latexmk no-brainer

https://ctan.org/pkg/latexmk

```sh
latexmk -c # clean
```



```perl
# .latexmkrc

$pdf_mode = 

1 -> pdflatex
4 -> lualatex
```

## Fonts

```tex
% See doc, various fontsetup
\usepackage[default]{fontsetup}
% check fontspec for what's under the hood
```

# G

https://authors.acm.org/proceedings/production-information/accepted-latex-packages

- microtype
- fancyref (cleverref?)
- gensymb -> the degree symbol
- xcolor
- pdfpages -> to automate combining pdfs
- todonotes -> For fun

# Math

mathpartir for induction rules

# Future

LuaMetaTex

The minted 3.0 unreleased  https://github.com/gpoore/minted?tab=readme-ov-file#development-status


