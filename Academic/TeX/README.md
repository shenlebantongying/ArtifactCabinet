# To make compile LaTeX fast:

+ LaTeX is wrong. Just don't use.
+ Don't use convient wrappers. Just define commands.
+ Don't draw in the documents. Just draw separately and includes PDF.
+ Splitting.

# Current No brainers

- mathtools
- hyperref
- latexmk
- microtype
- pdfpages -> to automate combining pdfs
- listings -> include source code
- biblatex

Graphics -> TikZ/MetaPost/Asymptote. 

For ref, just define one for each document.

# Fonts

Only OpenType fonts are good. Bundled ones are in `/usr/local/texlive/2025/texmf-dist/fonts`.

`unicode-math` -> <https://github.com/latex3/unicode-math>

```tex
\usepackage{unicode-math}
\setmathfont{texgyrepagella-math.otf}
\setmathfont{LibertinusMath-Regular.otf}
```

# Reference

https://authors.acm.org/proceedings/production-information/accepted-latex-packages

# Math

mathpartir for induction rules

