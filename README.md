# IWDS Data files

## data file

* iwds.xml (data file)
* iwds.rnc (rnc file)
* iwds.el (generator)
* ucv.html.tempalte (UCV template)
* nucv.html.template (NUCV template)

## data directories

* fig (figures)
* glyphs (glyph images)
* supercjk (supercjk images)
* ucs2003 (2003 images)
* ucs2012 (2012 images)

## not prepared yet

* mui.xml (mis-unified ideographs)

# How to create the document.

0. You need `identify`, `emacs` and `cask` commands installed.

1. Run the following in console.

```
% cask install
% cask run emacs --script iwds.el
```

Then, `ucv.html` and `nucv.html` will be produced. Convert HTML files to
PDF files by HTML publication tool.
