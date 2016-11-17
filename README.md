[![Build Status](https://travis-ci.org/ecos-umons/sourceR.svg?branch=master)](https://travis-ci.org/ecos-umons/sourceR)

sourceR
=======

Static code analysis for R currently allowing one to analyze code for
clone detection and naming conventions.



Installation
------------

With devtools package:

    devtools::install_github("ecos-umons/extractoR")



Usage
-----

You might need to adjust size of the stack if you want to analyze very
huge number of lines of code. Do this both for R and your OS. This is
known to happen when analyzing code from CRAN *cape* package.

For R it can be done such as:

```R
options(expressions=10000)
```

For GNU/Linux (and possibly other UNIX like OS) it can be done using
ulimit:

```bash
# Current limit
ulimit -s
# Change it to a value sufficient to analyze CRAN cape package
ulimit -s 16384
```
