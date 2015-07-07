sourceR
=======

Static code analysis for R

You might need to adjust size of the stack if you want to analyze very
huge lines of code. Do this both for R and your OS. This is known to
happen when analyzing code from CRAN *cape* package.

On R it can be done such as:

```R
options(expressions=10000)
```

On GNU/Linux (and possibly other UNIX like OS) it can be done using
ulimit:

```bash
# Current limit
ulimit -s
# Change it
ulimit -s 16384
```
