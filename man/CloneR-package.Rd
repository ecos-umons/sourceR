\name{CloneR-package}
\alias{CloneR-package}
\alias{CloneR}
\docType{package}
\title{CRAN packages metadata extractor}
\description{
  CloneR can be used to detect clones (similar chunks of code).
}

\details{
  CloneR consist of functions to visit R expressions objects and find
  functions similar functions in them.
}

\author{
Maëlick Claes <himself@maelick.net>

Maintainer: Maëlick Claes <himself@maelick.net>
}

\keyword{clone}
\keyword{code}
\keyword{copypaste}
\keyword{detection}

\examples{
code <- "f1 <- function(x) x + 1
f2 <- function(x, y) {
  x + y
}
f3 <- function(x, y) {
  function(x) x + y
}
f4 <- function(x) {
  f <- function(y) y - 1
  x * f(x) / 2
}"
clones <- FindFunctions(parse(text=code, keep.source=TRUE), use.hash=TRUE)
}