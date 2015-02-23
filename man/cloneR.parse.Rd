\name{ParsePackage}
\alias{ParsePackage}
\encoding{UTF-8}
\title{ParsePackage}
\description{
  Parse all R code found in the R directory of a package.
}

\usage{
  ParsePackage(package, guess.encoding=FALSE)
}
\arguments{
  \item{package}{Root directory of the source package.}
  \item{guess.encoding}{If TRUE try to guess file encoding. This might
    be useless and will drastically slow down the execution.}
}
\details{
  Returns a list R expression objects for all source files.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{functions}
\keyword{clones}
\keyword{copy}
\keyword{paste}
\keyword{hash}
