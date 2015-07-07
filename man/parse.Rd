\name{Parse}
\alias{Parse}
\alias{ParsePackage}
\alias{ParseFile}
\alias{ParseText}
\encoding{UTF-8}
\title{Parse}
\description{
  Parse R code from a package, a file or a character vector.
}

\usage{
  ParsePackage(package, guess.encoding=FALSE)
  ParseFile(filename, guess.encoding=FALSE)
  ParseText(text, name=NULL)
}
\arguments{
  \item{package}{Root directory of the source package.}
  \item{guess.encoding}{If TRUE try to guess file encoding. This might
    be useless and will drastically slow down the execution.}
  \item{filename}{Name of the file to parse.}
  \item{text}{Character vector to parse.}
  \item{name}{Name to give "srcfile" attribute of the parsed expression.}
}
\details{
  Returns an expression object representing the package, the file or the
  character vector parsed.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{functions}
\keyword{clones}
\keyword{copy}
\keyword{paste}
\keyword{hash}
\keyword{parse}