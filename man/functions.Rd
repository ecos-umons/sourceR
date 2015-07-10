\name{FunctionDefinitions}
\alias{FunctionDefinitions}
\alias{FunctionDefinitions.expression}
\alias{FunctionDefinitions.package.code}
\encoding{UTF-8}
\title{FunctionDefinitions}
\description{
  Find function definitions inside R expressions.
}

\usage{
FunctionDefinitions(expr, ...)
\method{FunctionDefinitions}{expression}(expr, as.data.table=TRUE,
                                         keep.code=TRUE, ...)
\method{FunctionDefinitions}{package.code}(expr, as.data.table=TRUE, ...)
}
\arguments{
  \item{expr}{R expression object.}
  \item{as.data.table}{Converts result as a data.table if TRUE.}
  \item{keep.code}{If converting results as a data.table,kKeeps source
    code reference if TRUE.}
  \item{...}{Further arguments to pass to method implementation.}
}
\details{
  Returns a list or \code{data.table} containing the
  description of all functions found in \code{expr}.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{functions}
\keyword{clones}
\keyword{copy}
\keyword{paste}
\keyword{hash}
