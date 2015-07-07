\name{FindFunctions}
\alias{FindFunctions}
\encoding{UTF-8}
\title{FindFunctions}
\description{
  Find functions inside R expressions.
}

\usage{
FindFunctions(expr, algo="sha1", as.data.table=TRUE, keep.code=TRUE)
}
\arguments{
  \item{expr}{R expression object.}
  \item{algo}{Hash algorithm to use.}
  \item{as.data.table}{Converts result as a data.table if TRUE.}
  \item{keep.code}{Keeps source code reference if TRUE.}
}
\details{
  \code{FindFunctions} returns a \code{data.table} containing the
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
