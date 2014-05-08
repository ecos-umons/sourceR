\name{FindFunctions}
\alias{FindFunctions}
\encoding{UTF-8}
\title{FindFunctions}
\description{
  Find functions inside R expressions.
}

\usage{
FindFunctions(expr, threshold=0, algo="sha1", use.hash=FALSE)
}
\arguments{
  \item{expr}{R expression object.}
  \item{threshold}{Minimum number of lines needed for the expression to
    be extracted.}
  \item{algo}{Hash algorithm to use.}
  \item{use.hash}{If TRUE returns a hash rather than a list.}
}
\details{
  \code{FindFunctions} returns a list or a hash containing the
  description of all functions found in \code{expr} with a minimum of
  \code{threshold} lines.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{functions}
\keyword{clones}
\keyword{copy}
\keyword{paste}
\keyword{hash}
