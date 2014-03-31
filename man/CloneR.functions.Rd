\name{FindFunctions}
\alias{FindFunctions}
\alias{FindFunctionsHash}
\encoding{UTF-8}
\title{FindFunctions}
\description{
  Find functions inside R expressions.
}

\usage{
FindFunctions(expr, threshold=0)
FindFunctionsHash(expr, threshold=0, algo="sha1")
}
\arguments{
  \item{expr}{R expression object.}
  \item{threshold}{Minimum number of lines needed for the expression to
    be extracted.}
  \item{algo}{Hash algorithm to use.}
}
\details{
  \code{FindFunctions} returns a list containing description of all
  functions with a minimum of \code{threshold} lines.

  \code{FindFunctionsHash} rather returns a hash table where keys are
  the result of a hash (default sha1) of the function body and values
  are list of function descriptions.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{functions}
\keyword{clones}
\keyword{copy}
\keyword{paste}
\keyword{hash}
