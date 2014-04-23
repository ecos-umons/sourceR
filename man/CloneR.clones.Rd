\name{FindClones}
\alias{FindClones}
\encoding{UTF-8}
\title{FindClones}
\description{
  Find clones inside R two expressions.
}

\usage{
  FindClones(expr1, expr2, threshold=0, algo="sha1")
}
\arguments{
  \item{expr1}{First R expression object.}
  \item{expr2}{Second R expression object.}
  \item{threshold}{Minimum number of lines needed for the expression to
    be extracted.}
  \item{algo}{Hash algorithm to use.}
}
\details{
  Returns the clones in \code{expr1} and \code{expr2}
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{functions}
\keyword{clones}
\keyword{copy}
\keyword{paste}
\keyword{hash}
