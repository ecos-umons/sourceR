\name{FindClones}
\alias{FindClones}
\alias{FindClonesFromHashes}
\encoding{UTF-8}
\title{FindClones}
\description{
  Find clones inside R two expressions.
}

\usage{
  FindClonesFromHashes(hashes)
  FindClones(exprs, threshold=0, algo="sha1")
}
\arguments{
  \item{hashes}{List of }
  \item{exprs}{List of R expression objects.}
  \item{threshold}{Minimum number of lines needed for the expression to
    be extracted.}
  \item{algo}{Hash algorithm to use.}
}
\details{
  \code{FindClonesFromHashes} returns the clones found in all
  \code{hashes}.

  \code{FindClones} returns the clones found in all \code{exprs}.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{functions}
\keyword{clones}
\keyword{copy}
\keyword{paste}
\keyword{hash}
