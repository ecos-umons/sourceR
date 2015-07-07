\name{FindClones}
\alias{FindClones}
\encoding{UTF-8}
\title{FindClones}
\description{
  Find clones inside R two expressions.
}

\usage{
  FindClones(exprs, algo="sha1", keep.code=FALSE)
}
\arguments{
  \item{exprs}{A single R expression object or a list of R expression
    objects.}
  \item{algo}{Hash algorithm to use.}
  \item{keep.code}{Keeps source code reference if TRUE.}
}
\details{
  Returns the Type-1 clones found in all \code{exprs}.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{functions}
\keyword{clones}
\keyword{copy}
\keyword{paste}
\keyword{hash}
\keyword{detection}
