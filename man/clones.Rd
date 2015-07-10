\name{Clones}
\alias{Clones}
\alias{Clones.list}
\alias{Clones.expression}
\alias{Clones.package.code}
\encoding{UTF-8}
\title{Clones}
\description{
  Find clones inside R two expressions.
}

\usage{
  Clones(exprs, ...)
  \method{Clones}{list}(exprs, keep.code=FALSE, ...)
  \method{Clones}{expression}(exprs, keep.code=FALSE, ...)
  \method{Clones}{package.code}(exprs, keep.code=FALSE, ...)
}
\arguments{
  \item{exprs}{A single R expression object or a list of R expression
    objects.}
  \item{keep.code}{Keeps source code reference if TRUE.}
  \item{...}{Further arguments to pass to method implementation.}
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
