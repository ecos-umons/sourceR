\name{CodingStyle}
\alias{CodingStyle}
\alias{CodingStyle.list}
\alias{CodingStyle.expression}
\alias{CodingStyle.package.code}
\encoding{UTF-8}
\title{CodingStyle}
\description{
  Extract assigned names from R code.
}

\usage{
  CodingStyle(expr, ...)
  \method{CodingStyle}{expression}(expr, ...)
  \method{CodingStyle}{package.code}(expr, ...)
}
\arguments{
  \item{expr}{A single R expression or package.code object, or a list of
    R expression orpackage.code objects.}
  \item{...}{Further arguments to pass to method implementation.}
}
\details{
  \code{CodingStyle.expression} returns a \code{data.table} containing
  the name of the variable assigned, whether it is global or not and its
  type.

  \code{CodingStyle.list} and \code{CodingStyle.package.code} apply
  \code{CodingStyle.expression} on list of expressions.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{coding}
\keyword{style}
\keyword{naming}
\keyword{convention}
