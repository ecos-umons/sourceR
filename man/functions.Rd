\name{Functions}
\alias{FunctionDefinitions}
\alias{FunctionDefinitions.expression}
\alias{FunctionDefinitions.package.code}
\alias{FunctionCalls}
\alias{FunctionCalls.expression}
\alias{FunctionCalls.package.code}
\alias{Globals}
\alias{Globals.expression}
\alias{Globals.package.code}
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
FunctionCalls(expr, ignore.common=TRUE, ...)
\method{FunctionCalls}{expression}(expr, ignore.common=TRUE, ...)
\method{FunctionCalls}{package.code}(expr, ignore.common=TRUE, ...)
Globals(expr, ...)
\method{Globals}{expression}(expr, ...)
\method{Globals}{package.code}(expr, ...)
}
\arguments{
  \item{expr}{R expression object.}
  \item{as.data.table}{Converts result as a data.table if TRUE.}
  \item{keep.code}{If converting results as a data.table,kKeeps source
    code reference if TRUE.}
  \item{ignore.common}{If TRUE ignores common function calls like
    \code{`(`}, \code{`\{`}, \code{`$`}, \code{if}, \code{`+`}, etc.}
  \item{...}{Further arguments to pass to method implementation.}
}
\details{
  \code{FunctionDefinitions} returns a list or \code{data.table}
  containing the description of all functions found in \code{expr}.
  \code{FunctionCalls} returns a \code{data.table} with all calls found
  in \code{expr} and \code{Globals} a \code{data.table} listing all
  globals variables defined.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{functions}
\keyword{clones}
\keyword{copy}
\keyword{paste}
\keyword{hash}
