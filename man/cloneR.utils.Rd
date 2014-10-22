\name{FunctionsHashToDataTable}
\alias{FunctionsHashToDataTable}
\encoding{UTF-8}
\title{FunctionsHashToDataTable}
\description{
  Converts functions hash to a data.table object
}

\usage{
FunctionsHashToDataTable(hash)
}
\arguments{
  \item{hash}{hash object as returned by \code{FindFunctions}.}
}
\details{
  Converts the hash as a data.table containing columns hash, file,
  begin.line, begin.col, end.line, end.col, name, args, body, is.global,
  size, col.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{hash}
\keyword{functions}
\keyword{data}
\keyword{frame}
\keyword{table}