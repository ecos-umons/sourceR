\name{CloneR-package}
\alias{CloneR-package}
\alias{CloneR}
\docType{package}
\title{CRAN packages metadata extractor}
\description{
  CloneR can be used to detect clones (similar chunks of code).
}

\details{
  CloneR consist of functions to visit R expressions objects and find
  functions similar functions in them.
}

\author{
Maëlick Claes <himself@maelick.net>

Maintainer: Maëlick Claes <himself@maelick.net>
}

\keyword{clone}
\keyword{code}
\keyword{copypaste}
\keyword{detection}

% \examples{
% \dontrun{
% library(CloneR)

% FetchAll("data")
% ExtractAll("data")


% dbuser <- "user"
% dbpass <- "password"
% dbname <- "rdata"

% con <- dbConnect(MySQL(), user=dbuser, password=dbpass, dbname=dbname)
% dbClearResult(dbSendQuery(con, "SET NAMES utf8"))
% dbClearResult(dbSendQuery(con, "SET collation_connection=utf8_bin"))
% dbClearResult(dbSendQuery(con, "SET collation_server=utf8_bin"))

% rdata <- LoadRData("data/rds")

% InsertAll(con, rdata)
% ExtractInsertTaskViews(con)
% }
% }