\name{VisitExpressions}
\alias{VisitExpressions}
\encoding{UTF-8}
\title{VisitExpressions}
\description{
  Visit R expression objects.
}

\usage{
VisitExpressions(expr, FUNC, global=FALSE, assign.name=NA)
}
\arguments{
  \item{expr}{Expression to visit.}
  \item{FUNC}{List of functions to apply on AST nodes (see details).}
  \item{global}{TRUE if this expression is at a global level}
  \item{assign.name}{Identifier name assigned to this expression.}
}
\details{
  This function visit an expression by doing a depth-first walk of the
  abstract syntax tree represented by the expression object.

  FUNC is a list of functions which are the followings:

  \code{FUNC$Ffunc} is called on call to \code{function}. It takes as
  arguments \code{args} the list of argument of the function, the
  \code{body} of the function and \code{ref} which is a \code{srcref} of
  the function.

  \code{FUNC$Fassign} is called on assignment. It takes as arguments the
  \code{name} of the assigned identifier and \code{value} the value
  assigned.

  \code{FUNC$Fcall} is called on function calls. It takes as arguments
  the \code{name} of the function called and \code{args} the expressions
  of the passed arguments.

  \code{FUNC$Fleaf} is called on the leafs of the AST (i.e. identifiers
  and simple values like numeric and characters). It takes as arguments
  the \code{value} of the node.

  Additionally the three first function also have an argument
  \code{res} containing the result of the recursive call of
  \code{VisitExpressions} on childs (either function body, assignment
  value or argument expressions).

  Also there are \code{global} and \code{assign.name} which are the same
  as for \code{VisitExpressions}.
}

\author{
  Maëlick Claes <himself@maelick.net>
}

\keyword{visit}
\keyword{code}
\keyword{expression}
