GetRef <- function(expr) {
  ref <- getSrcref(expr)
  file <- attr(ref[[1]], "srcfile")
  first <- ref[[1]][1:2]
  last <- ref[[length(ref)]][3:4]
  srcref(file, c(first, last))
}

VisitExpression <- function(e, FUNC, global=FALSE, assign.name=NA) {
  if (length(e) > 1) {
    name <- e[[1]]
    ref <- GetRef(e)
    if (name == "function") {
      FUNC$Ffunc(args=e[[2]], body=e[[3]], res=VisitExpression(e[[3]], FUNC),
                 ref=ref, global=global, assign.name=assign.name)
    } else if (name == "<-") {
      FUNC$Fassign(name=e[[2]], value=e[[3]],
                   res=VisitExpression(e[[3]], FUNC, global, e[[2]]),
                   ref=ref, global=global, assign.name=assign.name)
    } else {
      args <- as.list(e)[2:length(e)]
      FUNC$Fcall(name=name, args=args, res=lapply(args, VisitExpression, FUNC),
                 ref=ref, global=global, assign.name=assign.name)
    }
  } else {
    FUNC$Fleaf(value=e, ref=ref, global=global, assign.name=assign.name)
  }
}

VisitExpressions <- function(expr, FUNC, global=FALSE, assign.name=NA) {
  lapply(expr, VisitExpression, FUNC, global, assign.name)
}
