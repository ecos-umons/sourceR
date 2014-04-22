VisitExpression <- function(e, FUNC, global=FALSE, assign.name=NA) {
  if (length(e) > 1) {
    name <- e[[1]]
    if (name == "function") {
      FUNC$Ffunc(args=e[[2]], body=e[[3]], ref=getSrcref(e[[4]]),
                 res=VisitExpression(e[[3]], FUNC),
                 global=global, assign.name=assign.name)
    } else if (name == "<-") {
      FUNC$Fassign(name=e[[2]], value=e[[3]],
                   res=VisitExpression(e[[3]], FUNC, global, e[[2]]),
                   global=global, assign.name=assign.name)
    } else {
      args <- as.list(e)[2:length(e)]
      if (name == "{" & length(args) == 1) {
      } else {
        FUNC$Fcall(name=name, args=args,
                   res=lapply(args, VisitExpression, FUNC),
                   global=global, assign.name=assign.name)
      }
    }
  } else {
    FUNC$Fleaf(value=e, global=global, assign.name=assign.name)
  }
}

VisitExpressions <- function(expr, FUNC, global=FALSE, assign.name=NA) {
  lapply(expr, VisitExpression, FUNC, global, assign.name)
}
