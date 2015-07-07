VisitExpression <- function(e, Function, Assign, Call, Leaf,
                            global=FALSE, assign.name=NA) {
  Visit <- function(...) {
    VisitExpression(Function=Function, Assign=Assign, Call=Call, Leaf=Leaf, ...)
  }
  if (length(e) <= 1) {
    Leaf(value=e, global=global, assign.name=assign.name)
  } else if (e[[1]] == "function") {
    args.res <- lapply(names(e[[2]]), function(x) {
      Visit(e[[2]][[x]], assign.name=x)
    })
    Function(args=e[[2]], args.res=args.res, body=e[[3]],
             body.res=Visit(e[[3]]), ref=getSrcref(e[[4]]),
             global=global, assign.name=assign.name)
  } else if (e[[1]] == "<-" || e[[1]] == "=") {
    Assign(name=e[[2]], value=e[[3]], res=Visit(e[[3]], global, e[[2]]),
           global=global, assign.name=assign.name)
  } else {
    Call(name=e[[1]], args=args, res=lapply(as.list(e)[2:length(e)], Visit),
         global=global, assign.name=assign.name)
  }
}
