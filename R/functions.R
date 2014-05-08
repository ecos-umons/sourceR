FuncDesc <- function(ref, name, args, body, is.global) {
  res <- list(ref=ref, name=name, args=args, body=body, is.global=is.global)
  class(res) <- "FuncDesc"
  res
}

FindFunctionsList <- function(expr, threshold=0) {
  Ffunc <- function(args, body, ref, res, global, assign.name) {
    func <- FuncDesc(ref, assign.name, args, body, global)
    if (MatchThreshold(func$body, threshold)) {
      c(list(func), res)
    } else {
      res
    }
  }
  Fassign <- function(name, value, res, ...) res
  Fcall <- function(name, args, res, ...) do.call(c, res)
  Fleaf <- function(...) list()
  unlist(VisitExpressions(expr, list(Ffunc=Ffunc, Fassign=Fassign,
                                     Fcall=Fcall, Fleaf=Fleaf),
                          global=TRUE),
         recursive=FALSE)
}

FindFunctionsHash <- function(expr, threshold=0, algo="sha1") {
  h <- hash()
  Ffunc <- function(args, body, ref, res, global, assign.name) {
    func <- FuncDesc(ref, assign.name, args, body, global)
    res <- digest(res, algo)
    if (MatchThreshold(func$body, threshold)) {
      h[[res]] <- c(h[[res]], list(func))
    }
    digest(list(args, res), algo)
  }
  Fassign <- function(name, value, res, ...) digest(list(name, res), algo)
  Fcall <- function(name, args, res, ...) digest(list(name, res), algo)
  Fleaf <- function(value, ...) digest(value, algo)
  VisitExpressions(expr, list(Ffunc=Ffunc, Fassign=Fassign,
                              Fcall=Fcall, Fleaf=Fleaf),
                   global=TRUE)
  h
}

FindFunctions <- function(expr, threshold=0, algo="sha1", use.hash=FALSE) {
  if (use.hash) {
    FindFunctionsHash(expr, threshold, algo)
  } else {
    FindFunctionsList(expr, threshold)
  }
}
