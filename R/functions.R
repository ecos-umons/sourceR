FuncDesc <- function(ref, name, args, body, is.global, size=NA) {
  res <- list(ref=ref, name=name, args=args, body=body, is.global=is.global,
              size=size)
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
    res <- sum(unlist(res)) + 1
    func <- FuncDesc(ref, assign.name, args, body, global, res)
    if (MatchThreshold(func$body, threshold)) {
      key <- digest(body)
      h[[key]] <- c(h[[key]], list(func))
    }
    res
  }
  Fassign <- function(name, res, ...) res + 1
  Fcall <- function(name, args, res, ...) sum(unlist(res)) + 1
  Fleaf <- function(value, ...) 1
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
