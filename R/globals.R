Globals.expression <- function(expr, ...) {
  Function <- function(args.res, ref, ...) {
    file <- get("filename", attr(ref, "srcfile"))
    func <- data.table(file=file, begin.line=ref[1], begin.col=ref[2],
                       end.line=ref[3], end.col=ref[4])
    list(type=func)
  }
  Assign <- function(name, res, ...) {
    list(type=res$type, values=cbind(data.table(name=as.character(name)), res$type))
  }
  Leaf <- function(...) {
    value <- data.table(file=NA_character_, begin.line=NA_integer_,
                        begin.col=NA_integer_, end.line=NA_integer_,
                        end.col=NA_integer_)
    list(type=value)
  }
  Call <- function(name, res, assign.name, ...) {
    if ((name == "(" || name == "{") && length(res) > 0) {
      list(type=res[[length(res)]]$type,
           values=rbindlist(lapply(res, function(e) e$values)))
    } else Leaf()
  }
  res <- lapply(expr, VisitExpression, Function=Function,
                Assign=Assign, Call=Call, Leaf=Leaf, global=TRUE)
  res <- rbindlist(lapply(res, function(e) e$values))
  if (nrow(res)) res[, file := get("filename", attr(expr, "srcfile"))]
  res
}

Globals.package.code <- function(expr, ...) {
  rbindlist(mapply(function(expr, filename) {
    ## print(filename)
    Globals.expression(expr, ...)
  }, expr, names(expr), SIMPLIFY=FALSE))
}

Globals <- function(expr, ...) {
  UseMethod("Globals", expr)
}
