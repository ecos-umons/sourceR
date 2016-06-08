common.calls <- c("[", "{", "(", "[[", ":", "::", ":::",
                  "if", "for", "while", "return", "$", "-", "*", "+", "/", "!",
                  "==", "!=", "^", "<", "<=", ">", ">=", "&&", "||", "&", "|")

FunctionCalls.expression <- function(expr, ignore.common=TRUE, ...) {
  current.envir <- new.env(parent=emptyenv())
  Function <- function(args, args.res, body.res, ref, global, assign.name, ...) {
    old.envir <- current.envir
    environment(Function)$current.envir <- new.env(parent=old.envir)
    for (name in names(args)) {
      current.envir[[deparse(name)]] <- TRUE
    }
    force(args.res)
    force(body.res)
    environment(Function)$current.envir <- old.envir
    res <- rbind(rbindlist(args.res), body.res)
    if (!is.null(res) && !is.null(res$file) && any(is.na(res$file))) {
      res[is.na(res$file)]$file <- get("filename", attr(ref, "srcfile"))
      res[is.na(res$begin.line)]$begin.line <- ref[1]
      res[is.na(res$begin.col)]$begin.col <- ref[2]
      res[is.na(res$end.line)]$end.line <- ref[3]
      res[is.na(res$end.col)]$end.col <- ref[4]
    }
    res
  }
  Assign <- function(name, res, ...) {
    if (inherits(name, "name")) {
      current.envir[[deparse(name)]] <- TRUE
    }
    res
  }
  CreateTable <- function(name, package, public, args=c()) {
    if (is.null(args)) args <- character(0)
    data.table(name, package, public, args=list(args), file=NA_character_,
               begin.line=NA_integer_, begin.col=NA_integer_,
               end.line=NA_integer_, end.col=NA_integer_)
  }
  Call <- function(name, args, res, ...) {
    this <-
      if (inherits(name, "call")) {
        if (name[[1]] == "::") {
          CreateTable(name=deparse(name[[3]]), package=deparse(name[[2]]),
                      args=names(args), public=TRUE)
        } else if (name[[1]] == ":::") {
          CreateTable(name=deparse(name[[3]]), package=deparse(name[[2]]),
                      args=names(args), public=FALSE)
        }
      } else if (inherits(name, "name") &&
                 !exists(deparse(name), envir=current.envir) &&
                 !(ignore.common && deparse(name) %in% common.calls)) {
        CreateTable(name=deparse(name), package=NA,
                    args=names(args), public=TRUE)
      }
    rbind(this, rbindlist(res))
  }
  Leaf <- function(value, ...) {
    NULL
  }
  rbindlist(lapply(expr, VisitExpression, Function=Function,
                   Assign=Assign, Call=Call, Leaf=Leaf, global=TRUE))
}

FunctionCalls.package.code <- function(expr, ignore.common=TRUE, ...) {
  res <- lapply(expr, FunctionCalls.expression, ignore.common, ...)
  rbindlist(lapply(names(expr), function(f) {
    calls <- res[[f]]
    if (!is.null(calls) && any(is.na(calls$file))) {
      calls[is.na(calls$file), file := f]
    }
    calls
  }))
}

FunctionCalls <- function(expr, ignore.common=TRUE, ...) {
  UseMethod("FunctionCalls", expr)
}
