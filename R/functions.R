.AsDataTable <- function(funcs, keep.code=TRUE) {
  Functions <- function(func, keep.code) {
    res <- func[names(func) != "sub.func"]
    if (keep.code) {
      res$code <- do.call(paste, c(as.list(res$code), list(sep="\n")))
    } else {
      res <- res[names(res) != "code"]
    }
    res <- as.data.table(res)
    if (length(func$sub.func)) {
      sub <- lapply(func$sub.func, Functions, keep.code)
      rbind(res, rbindlist(sub))
    } else res
  }
  Inclusions <- function(func) {
    if (length(func$sub.func)) {
      GetSub <- function(name) sapply(func$sub.func, function(f) f[[name]])
      res <- data.table(file=func$file, parent.hash=func$hash,
                        parent.begin.line=func$begin.line,
                        parent.begin.col=func$begin.col,
                        parent.end.line=func$end.line,
                        parent.end.col=func$end.col,
                        sub.hash=GetSub("hash"),
                        sub.begin.line=GetSub("begin.line"),
                        sub.begin.col=GetSub("begin.col"),
                        sub.end.line=GetSub("end.line"),
                        sub.end.col=GetSub("end.col"))
      rbind(res, rbindlist(lapply(func$sub.func, Inclusions)))
    }
  }
  funcs <- unlist(lapply(funcs, function(f) f$sub.func), recursive=FALSE)
  list(functions=rbindlist(lapply(funcs, Functions, keep.code)),
       inclusions=rbindlist(lapply(funcs, Inclusions)))
}

FunctionDefinitions.expression <- function(expr, as.data.table=TRUE,
                                           keep.code=TRUE, ...) {
  Function <- function(args.res, body, body.res, ref, global, assign.name, ...) {
    size <- if (length(args.res)) sum(sapply(args.res, `[[`, "size")) else 0
    size <- size + body.res$size + 1
    hashes <- lapply(args.res, `[[`, "hash")
    hash <- digest(c(list("function", body.res$hash), hashes), algo="sha1")
    file <- get("filename", attr(ref, "srcfile"))
    code <- as.character(ref)
    func <- list(hash=hash, body.hash=body.res$hash, name=deparse(assign.name),
                 file=file, begin.line=ref[1], begin.col=ref[2],
                 end.line=ref[3], end.col=ref[4], global=global,
                 size=size, body.size=body.res$size, code=code,
                 loc=length(ref), body.loc=length(body), sub.func=body.res$sub.func)
    list(hash=hash, size=size, sub.func=list(func))
  }
  Assign <- function(name, res, ...) {
    res$size <- res$size + 1
    res$hash <- digest(list("assign", name, res$hash), algo="sha1")
    res
  }
  Call <- function(name, args, res, ...) {
    if ((name == "(" || name == "{") && length(res) == 1) res[[1]]
    else {
      size <- sum(sapply(res, `[[`, "size")) + 1
      hashes <- lapply(res, `[[`, "hash")
      hash <- digest(c(list("call", name), hashes), algo="sha1")
      sub.func <- do.call(c, lapply(res, function(x) x$sub.func))
      if (!is.null(sub.func)) {
        list(size=size, hash=hash, sub.func=sub.func)
      } else list(size=size, hash=hash)
    }
  }
  Leaf <- function(value, ...) {
    list(size=1, hash=digest(value, algo="sha1"))
  }
  res <- lapply(expr, VisitExpression, Function=Function,
                Assign=Assign, Call=Call, Leaf=Leaf, global=TRUE)
  if (as.data.table) {
    .AsDataTable(res, keep.code)
  } else res
}

FunctionDefinitions.package.code <- function(expr, as.data.table=TRUE, ...) {
  res <- lapply(expr, FunctionDefinitions.expression, as.data.table, ...)
  if (as.data.table) {
    list(functions=rbindlist(lapply(res, function(x) x$functions)),
         inclusions=rbindlist(lapply(res, function(x) x$inclusions)))
  } else res
}

FunctionDefinitions <- function(expr, ...) {
  UseMethod("FunctionDefinitions", expr)
}

FunctionCalls.expression <- function(expr, ...) {
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
    if (!is.null(res) && any(is.na(res$file))) {
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
  CreateTable <- function(name, package, public) {
    data.table(name, package, public, file=NA_character_,
               begin.line=NA_integer_, begin.col=NA_integer_,
               end.line=NA_integer_, end.col=NA_integer_)
  }
  Call <- function(name, res, ...) {
    this <-
      if (inherits(name, "call")) {
        if (name[[1]] == "::") {
          CreateTable(name=deparse(name[[3]]),
                      package=deparse(name[[2]]), public=TRUE)
        } else if (name[[1]] == ":::") {
          CreateTable(name=deparse(name[[3]]),
                      package=deparse(name[[2]]), public=FALSE)
        }
      } else if (inherits(name, "name") &&
                 !exists(deparse(name), envir=current.envir)) {
        CreateTable(name=deparse(name), package=NA, public=TRUE)
      }
    this$args <- list(names(args))
    rbind(this, rbindlist(res))
  }
  Leaf <- function(value, ...) {
    NULL
  }
  rbindlist(lapply(expr, VisitExpression, Function=Function,
                   Assign=Assign, Call=Call, Leaf=Leaf, global=TRUE))
}

FunctionCalls.package.code <- function(expr, ...) {
  res <- lapply(expr, FunctionCalls.expression, ...)
  rbindlist(lapply(names(expr), function(f) {
    calls <- res[[f]]
    if (!is.null(calls) && any(is.na(calls$file))) {
      calls[is.na(calls$file), file := f]
    }
    calls
  }))
}

FunctionCalls <- function(expr, ...) {
  UseMethod("FunctionCalls", expr)
}
