FunctionAsDataTable <- function(func, keep.code) {
  res <- func[names(func) != "sub.func"]
  if (keep.code) {
    res$code <- do.call(paste, c(as.list(res$code), list(sep="\n")))
  } else {
    res <- res[names(res) != "code"]
  }
  res <- as.data.table(res)
  if (length(func$sub.func)) {
    sub <- lapply(func$sub.func, FunctionAsDataTable, keep.code)
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

FunctionsAsDataTable <- function(funcs, keep.code=TRUE) {
  funcs <- unlist(lapply(funcs, function(f) f$sub.func), recursive=FALSE)
  list(functions=rbindlist(lapply(funcs, FunctionAsDataTable, keep.code)),
       inclusions=rbindlist(lapply(funcs, Inclusions)))
}

FindFunctions <- function(expr, algo="sha1", as.data.table=TRUE,
                          keep.code=TRUE) {
  Function <- function(args.res, body, body.res, ref, global, assign.name, ...) {
    size <- if (length(args.res)) sum(sapply(args.res, `[[`, "size")) else 0
    size <- size + body.res$size + 1
    hashes <- lapply(args.res, `[[`, "hash")
    hash <- digest(c(list("function", body.res$hash), hashes))
    file <- get("filename", attr(ref, "srcfile"))
    code <- as.character(ref)
    func <- list(hash=hash, body.hash=body.res$hash, name=as.character(assign.name),
                 file=file, begin.line=ref[1], begin.col=ref[2],
                 end.line=ref[3], end.col=ref[4], global=global,
                 size=size, body.size=body.res$size, code=code,
                 loc=length(ref), body.loc=length(body), sub.func=body.res$sub.func)
    list(hash=hash, size=size, sub.func=list(func))
  }
  Assign <- function(name, res, ...) {
    res$size <- res$size + 1
    res$hash <- digest(list("assign", name, res$hash))
    res
  }
  Call <- function(name, args, res, ...) {
    if ((name == "(" || name == "{") && length(args) == 1) res[[1]]
    else {
      size <- sum(sapply(res, `[[`, "size")) + 1
      hashes <- lapply(res, `[[`, "hash")
      hash <- digest(c(list("call", name), hashes))
      sub.func <- do.call(c, lapply(res, function(x) x$sub.func))
      if (!is.null(sub.func)) {
        list(size=size, hash=hash, sub.func=sub.func)
      } else list(size=size, hash=hash)
    }
  }
  Leaf <- function(value, ...) {
    list(size=1, hash=digest(value))
  }
  res <- lapply(expr, lapply, VisitExpression, Function=Function,
                Assign=Assign, Call=Call, Leaf=Leaf, global=TRUE)
  if (as.data.table) {
    FunctionsAsDataTable(res, keep.code)
  } else res
}
