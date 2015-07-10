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
    FunctionAsDataTable(res, keep.code)
  } else res
}
