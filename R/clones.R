FindClones <- function(exprs, algo="sha1", keep.code=FALSE) {
  if (inherits(exprs, "list") && length(exprs) &&
      all(sapply(exprs, inherits, "expression") |
          sapply(exprs, inherits, "package.code"))) {
    res <- lapply(exprs, function(e) {
      if (inherits(e, "package.code")) FindFunctions(e, algo, TRUE, keep.code)
      else FindFunctions(list(e), algo, TRUE, keep.code)
    }$functions)
    hashes <- table(unlist(lapply(res, function(x) x[, unique(body.hash)])))
    lapply(res, function(x) x[body.hash %in% names(hashes)[hashes > 1]])
  } else if (inherits(exprs, "expression")) {
    res <- FindFunctions(list(exprs), algo, TRUE, keep.code)$functions
    res[, if (.N > 1) .SD, by="body.hash"]
  } else if (inherits(exprs, "package.code")) {
    res <- FindFunctions(exprs, algo, TRUE, keep.code)$functions
    res[, if (.N > 1) .SD, by="body.hash"]
  } else stop("exprs must either be an expression, a package.code or",
              "a list of expressions or package.code")
}
