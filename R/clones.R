FindClones <- function(exprs, algo="sha1") {
  if (inherits(exprs, "list") && length(exprs) &&
      all(sapply(exprs, inherits, "expression"))) {
    res <- lapply(exprs, FindFunctions, algo)
    hashes <- table(unlist(lapply(res, function(x) x[, unique(hash)])))
    lapply(res, function(x) x[hash %in% names(hashes)[hashes > 1]])
  } else if (inherits(exprs, "expression")) {
    FindFunctions(exprs, algo)[, if (.N > 1) .SD, by="hash"]
  } else stop("exprs must either be an expression or a list of expressions")
}
