Clones.list <- function(exprs, keep.code=FALSE) {
  res <- lapply(exprs, function(e) {
    FunctionDefinitions(e, TRUE, keep.code)$functions
  })
  hashes <- table(unlist(lapply(res, function(x) x[, unique(body.hash)])))
  lapply(res, function(x) x[body.hash %in% names(hashes)[hashes > 1]])
}

Clones.expression <- function(exprs, keep.code=FALSE) {
  res <- FunctionDefinitions(exprs, TRUE, keep.code)$functions
  res[, if (.N > 1) .SD, by="body.hash"]
}

Clones.package.code <- function(exprs, keep.code=FALSE) {
  res <- FunctionDefinitions(exprs, TRUE, keep.code)$functions
  res[, if (.N > 1) .SD, by="body.hash"]
}

Clones <- function(exprs, ...) {
  UseMethod("Clones", exprs)
}
