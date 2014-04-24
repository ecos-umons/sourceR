FindClonesFromHashes <- function(hashes) {
  clones <- unlist(hashes)
  ids <- lapply(names(hashes), function(x) rep(x, length(hashes[[x]])))
  res <- split(unlist(ids), clones)
  res[sapply(res, length) > 1]
}

FindClones <- function(exprs, threshold=0, algo="sha1") {
  hashes <- lapply(exprs, FindFunctionsHash, threshold, algo)
  if (is.null(names(hashes))) {
    names(hashes) <- 1:length(hashes)
  }
  list(hashes=hashes, clones=FindClonesFromHashes(lapply(hashes, names)))
}
