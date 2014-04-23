FindClones <- function(expr1, expr2, threshold=0, algo="sha1") {
  res <- list(expr1=FindFunctionsHash(expr1, threshold, algo),
              expr2=FindFunctionsHash(expr2, threshold, algo))
  res$clones <- intersect(names(res$expr1), res$expr2)
  res
}
