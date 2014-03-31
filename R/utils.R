MatchThreshold <- function(expr, threshold) {
  length(strsplit(as.character(as.expression(expr)),
                  "\n")[[1]]) >= threshold
}
