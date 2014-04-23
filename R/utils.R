MatchThreshold <- function(expr, threshold) {
  length(strsplit(as.character(as.expression(expr)),
                  "\n")[[1]]) >= threshold
}

GuessEncoding <- function(filename) {
  # Guesses the encoding of a file.
  cmd <- sprintf("file --mime-encoding %s", filename)
  strsplit(system(cmd, intern=TRUE), " ")[[1]][2]
}
