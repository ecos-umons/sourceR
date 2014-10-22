MatchThreshold <- function(expr, threshold) {
  length(strsplit(as.character(as.expression(expr)),
                  "\n")[[1]]) >= threshold
}

GuessEncoding <- function(filename) {
  # Guesses the encoding of a file.
  cmd <- sprintf("file --mime-encoding %s", filename)
  strsplit(system(cmd, intern=TRUE), " ")[[1]][2]
}

FunctionsHashToDataTable <- function(hash) {
  rbindlist(unlist(lapply(names(hash), function(h) {
    lapply(hash[[h]], function(f) {
      data.table(hash=h, file=f$file,
                 begin.line=f$begin[1], begin.col=f$begin[2],
                 end.line=f$end[1], end.col=f$end[2],
                 name=as.character(f$name), args=list(f$args), body=f$body,
                 is.global=f$is.global, size=f$size,
                 loc=length(strsplit(f$body, "\\n")[[1]]))
    })
  }), recursive=FALSE))
}
