ParsePackage <- function(package, guess.encoding=FALSE) {
  path <- file.path(package, "R")
  files <- dir(path, full.names=TRUE, recursive=TRUE)
  src <- grep("\\.R$", files, ignore.case=TRUE, value=TRUE)
  if (length(src)) {
    do.call(c, lapply(src, ParseFile))
  }
}

ParseFile <- function(filename, guess.encoding=FALSE) {
  encoding <- if (guess.encoding) GuessEncoding(filename) else "unknown"
  parse(filename, keep.source=TRUE, encoding=encoding)
}

ParseText <- function(text) {
  parse(text=text, keep.source=FALSE)
}
