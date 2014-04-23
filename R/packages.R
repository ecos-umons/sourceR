ParsePackage <- function(package, guess.encoding=FALSE) {
  path <- file.path(package, "R")
  files <- dir(path, full.names=TRUE, recursive=TRUE)
  src <- grep("\\.R$", files, ignore.case=TRUE, value=TRUE)
  if (length(src)) {
    do.call(c, lapply(src, function(filename) {
      encoding <- "unknown"
      if (guess.encoding) encoding <- GuessEncoding(filename)
      parse(filename, keep.source=TRUE, encoding=encoding)
    }))
  }
}
