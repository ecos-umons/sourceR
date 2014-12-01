FunctionDescription <- function(hash, ref, name, args, body, is.global, size=NA) {
  file <- get("filename", attr(ref, "srcfile"))
  data.table(hash=hash, file=file,
             begin.line=ref[1], begin.col=ref[2],
             end.line=ref[3], end.col=ref[4],
             name=as.character(name), args=list(args), body=body,
             is.global=is.global, size=size,
             loc=length(strsplit(body, "\\n")[[1]]))
}

FindFunctions <- function(expr, algo="sha1") {
  h <- hash()
  Ffunct <- function(args, body, ref, res, global, assign.name) {
    res <- sum(unlist(res)) + 1
    key <- digest(body)
    func <- FunctionDescription(key, ref, assign.name, args, body, global, res)
    h[[key]] <- c(h[[key]], list(func))
    res
  }
  Fassign <- function(name, res, ...) res + 1
  Fcall <- function(name, args, res, ...) sum(unlist(res)) + 1
  Fleaf <- function(value, ...) 1
  VisitExpressions(expr, list(funct=Ffunct, assign=Fassign,
                              call=Fcall, leaf=Fleaf),
                   global=TRUE)
  rbindlist(unlist(as.list(h), recursive=FALSE))
}
