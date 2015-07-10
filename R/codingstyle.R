CodingStyle.expression <- function(expr) {
  Make <- function(assign.name, global, type) {
    assign.name <- as.character(assign.name)
    if (length(assign.name) == 1 && !is.na(assign.name)) {
      data.table(name=assign.name, global=global, type=type)
    }
  }
  Function <- function(args, body.res, global, assign.name, ...) {
    rbind(Make(assign.name, global, "function"), body.res,
          data.table(name=names(args), global=FALSE, type="args"))
  }
  Assign <- function(name, res, assign.name, global, ...) {
    rbind(Make(assign.name, global, "assign"), res)
  }
  Call <- function(name, args, res, assign.name, global, ...) {
    rbind(Make(assign.name, global, "call"), rbindlist(res))
  }
  Leaf <- function(value, assign.name, global, ...) {
    Make(assign.name, global, class(value))
  }
  rbindlist(lapply(expr, VisitExpression, Function=Function,
                   Assign=Assign, Call=Call, Leaf=Leaf, global=TRUE))
}

CodingStyle.package.code <- function(expr) {
  lapply(expr, CodingStyle.expression)
}

CodingStyle <- function(expr, ...) {
  UseMethod("CodingStyle", expr)
}
