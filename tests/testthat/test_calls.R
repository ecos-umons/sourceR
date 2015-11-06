library(sourceR)
context("Calls")

test_that("Calls with common", {
  code <- ParseText("
f1(y, x=1)
f1 <- 2
f1()
f2()
f2 <- function() f1
f2()
package::f2()
package:::f2()
1 + 1")
  calls <- FunctionCalls(code, TRUE)
  expect_equal(calls$name, c("f1", "f2", "f2", "f2"))
  expect_equal(calls$package, c(NA, NA, "package", "package"))
})

test_that("Calls with common", {
  code <- ParseText("
f1(y, x=1)
f1 <- 2
f1()
f2()
f2 <- function() f1
f2()
package::f2()
package:::f2()
1 + 1")
  calls <- FunctionCalls(code)
  expect_equal(calls$name, c("f1", "f2", "f2", "f2"))
  expect_equal(calls$package, c(NA, NA, "package", "package"))
})

test_that("Calls without common", {
  code <- ParseText("
f1(y, x=1)
f1 <- 2
f1()
f2()
f2 <- function() f1
f2()
package::f2()
package:::f2()
1 + 1")
  calls <- FunctionCalls(code, FALSE)
  expect_equal(calls$name, c("f1", "f2", "f2", "::", "f2", ":::", "+"))
  expect_equal(calls$package, c(NA, NA, "package", NA, "package", NA, NA))
})
