library(sourceR)
context("Clones")

test_that("Curly braces with single expressions are ignored", {
  func1 <- ParseText("function(x) if (x < 0) NA")
  func2 <- ParseText("function(x) if (x < 0) { NA }")
  expect_equal(FunctionDefinitions(func1)$functions$hash,
               FunctionDefinitions(func2)$functions$hash)
  clones <- Clones(list(func1, func2))
  expect_equal(length(clones[[1]]$hash), 1)
  expect_equal(length(clones[[2]]$hash), 1)
  expect_equal(clones[[1]]$hash, clones[[2]]$hash)
})

test_that("Not clones", {
  func1 <- ParseText("function(x) if (x < 0) NA")
  func2 <- ParseText("function(x) if (x < 0) { print(x); NA }")
  expect_false(FunctionDefinitions(func1)$functions$hash ==
                 FunctionDefinitions(func2)$functions$hash)
  clones <- Clones(list(func1, func2))
  expect_equal(length(clones[[1]]$hash), 0)
  expect_equal(length(clones[[2]]$hash), 0)
})
