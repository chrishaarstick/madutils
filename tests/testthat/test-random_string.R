
# random_string tests -----------------------------------------------------

# R Libraries
library(madutils)
library(testthat)
library(checkmate)

context("random string unit tests")


# Unit Tests --------------------------------------------------------------

test_that("generates random string", {
  
  s1 <- random_string("")
  expect_string(s1)
  expect_gte(nchar(s1), 8)
  
  s2 <- random_string("test")
  expect_true(grepl("test", s2))
})

