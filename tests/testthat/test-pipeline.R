
# Pipeline Unit Tests -----------------------------------------------------

# R Libraries
library(madutils)
library(testthat)
library(checkmate)
library(dplyr)

context("pipeline unit tests")


# Unit Tests --------------------------------------------------------------

test_that("pipeline constructor works as expected", {
  
  p1 <- pipeline()
  expect_class(p1, "pipeline")
  expect_function(p1$expr)
  expect_posixct(p1$created_on)
})


test_that("pipeline execute method works as expected", {
  
  df <- tibble(x = rnorm(10))
  p1 <- pipeline()
  p1.1 <- execute(df, p1)
  
  expect_class(p1.1, "pipeline")
  expect_equal(p1.1$output, df)
})


test_that("pipeline flow method works as expected", {
  
  df <- tibble(x = rnorm(10))
  p1 <- pipeline()
  df.1 <- flow(df, p1)
  
  expect_equal(df.1, df)
})


test_that("pipeline execute method works with multiple expr args", {
  
  df <- tibble(x = rnorm(10))
  p1 <- pipeline(expr = function(x, n){head(x, n)})
  p1.1 <- execute(df, p1, n=5)
  
  expect_data_frame(p1.1$output, nrows = 5)
  expect_equal(p1.1$output, head(df, 5))
})



test_that("pipeline flow method works with multiple expr args", {
  
  df <- tibble(x = rnorm(10))
  p1 <- pipeline(expr = function(x, n){head(x, n)})
  df1.1 <- flow(df, p1, n=5)
  
  expect_data_frame(df1.1, nrows = 5)
  expect_equal(df1.1, head(df, 5))
})