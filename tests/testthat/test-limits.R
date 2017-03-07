library(testthat)
context("limits")

test_that("limit errors are correct", {
  superheat(iris[, -5], yt = -(1:4), yt.lim = c(-10, 0), yt.plot.type = "bar")
  superheat(iris[, -5], yt = 1:4, yt.lim = c(0, 10), yt.plot.type = "line")
  superheat(iris[, -5], yr = 1:150, yr.lim = c(0, 200))
  superheat(iris[, -5], yr = 1:150, yr.lim = c(0, 200), yr.plot.type = "bar")
})

